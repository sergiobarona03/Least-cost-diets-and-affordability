# ------------------ Paquetes ------------------
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(janitor)
library(rlang)
library(forecast)   # auto.arima / ets
# install.packages("Matrix")
library(Matrix)

# ------------------ Rutas ------------------
path_input   <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\estimadores-banrep\\CALI\\hyndman\\input_hyndman.xlsx"
sheet_input  <- "Sheet1"

path_pca     <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\estimadores-banrep\\CALI\\hyndman\\resultados_pca_h.xlsx"
sheet_res    <- "Resumen"
sheet_det    <- "Detalle_Contribuciones"

# Salida
path_out     <- "pronosticos_reconciliados.xlsx"

# Horizonte de pronóstico (meses)
h <- 72

# ------------------ 1) Cargar datos ------------------
raw <- read_excel(path_input, sheet = sheet_input) %>%
  janitor::clean_names()

pca_res <- read_excel(path_pca, sheet = sheet_res) %>%
  janitor::clean_names() %>%
  mutate(subclase = as.integer(subclase))

pca_det <- read_excel(path_pca, sheet = sheet_det) %>%
  janitor::clean_names() %>%
  mutate(subclase = as.integer(subclase),
         producto = as.character(producto))

stopifnot(all(c("fecha","cod_subclase","articulo") %in% names(raw)))

df <- raw %>%
  mutate(
    fecha        = as.Date(fecha),
    year         = year(fecha),
    month        = month(fecha),
    cod_subclase = as.integer(cod_subclase),
    articulo     = as.character(articulo)
  ) %>%
  arrange(fecha, cod_subclase, articulo)

# Variable a pronosticar
var_y <- if ("precio_500g" %in% names(df)) "precio_500g" else "ipc"

# ------------------ 2) Pesos de artículos dentro de subclase (PCA) ------------------
# Usamos contribución de PC1 como peso relativo; normalizamos por subclase.
w_art <- pca_det %>%
  transmute(subclase,
            articulo = producto,
            w_pc1 = as.numeric(contribucion_pc1)) %>%
  group_by(subclase) %>%
  mutate(w_articulo = ifelse(sum(w_pc1, na.rm = TRUE) > 0,
                             w_pc1 / sum(w_pc1, na.rm = TRUE), 0)) %>%
  ungroup() %>%
  select(subclase, articulo, w_articulo)

# ------------------ 3) Preparar series (mensuales) ------------------
idx_mensual <- df %>%
  summarize(min_fecha = min(floor_date(fecha, "month")),
            max_fecha = max(floor_date(fecha, "month"))) %>%
  tidyr::expand_grid(fecha = seq(min_fecha, max_fecha, by = "month")) %>%
  pull(fecha)

panel <- df %>%
  select(fecha, cod_subclase, articulo, y = all_of(var_y)) %>%
  mutate(fecha = floor_date(fecha, "month")) %>%
  distinct() %>%
  complete(fecha = idx_mensual,
           nesting(cod_subclase, articulo)) %>%
  arrange(cod_subclase, articulo, fecha)

# ------------------ 4) Construir jerarquía y matriz S ------------------
bottom_catalog <- panel %>%
  distinct(cod_subclase, articulo) %>%
  arrange(cod_subclase, articulo) %>%
  mutate(bottom_id = row_number(),
         subclase  = cod_subclase) %>%
  select(subclase, articulo, bottom_id)

m_bottom <- nrow(bottom_catalog)

idx_by_sub <- bottom_catalog %>%
  group_by(subclase) %>%
  summarize(ids = list(bottom_id), .groups = "drop")

subclases <- idx_by_sub$subclase
m_sub     <- length(subclases)

# Matriz S:
# Filas: 1 (Total) + m_sub (Subclases) + m_bottom (Artículos)
# Cols:  m_bottom (artículos)
S <- Matrix(0, nrow = 1 + m_sub + m_bottom, ncol = m_bottom, sparse = TRUE)

# 4.1 Identidad para artículos
S[(1 + m_sub + 1):(1 + m_sub + m_bottom), ] <- Diagonal(m_bottom, 1)

# 4.2 Filas de subclase: pesos PCA por artículo (suman 1 dentro de subclase)
for (i in seq_len(m_sub)) {
  sc  <- subclases[i]
  ids <- idx_by_sub$ids[[i]]
  arti_names <- bottom_catalog$articulo[ids]
  w_sub <- w_art %>%
    filter(subclase == sc, articulo %in% arti_names) %>%
    right_join(tibble(articulo = arti_names), by = "articulo") %>%
    mutate(w_articulo = ifelse(is.na(w_articulo), 1/length(arti_names), w_articulo))
  w_sub$w_articulo <- w_sub$w_articulo / sum(w_sub$w_articulo)  # seguridad
  S[1 + i, ids] <- w_sub$w_articulo
}

# 4.3 Fila TOTAL = suma simple de todos los artículos (sin pesos de subclase)
S[1, ] <- 1

# ------------------ 5) Series por nodo ------------------
Y_bottom_wide <- panel %>%
  left_join(bottom_catalog, by = c("cod_subclase" = "subclase", "articulo")) %>%
  select(fecha, bottom_id, y) %>%
  arrange(fecha, bottom_id) %>%
  pivot_wider(names_from = bottom_id, values_from = y)

Y_bottom_wide <- full_join(
  tibble(fecha = idx_mensual),
  Y_bottom_wide,
  by = "fecha"
) %>% arrange(fecha)

Y_bottom <- Y_bottom_wide %>%
  select(-fecha) %>%
  as.matrix()

# Imputación simple (LOCF; si todo NA, mediana/0)
for (j in seq_len(ncol(Y_bottom))) {
  col <- Y_bottom[, j]
  if (all(is.na(col))) {
    col <- rep(0, length(col))
  } else {
    for (t in seq_along(col)) if (is.na(col[t])) col[t] <- if (t == 1) median(col, na.rm = TRUE) else col[t-1]
  }
  Y_bottom[, j] <- col
}

# Y_all(t) = S %*% Y_bottom(t)
Y_all <- t(apply(Y_bottom, 1, function(x) as.numeric(S %*% x)))

n_nodes <- nrow(S)
stopifnot(ncol(S) == ncol(Y_bottom))

# ------------------ 6) Pronósticos independientes (robusto) ------------------
fit_forecast <- function(x, h) {
  tsx <- ts(x, frequency = 12)
  fit <- tryCatch(forecast::auto.arima(tsx), error = function(e) NULL)
  if (is.null(fit)) fit <- tryCatch(forecast::ets(tsx), error = function(e) NULL)
  if (is.null(fit)) return(rep(NA_real_, h))
  as.numeric(forecast::forecast(fit, h = h)$mean)
}

Yhat_list <- lapply(seq_len(ncol(Y_all)), function(j) fit_forecast(Y_all[, j], h))

# cbind de vectores (cada uno de largo h) -> matriz (h x n_nodes), y luego transponemos
Yhat_mat <- do.call(cbind, Yhat_list)
Yhat_mat <- t(Yhat_mat)  # (n_nodes x h)

stopifnot(nrow(Yhat_mat) == nrow(S), ncol(Yhat_mat) == h)

# ------------------ 7) Reconciliación óptima (OLS) ------------------
SS  <- as.matrix(t(S) %*% S)
SSi <- solve(SS)
P   <- S %*% SSi %*% t(S)
Ytilde_mat <- P %*% Yhat_mat

# ------------------ 8) Empaquetar resultados ------------------
node_labels <- c(
  "TOTAL",
  paste0("SUBCLASE_", subclases),
  paste0("ARTICULO_", bottom_catalog$subclase, "_", bottom_catalog$articulo)
)

df_fore_base <- tibble(
  nodo = rep(node_labels, times = h),
  horizonte = rep(1:h, each = length(node_labels)),
  yhat = as.numeric(Yhat_mat)
)

df_fore_rec <- tibble(
  nodo = rep(node_labels, times = h),
  horizonte = rep(1:h, each = length(node_labels)),
  ytilde = as.numeric(Ytilde_mat)
)

n_total <- 1; n_sub <- m_sub; n_art <- m_bottom

split_level <- function(df, value_col) {
  list(
    Total = df %>% 
      filter(nodo == "TOTAL") %>% 
      select(horizonte, !!value_col),
    
    Subclases = df %>% 
      filter(str_starts(nodo, "SUBCLASE_")) %>% 
      separate(nodo, into = c("lvl","subclase"), sep = "_", extra = "merge") %>% 
      mutate(subclase = as.integer(subclase)) %>% 
      select(horizonte, subclase, !!value_col),
    
    Articulos = df %>% 
      filter(str_starts(nodo, "ARTICULO_")) %>% 
      mutate(tmp = str_remove(nodo, "^ARTICULO_")) %>% 
      separate(tmp, into = c("subclase","articulo"), sep = "_", extra = "merge") %>% 
      mutate(subclase = as.integer(subclase)) %>% 
      select(horizonte, subclase, articulo, !!value_col)
  )
}

base_lvls <- split_level(df_fore_base, rlang::sym("yhat"))
rec_lvls  <- split_level(df_fore_rec,  rlang::sym("ytilde"))

# ------------------ 9) Exportar a Excel ------------------
write_xlsx(
  list(
    "__INFO__" = tibble(
      nota = c(
        "Pronósticos independientes con auto.arima/ets.",
        "Reconciliación óptima OLS: Ytilde = S (S'S)^{-1} S' Yhat.",
        "Fila TOTAL = suma simple de artículos (sin ponderaciones de subclase).",
        "Filas de SUBCLASE reparten artículos con pesos PCA (contribución PC1); si faltan, pesos iguales."
      )
    ),
    "Total_Base"       = base_lvls$Total,
    "Subclase_Base"    = base_lvls$Subclases,
    "Articulo_Base"    = base_lvls$Articulos,
    "Total_Reconc"     = rec_lvls$Total,
    "Subclase_Reconc"  = rec_lvls$Subclases,
    "Articulo_Reconc"  = rec_lvls$Articulos
  ),
  path = path_out
)

message("Listo: escrito ", path_out)

