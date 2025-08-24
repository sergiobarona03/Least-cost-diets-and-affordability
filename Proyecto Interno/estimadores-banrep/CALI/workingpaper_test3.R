# ==== Directorio de trabajo ====
setwd("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno")

library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(purrr)
library(broom)
library(stringr)   

# ==== Parámetros ====
ciudad_objetivo <- "CALI"
fecha_tope      <- as.Date("2018-03-01")
anio_base       <- 2008

# ==== Normalizar subclase y articulo ====
norm_subclase <- function(x) {
  x <- as.character(x)
  x <- gsub("[^0-9]", "", x)
  str_pad(substr(x, 1, 8), 8, pad = "0")
}
norm_subclase_compat <- function(x) {
  # Compatible con tu mapeo "0 + 6 dígitos + 0" -> 8 dígitos tipo 01510100
  dig <- gsub("[^0-9]", "", as.character(x))
  n <- nchar(dig)
  out <- ifelse(
    n == 6, paste0("0", dig, "0"),
    ifelse(n >= 8, substr(dig, 1, 8),
           ifelse(n == 7, paste0("0", dig),
                  str_pad(dig, 8, side = "right", pad = "0")))
  )
  out
}
norm_articulo <- function(x) {
  x <- as.character(x)
  gsub("[^0-9]", "", x)
}

# ==== Datos correlativa ====
ipc_subclase <- read_excel("var-ipc/XYZ_Correlativa-ENPH-IPC-2008.xlsx") %>%
  clean_names() %>%
  mutate(clase = norm_subclase(clase_9),
         articulo = norm_articulo(articulo_11)) %>%
  select(clase, gasto_basico, articulo, descripcion_ipc)

# ==== Mapeo por subclase ====
mapeo_unico <- ipc_subclase %>%
  mutate(subclase_from_art = norm_subclase(paste0("0", substr(articulo, 1, 6), "0")),
         match_flag = (clase == subclase_from_art)) %>%
  group_by(articulo) %>%
  slice(if (any(match_flag, na.rm = TRUE)) which.max(match_flag) else 1L) %>%
  ungroup() %>%
  transmute(articulo = norm_articulo(articulo),
            subclase = norm_subclase(if_else(!is.na(clase) & clase == subclase_from_art,
                                             clase, subclase_from_art)),
            descripcion_ipc) %>%
  distinct(articulo, .keep_all = TRUE)

# ==== Precios DANE ====
precios <- read_excel("Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx") %>%
  clean_names() %>%
  transmute(
    anio       = as.integer(ano),
    mes        = as.integer(mes_num),
    ciudad     = as.character(nombre_ciudad),
    articulo   = norm_articulo(codigo_articulo),
    nombre_art = as.character(articulo),
    precio     = as.numeric(precio_500g),
    fecha      = as.Date(sprintf("%d-%02d-01", ano, mes))
  ) %>%
  filter(ciudad == ciudad_objetivo, fecha <= fecha_tope)

# ==== IPC ====  (cambio: usar norm_subclase_compat)
var_ipc <- read_excel("var-ipc/IPC.xls") %>%
  clean_names() %>%
  mutate(
    ciudad = recode(ciudad,
                    "CARTAGENA DE INDIAS" = "CARTAGENA",
                    "BOGOTÁ, D.C."        = "BOGOTÁ D.C."),
    mes_num = recode(mes,
                     "Ene"=1,"Feb"=2,"Mar"=3,"Abr"=4,"May"=5,"Jun"=6,
                     "Jul"=7,"Ago"=8,"Sep"=9,"Oct"=10,"Nov"=11,"Dic"=12),
    fecha    = as.Date(sprintf("%d-%02d-01", as.integer(ano), mes_num)),
    subclase = norm_subclase_compat(subclase)   # <- aquí el ajuste clave
  ) %>%
  filter(ciudad == ciudad_objetivo, fecha <= fecha_tope) %>%
  transmute(fecha, subclase, lnIPC = log(as.numeric(numero_indice))) %>%
  distinct(fecha, subclase, .keep_all = TRUE)

# ==== Unir precios con la correlativa ====
df <- precios %>%
  mutate(articulo = norm_articulo(articulo)) %>%
  left_join(mapeo_unico, by = "articulo")

# ==== Definir P0  ====
p0_tabla <- df %>%
  filter(anio == anio_base) %>%
  group_by(articulo) %>%
  summarise(p0 = mean(precio, na.rm = TRUE), .groups = "drop")

fallback_p0 <- df %>%
  arrange(articulo, fecha) %>%
  group_by(articulo) %>%
  summarise(p0_fb = first(na.omit(precio)), .groups = "drop")

p0_tabla <- full_join(p0_tabla, fallback_p0, by = "articulo") %>%
  mutate(p0 = if_else(is.na(p0), p0_fb, p0)) %>%
  select(articulo, p0)

# ==== Construir panel con ln_rel y lnIPC ====
panel <- df %>%
  mutate(subclase = norm_subclase(subclase)) %>%  # ya viene como 8 dígitos compatibles
  filter(!is.na(subclase) & subclase != "") %>%
  inner_join(var_ipc, by = c("fecha","subclase")) %>%
  left_join(p0_tabla, by = "articulo") %>%
  filter(!is.na(precio), !is.na(p0)) %>%
  mutate(ln_rel = log(precio) - log(p0)) %>%
  select(subclase, fecha, articulo, lnIPC, ln_rel)

# =========================
# === MCO IPC (robusto) ===
# =========================

safe_lm_dyn <- purrr::possibly(function(d) {
  d_wide <- tidyr::pivot_wider(
    d,
    names_from  = articulo,
    values_from = ln_rel,
    names_prefix = "ln_rel_"
  )
  xvars <- grep("^ln_rel_", names(d_wide), value = TRUE)
  if (length(xvars) == 0) return(tibble())
  
  na_all <- vapply(d_wide[xvars], function(v) all(is.na(v)), logical(1))
  xkeep  <- xvars[!na_all]
  if (length(xkeep) == 0) return(tibble())
  
  fit <- lm(lnIPC ~ ., data = d_wide[, c("lnIPC", xkeep), drop = FALSE],
            na.action = na.exclude)
  
  broom::tidy(fit) %>%
    filter(term != "(Intercept)", !is.na(estimate)) %>%
    mutate(term = as.character(term))
}, otherwise = tibble())

res_coef <- panel %>%
  group_by(subclase) %>%
  group_modify(~ safe_lm_dyn(.x)) %>%
  ungroup() %>%
  mutate(alimento = sub("^ln_rel_", "", term)) %>%
  select(subclase, alimento, estimate, std.error, statistic, p.value)

safe_glance_dyn <- purrr::possibly(function(d) {
  d_wide <- tidyr::pivot_wider(
    d,
    names_from  = articulo,
    values_from = ln_rel,
    names_prefix = "ln_rel_"
  )
  xvars <- grep("^ln_rel_", names(d_wide), value = TRUE)
  if (length(xvars) == 0) return(tibble())
  
  na_all <- vapply(d_wide[xvars], function(v) all(is.na(v)), logical(1))
  xkeep  <- xvars[!na_all]
  if (length(xkeep) == 0) return(tibble())
  
  fit <- lm(lnIPC ~ ., data = d_wide[, c("lnIPC", xkeep), drop = FALSE],
            na.action = na.exclude)
  broom::glance(fit) %>% mutate(n_regresores = length(xkeep))
}, otherwise = tibble())

res_modelo <- panel %>%
  group_by(subclase) %>%
  group_modify(~ safe_glance_dyn(.x)) %>%
  ungroup()

# =========================
# === Mapa de inclusión ===
# =========================

safe_inclusion_map <- purrr::possibly(function(d) {
  d_wide <- tidyr::pivot_wider(
    d,
    names_from  = articulo,
    values_from = ln_rel,
    names_prefix = "ln_rel_"
  )
  xvars <- grep("^ln_rel_", names(d_wide), value = TRUE)
  if (length(xvars) == 0) return(tibble(alimento = character(), estado = character()))
  
  na_all <- vapply(d_wide[xvars], function(v) all(is.na(v)), logical(1))
  xkeep  <- xvars[!na_all]
  xdrop_allna <- xvars[na_all]
  
  if (length(xkeep) == 0) {
    return(tibble(
      alimento = sub("^ln_rel_", "", xdrop_allna),
      estado   = "excluido_sin_datos_validos"
    ))
  }
  
  fit <- lm(lnIPC ~ ., data = d_wide[, c("lnIPC", xkeep), drop = FALSE],
            na.action = na.exclude)
  
  coef_names <- setdiff(names(coef(fit)), "(Intercept)")
  x_in    <- intersect(xkeep, coef_names)
  x_alias <- setdiff(xkeep, x_in)
  
  tibble(
    alimento = sub("^ln_rel_", "", c(x_in, x_alias, xdrop_allna)),
    estado   = c(rep("incluido", length(x_in)),
                 rep("excluido_por_aliasing", length(x_alias)),
                 rep("excluido_sin_datos_validos", length(xdrop_allna)))
  )
}, otherwise = tibble(alimento = character(), estado = character()))

mapa_inclusion <- panel %>%
  group_by(subclase) %>%
  group_modify(~ safe_inclusion_map(.x)) %>%
  ungroup()

excluidos_por_subclase <- mapa_inclusion %>%
  filter(estado != "incluido") %>%
  arrange(subclase, estado, alimento)

incluidos_por_subclase <- mapa_inclusion %>%
  filter(estado == "incluido") %>%
  arrange(subclase, alimento)

# === Objetos finales ===
# - res_coef, res_modelo, mapa_inclusion,
#   excluidos_por_subclase, incluidos_por_subclase

esperado <- mapeo_unico %>%
  group_by(subclase) %>%
  summarise(total_alimentos = n_distinct(articulo), .groups = "drop")

efectivo <- res_coef %>%
  group_by(subclase) %>%
  summarise(n_alimentos_modelo = n_distinct(alimento), .groups = "drop")

comparacion <- esperado %>%
  left_join(efectivo, by = "subclase") %>%
  mutate(n_alimentos_modelo = ifelse(is.na(n_alimentos_modelo), 0, n_alimentos_modelo),
         diferencia = total_alimentos - n_alimentos_modelo)

