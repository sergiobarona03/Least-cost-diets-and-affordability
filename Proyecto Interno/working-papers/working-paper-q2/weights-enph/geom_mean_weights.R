############################################################
## 06_ipc_weights_constant_3cities_with_plots.R
## Constant implicit product weights to replicate IPC subclase
## Cities: BOGOTA D.C., CALI, MEDELLIN
## Outputs:
##  - ipc_product_weights.csv
##  - ipc_fitted_indices.csv
##  - failures_log.csv
##  - plots/ipc_fit__<city>__<subclase>.png
##  - plots/ipc_fit__<city>__<subclase>__ALL.png  (combined)
############################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
  library(writexl)
})

# -----------------------------
# 1) Paths (EDIT ONLY THIS BLOCK)
# -----------------------------
base_dir <- "C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"

in_retail <- file.path(base_dir, "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")
in_ipc1   <- file.path(base_dir, "var-ipc/IPC.xls")
in_ipc2   <- file.path(base_dir, "var-ipc/IPC_2.xls")
in_map    <- file.path(base_dir, "var-ipc/correlativa_ipc_articulos.xlsx")

out_dir  <- file.path(base_dir, "working-papers/working-paper-ipc/output/ipc_weights")
plot_dir <- file.path(out_dir, "plots")

dir.create(out_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2) Helpers
# -----------------------------
meses_esp <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

make_date <- function(a, m) as.Date(sprintf("%04d-%02d-01", as.integer(a), as.integer(m)))

norm_city <- function(x) {
  x <- toupper(iconv(as.character(x), from = "", to = "ASCII//TRANSLIT"))
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  x
}

extract_code8 <- function(x) {
  x <- as.character(x)
  x <- gsub("\\D", "", x)
  ifelse(
    nchar(x) >= 8, substr(x, 1, 8),
    ifelse(nchar(x) == 6, paste0(x, "00"), NA_character_)
  )
}

softmax <- function(theta) {
  z <- theta - max(theta)
  w <- exp(z)
  w / sum(w)
}

safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# -----------------------------
# 3) Cities to keep
# -----------------------------
cities_keep <- c("BOGOTA D.C.", "CALI", "MEDELLIN")

# -----------------------------
# 4) Load IPC (bind IPC + IPC_2)
# -----------------------------
ipc <- bind_rows(
  read_excel(in_ipc1) %>% clean_names(),
  read_excel(in_ipc2) %>% clean_names()
) %>%
  mutate(
    ciudad = norm_city(ciudad),
    cod_subclase = extract_code8(subclase),
    mes_num = match(mes, meses_esp),
    ano = suppressWarnings(as.integer(ano)),
    ipc = suppressWarnings(as.numeric(numero_indice)),
    fecha = make_date(ano, mes_num)
  ) %>%
  filter(
    !is.na(ciudad), !is.na(cod_subclase), !is.na(fecha), !is.na(ipc),
    ciudad %in% cities_keep
  ) %>%
  select(ciudad, cod_subclase, fecha, ipc) %>%
  group_by(ciudad, cod_subclase, fecha) %>%
  summarise(ipc = last(ipc), .groups = "drop") %>%
  arrange(ciudad, cod_subclase, fecha)

# -----------------------------
# 5) Load retail prices (DANE)
# -----------------------------
retail <- read_excel(in_retail) %>%
  mutate(
    ciudad = norm_city(nombre_ciudad),
    fecha  = make_date(ano, mes_num),
    codigo_articulo = suppressWarnings(as.numeric(codigo_articulo)),
    articulo = as.character(articulo),
    price = suppressWarnings(as.numeric(precio_500g))
  ) %>%
  filter(
    !is.na(ciudad), !is.na(fecha), !is.na(codigo_articulo),
    !is.na(price), price > 0,
    ciudad %in% cities_keep
  ) %>%
  select(ciudad, fecha, codigo_articulo, articulo, price) %>%
  group_by(ciudad, fecha, codigo_articulo, articulo) %>%
  summarise(price = mean(price), .groups = "drop") %>%
  arrange(ciudad, articulo, fecha)

# -----------------------------
# 6) Load mapping product -> IPC subclase
# -----------------------------
map <- read_excel(in_map) %>%
  rename(codigo_articulo = cod_dane, subclase = cod_ipc) %>%
  mutate(
    codigo_articulo = suppressWarnings(as.numeric(codigo_articulo)),
    cod_subclase = extract_code8(subclase)
  ) %>%
  filter(!is.na(codigo_articulo), !is.na(cod_subclase)) %>%
  select(codigo_articulo, cod_subclase) %>%
  distinct()

# Attach mapping to retail
retail_m <- retail %>%
  left_join(map, by = "codigo_articulo") %>%
  filter(!is.na(cod_subclase))

# -----------------------------
# 7) Keys (city x subclase) with overlap
# -----------------------------
keys <- retail_m %>%
  distinct(ciudad, cod_subclase) %>%
  inner_join(ipc %>% distinct(ciudad, cod_subclase),
             by = c("ciudad", "cod_subclase")) %>%
  arrange(ciudad, cod_subclase)

# -----------------------------
# 8) Estimation loop + plots
# -----------------------------
min_months   <- 12
min_products <- 2

weights_out <- list()
fit_out     <- list()
fail_out    <- list()

for (i in seq_len(nrow(keys))) {
  
  city <- keys$ciudad[i]
  sc   <- keys$cod_subclase[i]
  
  df_p <- retail_m %>% filter(ciudad == city, cod_subclase == sc)
  df_i <- ipc      %>% filter(ciudad == city, cod_subclase == sc)
  
  # Overlap dates: intersect on character -> Date
  dates_chr <- intersect(as.character(df_p$fecha), as.character(df_i$fecha))
  dates <- as.Date(dates_chr)
  
  if (length(dates) < min_months) {
    fail_out[[length(fail_out)+1]] <- tibble(
      ciudad = city, cod_subclase = sc,
      motivo = "Too few overlap months", n_overlap = length(dates)
    )
    next
  }
  
  df_p <- df_p %>% filter(fecha %in% dates)
  df_i <- df_i %>% filter(fecha %in% dates)
  
  wide <- df_p %>%
    select(fecha, articulo, price) %>%
    group_by(fecha, articulo) %>%
    summarise(price = mean(price), .groups = "drop") %>%
    pivot_wider(names_from = articulo, values_from = price) %>%
    left_join(df_i %>% select(fecha, ipc), by = "fecha") %>%
    arrange(fecha)
  
  y <- wide$ipc
  X <- as.matrix(wide %>% select(-fecha, -ipc))
  
  # Drop all-NA product columns
  keep_cols <- which(colSums(!is.na(X)) > 0)
  X <- X[, keep_cols, drop = FALSE]
  
  if (ncol(X) < min_products) {
    fail_out[[length(fail_out)+1]] <- tibble(
      ciudad = city, cod_subclase = sc,
      motivo = "Too few products after pivot", n_products = ncol(X)
    )
    next
  }
  
  Xlog <- suppressWarnings(log(X))
  ylog <- suppressWarnings(log(y))
  
  # Fill NA in Xlog with column mean
  for (j in seq_len(ncol(Xlog))) {
    mu <- mean(Xlog[, j], na.rm = TRUE)
    Xlog[is.na(Xlog[, j]), j] <- mu
  }
  
  ok <- is.finite(ylog)
  Xlog <- Xlog[ok, , drop = FALSE]
  ylog <- ylog[ok]
  fechas_ok <- wide$fecha[ok]
  
  if (length(ylog) < min_months) {
    fail_out[[length(fail_out)+1]] <- tibble(
      ciudad = city, cod_subclase = sc,
      motivo = "Too few months after y-clean", n_months = length(ylog)
    )
    next
  }
  
  obj <- function(theta) {
    w <- softmax(theta)
    xb <- as.vector(Xlog %*% w)
    a  <- mean(ylog - xb)
    sum((ylog - (a + xb))^2)
  }
  
  fit <- tryCatch(optim(rep(0, ncol(Xlog)), obj, method = "BFGS"),
                  error = function(e) NULL)
  
  if (is.null(fit)) {
    fail_out[[length(fail_out)+1]] <- tibble(
      ciudad = city, cod_subclase = sc,
      motivo = "optim failed"
    )
    next
  }
  
  w_hat <- softmax(fit$par)
  xb <- as.vector(Xlog %*% w_hat)
  a  <- mean(ylog - xb)
  
  weights_out[[length(weights_out)+1]] <-
    tibble(ciudad = city, cod_subclase = sc,
           articulo = colnames(Xlog), weight = w_hat) %>%
    arrange(desc(weight))
  
  df_fit <- tibble(
    ciudad = city, cod_subclase = sc,
    fecha = fechas_ok,
    ipc_obs = exp(ylog),
    ipc_fit = exp(a + xb)
  )
  
  fit_out[[length(fit_out)+1]] <- df_fit
  
  # ---- Plot: observed vs fitted IPC ----
  p <- ggplot(df_fit, aes(x = fecha)) +
    geom_line(aes(y = ipc_obs, color = "IPC observado"), linewidth = 0.6) +
    geom_line(aes(y = ipc_fit, color = "IPC ajustado"), linewidth = 0.6, linetype = "dashed") +
    labs(
      title = paste0("IPC observado vs IPC ajustado — ", city),
      subtitle = paste0("Subclase: ", sc, " | Meses: ", nrow(df_fit),
                        " | Productos: ", ncol(Xlog)),
      x = NULL, y = "Índice (nivel)", color = ""
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")
  
  out_png <- file.path(
    plot_dir,
    paste0("ipc_fit__", safe_name(city), "__", sc, ".png")
  )
  ggsave(out_png, p, width = 11, height = 4.5, dpi = 200)
}

weights_df <- if (length(weights_out) == 0) tibble() else bind_rows(weights_out)
fit_df     <- if (length(fit_out) == 0) tibble() else bind_rows(fit_out)
fail_df    <- if (length(fail_out) == 0) tibble() else bind_rows(fail_out)

# -----------------------------
# 9) Combined plot per city (all subclases together)
# -----------------------------
if (nrow(fit_df) > 0) {
  for (city in unique(fit_df$ciudad)) {
    df_city <- fit_df %>% filter(ciudad == city)
    
    p_all <- ggplot(df_city, aes(x = fecha)) +
      geom_line(aes(y = ipc_obs, group = cod_subclase, color = "IPC observado"),
                linewidth = 0.35, alpha = 0.7) +
      geom_line(aes(y = ipc_fit, group = cod_subclase, color = "IPC ajustado"),
                linewidth = 0.35, linetype = "dashed", alpha = 0.7) +
      facet_wrap(~ cod_subclase, scales = "free_y") +
      labs(
        title = paste0("IPC observado vs ajustado — ", city),
        subtitle = "Cada panel es una subclase (escala libre)",
        x = NULL, y = "Índice (nivel)", color = ""
      ) +
      theme_bw(base_size = 9) +
      theme(legend.position = "bottom",
            strip.text = element_text(size = 8))
    
    out_png_all <- file.path(plot_dir, paste0("ipc_fit__", safe_name(city), "__ALL.png"))
    ggsave(out_png_all, p_all, width = 14, height = 9, dpi = 200)
  }
}

# -----------------------------
# 10) Save outputs
# -----------------------------
write_csv(weights_df, file.path(out_dir, "ipc_product_weights.csv"))
write_csv(fit_df,     file.path(out_dir, "ipc_fitted_indices.csv"))
write_csv(fail_df,    file.path(out_dir, "failures_log.csv"))

write_xlsx(
  list(weights = weights_df, fitted_indices = fit_df, failures = fail_df),
  file.path(out_dir, "ipc_weights_outputs.xlsx")
)

message("DONE.")
message("Cities kept: ", paste(cities_keep, collapse = ", "))
message("Weights rows: ", nrow(weights_df))
message("Fitted rows: ", nrow(fit_df))
message("Failures rows: ", nrow(fail_df))
message("Plots saved in: ", plot_dir)
