########################################################
## Retail price extension using IPC index ratios
## Full sample base (DANE observed 1999-01..2018-03)
## Forward-fill to latest IPC month available (e.g., 2025)
## Cities: CALI, BOGOTÁ D.C., MEDELLÍN
## NOTE: IPC is built by rbind/bind_rows(IPC.xls, IPC_2.xls)
########################################################

# -----------------------------
# 0. Packages
# -----------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
  library(writexl)
})

# -----------------------------
# 1. Paths (EDIT ONLY THIS BLOCK)
# -----------------------------
base_dir <- "C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"

in_prices <- file.path(base_dir, "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")
in_ipc    <- file.path(base_dir, "var-ipc/IPC.xls")
in_ipc2   <- file.path(base_dir, "var-ipc/IPC_2.xls")
in_corr1  <- file.path(base_dir, "var-ipc/correlativa_ipc.xlsx")
in_corr2  <- file.path(base_dir, "var-ipc/correlativa_ipc_articulos.xlsx")

out_dir   <- file.path(base_dir, "working-papers/working-paper-ipc/output/forecasting_fullsample")
plot_dir  <- file.path(out_dir, "plots")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2. Helpers
# -----------------------------
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

meses_esp <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

make_date <- function(ano, mes_num) as.Date(sprintf("%04d-%02d-01", as.integer(ano), as.integer(mes_num)))

mode1 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# -----------------------------
# 3. Load & clean data
# -----------------------------

# 3.1 DANE prices
dane <- read_excel(in_prices)

dane <- dane %>%
  mutate(
    fecha = make_date(ano, mes_num),
    cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0")
  ) %>%
  filter(!is.na(fecha))

# 3.2 IPC (index levels) — STACK IPC.xls + IPC_2.xls
ipc_raw1 <- read_excel(in_ipc)  %>% clean_names()
ipc_raw2 <- read_excel(in_ipc2) %>% clean_names()

ipc_raw <- bind_rows(ipc_raw1, ipc_raw2)

ipc <- ipc_raw %>%
  mutate(
    ciudad = case_when(
      ciudad == "CARTAGENA DE INDIAS" ~ "CARTAGENA",
      ciudad == "BOGOTÁ, D.C." ~ "BOGOTÁ D.C.",
      TRUE ~ ciudad
    ),
    cod_subclase = substr(subclase, 1, 8),
    mes_num = match(mes, meses_esp),
    ano = suppressWarnings(as.integer(ano)),
    ipc = suppressWarnings(as.numeric(numero_indice)),
    fecha = make_date(ano, mes_num)
  ) %>%
  select(ciudad, cod_subclase, fecha, ano, mes_num, ipc) %>%
  filter(!is.na(fecha), !is.na(ipc), !is.na(cod_subclase), !is.na(ciudad)) %>%
  arrange(ciudad, cod_subclase, fecha) %>%
  # if IPC.xls and IPC_2.xls overlap, keep last row per key
  group_by(ciudad, cod_subclase, fecha) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  arrange(ciudad, cod_subclase, fecha)

# 3.3 Correlativas
corr_subclase <- read_excel(in_corr1) %>%
  fill(subclase, ipc, .direction = "down") %>%
  mutate(cod_subclase = paste0("0", gasto_basico, "00")) %>%
  select(cod_subclase, subclase) %>%
  mutate(
    subclase = as.character(subclase),
    cod_subclase = as.character(cod_subclase)
  )

corr_producto <- read_excel(in_corr2) %>%
  rename(codigo_articulo = cod_dane, subclase = cod_ipc) %>%
  mutate(
    codigo_articulo = as.character(codigo_articulo),
    subclase = as.character(subclase)
  )

# -----------------------------
# 4. Settings: Cities and horizon
# -----------------------------
cities_use <- c("CALI", "BOGOTÁ D.C.", "MEDELLÍN")

dane_use <- dane %>%
  filter(nombre_ciudad %in% cities_use) %>%
  arrange(nombre_ciudad, articulo, fecha)

ipc_last_date_overall <- max(ipc$fecha, na.rm = TRUE)
message("IPC last date in stacked file: ", ipc_last_date_overall)

# -----------------------------
# 5. Core functions
# -----------------------------
assign_subclase_for_series <- function(df_series, corr_subclase, corr_producto) {
  tmp1 <- df_series %>%
    left_join(corr_subclase, by = "cod_subclase") %>%
    mutate(subclase = as.character(subclase))
  
  sub1 <- mode1(tmp1$subclase)
  
  if (is.na(sub1) || length(unique(na.omit(tmp1$subclase))) > 1) {
    tmp2 <- df_series %>%
      left_join(corr_producto, by = "codigo_articulo") %>%
      mutate(subclase = as.character(subclase))
    sub2 <- mode1(tmp2$subclase)
    return(list(subclase = sub2, method = "producto"))
  }
  
  list(subclase = sub1, method = "gasto_basico")
}

build_ipc_series <- function(ipc, city_name, subclase_ipc) {
  subclase_ipc_full <- paste0(subclase_ipc, "00")
  subclase_ipc_code <- substr(subclase_ipc_full, 1, 8)
  
  ipc %>%
    filter(ciudad == city_name, cod_subclase == subclase_ipc_code) %>%
    select(fecha, ipc) %>%
    arrange(fecha)
}

forward_fill_with_ipc <- function(df_series, df_ipc) {
  
  last_obs_date <- max(df_series$fecha[!is.na(df_series$precio_500g)], na.rm = TRUE)
  
  last_obs_price <- df_series %>%
    filter(fecha == last_obs_date) %>%
    slice_tail(n = 1) %>%
    pull(precio_500g)
  
  d0 <- min(df_series$fecha, na.rm = TRUE)
  d1 <- max(df_ipc$fecha, na.rm = TRUE)
  
  full_dates <- tibble(fecha = seq.Date(from = d0, to = d1, by = "month"))
  
  out <- full_dates %>%
    left_join(df_series %>% select(fecha, precio_500g), by = "fecha") %>%
    left_join(df_ipc, by = "fecha") %>%
    arrange(fecha) %>%
    mutate(
      precio_obs = precio_500g,
      precio_hat = NA_real_,
      status = if_else(!is.na(precio_obs), "observed", "imputed")
    ) %>%
    select(fecha, precio_obs, ipc, precio_hat, status)
  
  anchor_idx <- which(out$fecha == last_obs_date)
  
  if (length(anchor_idx) != 1) {
    return(list(data = out, ok = FALSE, issue = "No unique anchor date index"))
  }
  if (is.na(out$ipc[anchor_idx])) {
    return(list(data = out, ok = FALSE, issue = "IPC missing at anchor month"))
  }
  
  out$precio_hat[anchor_idx] <- last_obs_price
  
  gap_found <- FALSE
  gap_date  <- as.Date(NA)
  
  if (anchor_idx < nrow(out)) {
    for (k in (anchor_idx + 1):nrow(out)) {
      if (is.na(out$ipc[k]) || is.na(out$ipc[k - 1])) {
        gap_found <- TRUE
        gap_date <- out$fecha[k]
        break
      }
      out$precio_hat[k] <- out$precio_hat[k - 1] * (out$ipc[k] / out$ipc[k - 1])
    }
  }
  
  out <- out %>%
    mutate(precio_hat = if_else(!is.na(precio_obs), precio_obs, precio_hat))
  
  issue <- if (gap_found) paste0("IPC gap found starting at ", gap_date) else NA_character_
  
  list(data = out, ok = TRUE, issue = issue)
}

# -----------------------------
# 6. Run over all city × article
# -----------------------------
series_keys <- dane_use %>%
  distinct(nombre_ciudad, articulo) %>%
  arrange(nombre_ciudad, articulo)

fail_log <- list()
map_log  <- list()
all_series_out <- list()

for (i in seq_len(nrow(series_keys))) {
  
  city_name <- series_keys$nombre_ciudad[i]
  food_name <- series_keys$articulo[i]
  
  message("Processing: ", city_name, " - ", food_name)
  
  df_series <- dane_use %>%
    filter(nombre_ciudad == city_name, articulo == food_name) %>%
    arrange(fecha) %>%
    mutate(
      codigo_articulo = as.character(codigo_articulo),
      cod_subclase = as.character(cod_subclase)
    )
  
  if (nrow(df_series) == 0 || all(is.na(df_series$precio_500g))) {
    fail_log[[length(fail_log) + 1]] <- tibble(
      ciudad = city_name, articulo = food_name,
      motivo = "No observed prices in DANE series"
    )
    next
  }
  
  sub_info <- assign_subclase_for_series(df_series, corr_subclase, corr_producto)
  subclase_ipc <- sub_info$subclase
  method_used  <- sub_info$method
  
  if (is.na(subclase_ipc) || length(subclase_ipc) != 1) {
    fail_log[[length(fail_log) + 1]] <- tibble(
      ciudad = city_name, articulo = food_name,
      motivo = "Could not assign a single IPC subclase"
    )
    next
  }
  
  df_ipc <- build_ipc_series(ipc, city_name, subclase_ipc)
  
  if (nrow(df_ipc) == 0) {
    fail_log[[length(fail_log) + 1]] <- tibble(
      ciudad = city_name, articulo = food_name,
      motivo = "No IPC series for city+subclase",
      subclase = subclase_ipc, method = method_used
    )
    next
  }
  
  ff <- forward_fill_with_ipc(df_series %>% select(fecha, precio_500g), df_ipc)
  
  if (!isTRUE(ff$ok)) {
    fail_log[[length(fail_log) + 1]] <- tibble(
      ciudad = city_name, articulo = food_name,
      motivo = ff$issue,
      subclase = subclase_ipc, method = method_used
    )
    next
  }
  
  map_log[[length(map_log) + 1]] <- tibble(
    ciudad = city_name,
    articulo = food_name,
    subclase_ipc = subclase_ipc,
    method = method_used,
    ipc_last_date = max(df_ipc$fecha, na.rm = TRUE),
    issue = ff$issue
  )
  
  df_out <- ff$data %>%
    mutate(
      ciudad = city_name,
      articulo = food_name,
      subclase_ipc = subclase_ipc,
      method = method_used,
      precio_final = if_else(!is.na(precio_obs), precio_obs, precio_hat)
    ) %>%
    select(ciudad, articulo, subclase_ipc, method, fecha,
           precio_obs, precio_hat, precio_final, ipc, status)
  
  all_series_out[[length(all_series_out) + 1]] <- df_out
  
  # Plot (saved)
  p <- ggplot(df_out, aes(x = fecha)) +
    geom_line(aes(y = precio_final, color = "Precio final"), linewidth = 0.4) +
    geom_point(
      data = df_out %>% filter(!is.na(precio_obs)),
      aes(y = precio_obs, color = "Observado"),
      size = 0.6, alpha = 0.7
    ) +
    labs(
      title = paste0(city_name, " — ", food_name),
      subtitle = paste0("Subclase IPC: ", subclase_ipc, " (", method_used, ")",
                        if (!is.na(ff$issue)) paste0(" | ", ff$issue) else ""),
      x = NULL, y = "Precio (500g)", color = ""
    ) +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom")
  
  out_png <- file.path(plot_dir, paste0(safe_name(city_name), "__", safe_name(food_name), ".png"))
  ggsave(out_png, p, width = 10, height = 4, dpi = 200)
}

# -----------------------------
# 7. Bind outputs + save
# -----------------------------
prices_extended <- if (length(all_series_out) == 0) tibble() else bind_rows(all_series_out)
mapping_table   <- if (length(map_log) == 0) tibble() else bind_rows(map_log)
fail_df         <- if (length(fail_log) == 0) tibble() else bind_rows(fail_log)

write_csv(prices_extended, file.path(out_dir, "prices_extended_city_article_month.csv"))
write_csv(mapping_table,   file.path(out_dir, "mapping_city_article_to_ipc_subclase.csv"))
write_csv(fail_df,         file.path(out_dir, "failures_log.csv"))

saveRDS(prices_extended, file.path(out_dir, "prices_extended_city_article_month.rds"))
saveRDS(mapping_table,   file.path(out_dir, "mapping_city_article_to_ipc_subclase.rds"))
saveRDS(fail_df,         file.path(out_dir, "failures_log.rds"))

write_xlsx(
  list(
    prices_extended = prices_extended,
    mapping_table   = mapping_table,
    failures_log    = fail_df
  ),
  file.path(out_dir, "ipc_forward_fill_outputs.xlsx")
)

message("DONE. Outputs in: ", out_dir)
message("Rows in extended panel: ", nrow(prices_extended))
message("Unique city-article series produced: ", n_distinct(prices_extended$ciudad, prices_extended$articulo))
message("IPC last date (stacked): ", ipc_last_date_overall)
