########################################################
## Metodología A: Estimación de precios minoristas         ##
## usando variación del IPC a nivel de subclase             ##
## (forward-fill con producto telescópico de variaciones)   ##
##                                                          ##
## TRES VENTANAS DE ANCLA:                                  ##
##   1) Ancla 1999-01 -> extrapola hasta 2018-03 (solapa con##
##      datos observados: solo gráfica comparativa)         ##
##   2) Ancla 2015-01 -> extrapola hasta 2018-03 (idem)      ##
##   3) Ancla 2018-03 -> extrapola hasta 2024-12 (extrapola- ##
##      ción real, sin observación que comparar)            ##
##                                                          ##
## NO se calculan métricas (MAPE/RMSE/etc.) — solo se        ##
## producen gráficas comparativas para ventanas 1 y 2.       ##
##                                                          ##
## Output:                                                   ##
##   - output/metA_precios_tres_ventanas.rds (panel completo)##
##   - output/tablas/metA_mapping_subclase.xlsx               ##
##   - output/tablas/metA_series_fallidas.xlsx                 ##
##   - output/figuras/ventana1_1999/<ciudad>.pdf  (todas las series, multi-página)
##   - output/figuras/ventana2_2015/<ciudad>.pdf
##   - output/figuras/ventana3_2018/<ciudad>.pdf
########################################################

# -----------------------------
# Packages
# -----------------------------
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(writexl)

# -----------------------------
# Paths (EDITAR AQUÍ — sin 00_config.R)
# -----------------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/review-food-security-paper/0A_InformeFinal/"

in_prices <- file.path(base_dir, "input/precios_unadj_DANE_1999_2018.xlsx")
in_ipc    <- file.path(base_dir, "input/IPC.xls")     # tramo 1
in_ipc2   <- file.path(base_dir, "input/IPC_2.xls")   # tramo 2
in_ipc3   <- file.path(base_dir, "input/IPC_3.xls")   # tramo 3
in_ipc4   <- file.path(base_dir, "input/IPC_4.xls")   # tramo 4
in_corr1  <- file.path(base_dir, "input/correlativa_ipc.xlsx")
in_corr2  <- file.path(base_dir, "input/correlativa_ipc_articulos.xlsx")

out_dir     <- file.path(base_dir, "met_a1_ipc_forward/output")
out_figuras <- file.path(out_dir, "figuras")
out_tablas  <- file.path(out_dir, "tablas")

dir.create(out_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(out_figuras, showWarnings = FALSE, recursive = TRUE)
dir.create(out_tablas,  showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Ciudades a procesar
# -----------------------------
cities_use <- c("CALI", "BOGOTA D.C.", "MEDELLIN")

# -----------------------------
# Definición de las tres ventanas
# -----------------------------
ventanas <- list(
  list(nombre = "ventana1_1999", ancla = as.Date("1999-01-01"), fin = as.Date("2018-03-01"), tipo = "validacion_grafica"),
  list(nombre = "ventana2_2015", ancla = as.Date("2015-01-01"), fin = as.Date("2018-03-01"), tipo = "validacion_grafica"),
  list(nombre = "ventana3_2018", ancla = as.Date("2018-03-01"), fin = as.Date("2024-12-01"), tipo = "extrapolacion_final")
)

# -----------------------------
# Helpers
# -----------------------------
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

strip_accents <- function(x) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")

meses_esp <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

make_date <- function(ano, mes_num) {
  as.Date(sprintf("%04d-%02d-01", as.integer(ano), as.integer(mes_num)))
}

mode1 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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

# Forward-fill con ancla FIJA explícita (no dinámica) y fecha de cierre explícita
forward_fill_fixed_anchor <- function(df_series, df_ipc, fecha_ancla, fecha_fin) {
  
  anchor_row <- df_series %>% filter(fecha == fecha_ancla)
  
  if (nrow(anchor_row) == 0 || is.na(anchor_row$precio_500g[1])) {
    return(list(data = NULL, ok = FALSE, issue = "Sin precio observado en la fecha de ancla"))
  }
  
  anchor_price <- anchor_row$precio_500g[1]
  
  full_dates <- tibble(fecha = seq.Date(from = fecha_ancla, to = fecha_fin, by = "month"))
  
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
  
  if (is.na(out$ipc[1])) {
    return(list(data = out, ok = FALSE, issue = "IPC faltante en la fecha de ancla"))
  }
  
  out$precio_hat[1] <- anchor_price
  
  gap_found <- FALSE
  gap_date  <- as.Date(NA)
  
  if (nrow(out) > 1) {
    for (k in 2:nrow(out)) {
      if (is.na(out$ipc[k]) || is.na(out$ipc[k - 1])) {
        gap_found <- TRUE
        gap_date <- out$fecha[k]
        break
      }
      out$precio_hat[k] <- out$precio_hat[k - 1] * (out$ipc[k] / out$ipc[k - 1])
    }
  }
  
  issue <- if (gap_found) paste0("IPC gap found starting at ", gap_date) else NA_character_
  
  list(data = out, ok = TRUE, issue = issue)
}

# -----------------------------
# 1. Cargar y limpiar datos
# -----------------------------
message("Cargando precios DANE...")
dane <- read_excel(in_prices) %>%
  mutate(
    fecha        = make_date(ano, mes_num),
    cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0")
  ) %>%
  filter(!is.na(fecha))

message("Cargando IPC (4 archivos, misma estructura, distintos tramos de fecha)...")
ipc_raw1 <- read_excel(in_ipc)  %>% clean_names()
ipc_raw2 <- read_excel(in_ipc2) %>% clean_names()
ipc_raw3 <- read_excel(in_ipc3) %>% clean_names()
ipc_raw4 <- read_excel(in_ipc4) %>% clean_names()
ipc_raw  <- bind_rows(ipc_raw1, ipc_raw2, ipc_raw3, ipc_raw4)

ipc <- ipc_raw %>%
  mutate(
    ciudad = strip_accents(ciudad),
    ciudad = case_when(
      ciudad == "CARTAGENA DE INDIAS" ~ "CARTAGENA",
      ciudad == "BOGOTA, D.C."        ~ "BOGOTA D.C.",
      TRUE ~ ciudad
    ),
    cod_subclase = substr(subclase, 1, 8),
    mes_num = match(mes, meses_esp),
    ano     = as.integer(ano),
    ipc     = as.numeric(numero_indice),
    fecha   = make_date(ano, mes_num)
  ) %>%
  select(ciudad, cod_subclase, fecha, ano, mes_num, ipc) %>%
  filter(!is.na(fecha), !is.na(ipc), !is.na(cod_subclase), !is.na(ciudad)) %>%
  arrange(ciudad, cod_subclase, fecha)

message(sprintf("  %d filas IPC | %d subclases | %s a %s",
                nrow(ipc), n_distinct(ipc$cod_subclase), min(ipc$fecha), max(ipc$fecha)))

message("Cargando correlativas...")
corr_subclase <- read_excel(in_corr1) %>%
  fill(subclase, ipc, .direction = "down") %>%
  mutate(cod_subclase = paste0("0", gasto_basico, "00")) %>%
  select(cod_subclase, subclase) %>%
  mutate(subclase = as.character(subclase), cod_subclase = as.character(cod_subclase))

corr_producto <- read_excel(in_corr2) %>%
  rename(codigo_articulo = cod_dane, subclase = cod_ipc) %>%
  mutate(codigo_articulo = as.character(codigo_articulo), subclase = as.character(subclase))

dane_use <- dane %>%
  mutate(nombre_ciudad = strip_accents(nombre_ciudad)) %>%
  filter(nombre_ciudad %in% cities_use) %>%
  arrange(nombre_ciudad, articulo, fecha)

message(sprintf("  %d filas | %d artículos | %d ciudades",
                nrow(dane_use), n_distinct(dane_use$articulo), n_distinct(dane_use$nombre_ciudad)))

# -----------------------------
# 2. Loop principal: ventana x ciudad x artículo
# -----------------------------
series_keys <- dane_use %>%
  distinct(nombre_ciudad, articulo) %>%
  arrange(nombre_ciudad, articulo)

fail_log <- list()
map_log  <- list()
all_series_out <- list()

for (v in seq_along(ventanas)) {
  
  vent <- ventanas[[v]]
  message("\n=== Procesando ", vent$nombre, " | ancla: ", vent$ancla, " -> fin: ", vent$fin, " ===")
  
  vent_dir <- file.path(out_figuras, vent$nombre)
  dir.create(vent_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Acumulador de gráficas por ciudad, para volcarlas a un PDF multi-página al final de la ventana
  plots_by_city <- setNames(vector("list", length(cities_use)), cities_use)
  
  for (i in seq_len(nrow(series_keys))) {
    
    city_name <- series_keys$nombre_ciudad[i]
    food_name <- series_keys$articulo[i]
    
    df_series <- dane_use %>%
      filter(nombre_ciudad == city_name, articulo == food_name) %>%
      arrange(fecha) %>%
      mutate(
        codigo_articulo = as.character(codigo_articulo),
        cod_subclase     = as.character(cod_subclase)
      )
    
    if (nrow(df_series) == 0 || all(is.na(df_series$precio_500g))) {
      fail_log[[length(fail_log) + 1]] <- tibble(
        ventana = vent$nombre, ciudad = city_name, articulo = food_name,
        motivo = "No observed prices in DANE series"
      )
      next
    }
    
    sub_info <- assign_subclase_for_series(df_series, corr_subclase, corr_producto)
    subclase_ipc <- sub_info$subclase
    method_used  <- sub_info$method
    
    if (is.na(subclase_ipc) || length(subclase_ipc) != 1) {
      fail_log[[length(fail_log) + 1]] <- tibble(
        ventana = vent$nombre, ciudad = city_name, articulo = food_name,
        motivo = "No se logró asignar una única clase del IPC"
      )
      next
    }
    
    df_ipc <- build_ipc_series(ipc, city_name, subclase_ipc)
    
    if (nrow(df_ipc) == 0) {
      fail_log[[length(fail_log) + 1]] <- tibble(
        ventana = vent$nombre, ciudad = city_name, articulo = food_name,
        motivo = "No hay serie del IPC en este caso",
        subclase = subclase_ipc, method = method_used
      )
      next
    }
    
    ff <- forward_fill_fixed_anchor(
      df_series %>% select(fecha, precio_500g),
      df_ipc,
      fecha_ancla = vent$ancla,
      fecha_fin   = vent$fin
    )
    
    if (!isTRUE(ff$ok)) {
      fail_log[[length(fail_log) + 1]] <- tibble(
        ventana = vent$nombre, ciudad = city_name, articulo = food_name,
        motivo = ff$issue, subclase = subclase_ipc, method = method_used
      )
      next
    }
    
    map_log[[length(map_log) + 1]] <- tibble(
      ventana = vent$nombre, ciudad = city_name, articulo = food_name,
      subclase_ipc = subclase_ipc, method = method_used,
      issue = ff$issue
    )
    
    df_out <- ff$data %>%
      mutate(
        ventana = vent$nombre,
        ciudad = city_name,
        articulo = food_name,
        subclase_ipc = subclase_ipc,
        method = method_used
      ) %>%
      select(ventana, ciudad, articulo, subclase_ipc, method, fecha,
             precio_obs, precio_hat, ipc, status)
    
    all_series_out[[length(all_series_out) + 1]] <- df_out
    
    # ---- Figura: observado vs. estimado ----
    # Para ventana 1 y 2 (validación gráfica): graficar ambas líneas superpuestas
    # Para ventana 3 (extrapolación final): el observado solo existe en el primer punto (ancla)
    p <- ggplot(df_out, aes(x = fecha)) +
      geom_line(aes(y = precio_hat, color = "Precio estimado"),
                linewidth = 0.4, linetype = "dashed") +
      geom_line(
        data = df_out %>% filter(!is.na(precio_obs)),
        aes(y = precio_obs, color = "Precio observado"),
        linewidth = 0.4
      ) +
      scale_color_manual(values = c("Precio observado" = "black", "Precio estimado" = "red")) +
      labs(
        title = paste0("Metodología A — ", vent$nombre),
        subtitle = paste0(city_name, " — ", food_name,
                          " | Subclase IPC: ", subclase_ipc, " (", method_used, ")",
                          if (!is.na(ff$issue)) paste0(" | ", ff$issue) else ""),
        x = NULL, y = "Precio (500g)", color = ""
      ) +
      theme_bw(base_size = 10) +
      theme(legend.position = "bottom")
    
    plots_by_city[[city_name]][[length(plots_by_city[[city_name]]) + 1]] <- p
  }
  
  # ---- Volcar las gráficas acumuladas de esta ventana a un PDF por ciudad ----
  for (cn in names(plots_by_city)) {
    if (length(plots_by_city[[cn]]) == 0) next
    pdf_path <- file.path(vent_dir, paste0(safe_name(cn), ".pdf"))
    pdf(pdf_path, width = 10, height = 4, onefile = TRUE)
    for (pl in plots_by_city[[cn]]) print(pl)
    dev.off()
    message("  PDF guardado: ", pdf_path, " (", length(plots_by_city[[cn]]), " páginas)")
  }
}

# -----------------------------
# 3. Unir outputs y guardar
# -----------------------------
prices_all_windows <- if (length(all_series_out) == 0) tibble() else bind_rows(all_series_out)
mapping_table       <- if (length(map_log) == 0) tibble() else bind_rows(map_log)
fail_df             <- if (length(fail_log) == 0) tibble() else bind_rows(fail_log)

saveRDS(prices_all_windows, file.path(out_dir, "metA_precios_tres_ventanas.rds"))

write_xlsx(
  list(mapping_table = mapping_table),
  file.path(out_tablas, "metA_mapping_subclase.xlsx")
)

write_xlsx(
  list(series_fallidas = fail_df),
  file.path(out_tablas, "metA_series_fallidas.xlsx")
)

message("\nListo. Outputs en: ", out_dir)
message(sprintf("  %d combinaciones ventana-ciudad-articulo procesadas exitosamente", length(all_series_out)))
message(sprintf("  %d combinaciones fallidas", nrow(fail_df)))