
########################################################
## Metodología 1: Estimación hacia adelante usando IPC 
## Periodo: 1999-01 a 2018-03
## Train <= 2015-01
## Fuente de datos: DANE - IPC 
########################################################

# Directorio 
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/"

# -----------------------------
# 1. Cargar librerías y definición de directorios
# -----------------------------

# Librerías:
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(writexl)
library(patchwork) 
library(Metrics)

# Datos precios IPC
in_prices <- file.path(base_dir, "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")

# Variación del IPC
in_ipc    <- file.path(base_dir, "var-ipc/IPC.xls")

# Tablas correlativas a nivel de producto y subclase
in_corr1  <- file.path(base_dir, "var-ipc/correlativa_ipc.xlsx")
in_corr2  <- file.path(base_dir, "var-ipc/correlativa_ipc_articulos.xlsx")  

# Aquí se guardan los resultados y los gráficos
out_dir   <- file.path(base_dir, "working-papers/working-paper-1225/m0/output_ipc_fill")
plot_dir  <- file.path(out_dir, "plots_grouped")

# -----------------------------
# 2. Funciones auxiliares
# -----------------------------

# Convertir nombres
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# Vector de meses
meses_esp <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

# Guardar figuras en páginas (para el working paper)
save_plot_pages <- function(plots, out_prefix, plot_dir,
                            ncol = 3, nrow = 2,
                            width = 12, height = 8, dpi = 300,
                            page_title = NULL) {
  if (length(plots) == 0) return(invisible(NULL))
  per_page <- ncol * nrow
  n_pages  <- ceiling(length(plots) / per_page)
  
  for (pg in seq_len(n_pages)) {
    idx <- ((pg - 1) * per_page + 1):min(pg * per_page, length(plots))
    page_plots <- plots[idx]
    
    g <- wrap_plots(page_plots, ncol = ncol, nrow = nrow, guides = "collect") &
      theme(legend.position = "bottom")
    
    if (!is.null(page_title)) {
      g <- g + plot_annotation(title = page_title)
    }
    
    out_file <- file.path(plot_dir, paste0(out_prefix, "__page_", sprintf("%02d", pg), ".png"))
    ggsave(out_file, g, width = width, height = height, dpi = dpi)
  }
}

# -----------------------------
# 3. Cargar los datos
# -----------------------------

# Precios y variación del ipc
dane_99_18 <- read_excel(in_prices) 
var_ipc    <- read_excel(in_ipc) %>% clean_names()

# Limpiar nombres de ciudades y crear el código de subclase
var_ipc <- var_ipc %>%
  mutate(
    ciudad = case_when(
      ciudad == "CARTAGENA DE INDIAS" ~ "CARTAGENA",
      ciudad == "BOGOTÁ, D.C." ~ "BOGOTÁ D.C.",
      TRUE ~ ciudad
    ),
    cod_subclase = substr(subclase, 1, 8) 
  )

# Crear código de subclase (6 primeros # del artículo)
dane_99_18 <- dane_99_18 %>%
  mutate(
    fecha = as.Date(sprintf("%04d-%02d-01", ano, mes_num)),
    cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0")
  ) %>%
  filter(fecha < as.Date("2018-04-01"))

# Cargar tablas correlativas (rellenar hacia abajo)
correlativa <- read_excel(in_corr1) %>%
  fill(subclase, ipc, .direction = "down") %>%
  mutate(cod_subclase = paste0("0", gasto_basico, "00")) %>%
  select(cod_subclase, subclase)

corr_producto <- read_excel(in_corr2) %>%
  rename(codigo_articulo = cod_dane, subclase = cod_ipc)

# Por simplicidad, sólo para Cali:
ciudad_input <- c("CALI")

# Filtro:
dane_use <- dane_99_18 %>% filter(nombre_ciudad %in% ciudad_input)

# Definición de vectores (ciudad y alimentos)
city_i <- sort(unique(dane_use$nombre_ciudad))
food_j <- sort(unique(dane_use$articulo))

# -----------------------------
# 4. Recepción de outputs
# -----------------------------

# Lista de alimentos fallidos
fail_list <- list()

# Resultados de las métricas de validación
resultados_metricas <- tibble(
  ciudad   = character(),
  articulo = character(),
  rmse     = numeric(),
  mape     = numeric(),
  n_valid  = integer()
)

# Lista para guardar las figuras
plots_by_city <- setNames(vector("list", length(city_i)), city_i)

datasets_list <- setNames(vector("list", length(city_i)), city_i)

# -----------------------------
# 5. Bucle principal (estimación hacia adelante)
# -----------------------------

for (ci in seq_along(city_i)) {
  for (fj in seq_along(food_j)) {

    city_name <- city_i[ci]
    food_name <- food_j[fj]
    
    message("Procesando: ", city_name, " - ", food_name)
    
    df_aux <- dane_use %>%
      filter(nombre_ciudad == city_name, articulo == food_name) %>%
      arrange(fecha)
    
    # Datos de entrenamiento
    train_df <- df_aux %>% filter(fecha <= as.Date("2015-01-01"))

    # Datos de validación
    test_df  <- df_aux %>% filter(fecha >= as.Date("2015-02-01"))
    
    # Se valida si existe el dato de corte (2015-01)
    anchor   <- train_df %>% filter(fecha == as.Date("2015-01-01"))
    if (nrow(anchor) == 0 || nrow(test_df) == 0) {
      fail_list[[length(fail_list) + 1]] <- tibble(
        ciudad = city_name,
        articulo = food_name,
        motivo = "Sin precio ancla 2015-01 o sin periodo de validación"
      )
      next
    }
    
    # Recuperar subclase, a partir de la correlativa
    test_df2 <- test_df %>%
      left_join(correlativa, by = "cod_subclase")
    
    # Si hay múltiples subclases asociadas, se refina con la correlativa
    # a nivel de producto
    if (n_distinct(na.omit(test_df2$subclase)) > 1) {
      test_df2 <- test_df %>%
        left_join(corr_producto, by = "codigo_articulo")
    }
    
    subclase_ipc <- unique(na.omit(test_df2$subclase))
    
    # Si no se puede asignar ninguna subclase, se añade a alimentos fallidos
    if (length(subclase_ipc) != 1) {
      fail_list[[length(fail_list) + 1]] <- tibble(
        ciudad = city_name,
        articulo = food_name,
        motivo = "No se pudo asignar una única subclase IPC"
      )
      next
    }
    
    # Ajustar el formato
    subclase_ipc_full <- paste0(subclase_ipc, "00")
    subclase_ipc_code <- substr(subclase_ipc_full, 1, 8)
    
    # Agregar la variación de la subclase (IPC)
    df_ipc <- var_ipc %>%
      filter(ciudad == city_name, cod_subclase == subclase_ipc_code) %>%
      mutate(
        mes_num = match(mes, meses_esp),
        ano = as.numeric(ano),
        ipc = as.numeric(numero_indice)
      ) %>%
      select(ano, mes_num, ipc) %>%
      arrange(ano, mes_num)
    
    # Si no hay IPC, alimento fallido
    if (nrow(df_ipc) == 0) {
      fail_list[[length(fail_list) + 1]] <- tibble(
        ciudad = city_name,
        articulo = food_name,
        motivo = "No hay IPC para ciudad+subclase"
      )
      next
    }
    
    # Merge: precio observado e IPC (precio estimado en NA)
    df_fore <- bind_rows(
      anchor %>% select(fecha, ano, mes_num, 
                        nombre_ciudad, articulo, precio_500g),
      test_df %>% select(fecha, ano, mes_num,
                         nombre_ciudad, articulo, precio_500g)
    ) %>%
      left_join(df_ipc, by = c("ano", "mes_num")) %>%
      arrange(fecha) %>%
      mutate(precio_hat = NA_real_)
    
    # Verificar que la ventana sea continua
    # Si no, el alimento es fallido
    if (any(is.na(df_fore$ipc))) {
      fail_list[[length(fail_list) + 1]] <- tibble(
        ciudad = city_name,
        articulo = food_name,
        motivo = "IPC faltante en la ventana de estimación"
      )
      next
    }
    
    # Predicción para la fecha ancla
    df_fore$precio_hat[1] <- df_fore$precio_500g[1]
    
    # Estimación recursiva hacia adelante
    for (k in 2:nrow(df_fore)) {
      df_fore$precio_hat[k] <- df_fore$precio_hat[k - 1] * (df_fore$ipc[k] / df_fore$ipc[k - 1])
    }
    
    # Cálculo de métricas de validación
    valid_df <- df_fore %>%
      filter(fecha >= as.Date("2015-02-01")) %>%
      drop_na(precio_500g, precio_hat)
    
    # Verificar que sean suficientes observaciones para validar
    # Si no, alimento fallido
    if (nrow(valid_df) < 3) {
      fail_list[[length(fail_list) + 1]] <- tibble(
        ciudad = city_name,
        articulo = food_name,
        motivo = "Muy pocas observaciones para validar"
      )
      next
    }
    
    # RMSE y MAPE
    rmse_val <- rmse(valid_df$precio_500g, valid_df$precio_hat)
    mape_val <- mape(valid_df$precio_500g, valid_df$precio_hat)*100
    resultados_metricas <- bind_rows(
      resultados_metricas,
      tibble(
        ciudad = city_name,
        articulo = food_name,
        rmse = rmse_val,
        mape = mape_val,
        n_valid = nrow(valid_df)
      )
    )
    
    # Gráficos en páginas
    df_plot = rbind(train_df %>% select(fecha, ano, mes_num,
                                 nombre_ciudad, articulo, precio_500g) %>%
                      mutate(ipc = NA,
                             precio_hat = NA), df_fore)
    
    p <- ggplot(df_plot, aes(x = fecha)) +
      geom_line(aes(y = precio_500g, color = "Precio real"),
                linewidth = 0.4) +
      geom_line(aes(y = precio_hat, color = "Precio estimado"),
                linewidth = 0.4, linetype = "dashed") +
      scale_color_manual(values = c("Precio real" = "black", "Precio estimado" = "red")) +
      labs(
        title = food_name,
        subtitle = paste0("RMSE=", round(rmse_val, 2), " | MAPE=", round(mape_val, 2), "%"),
        x = NULL, y = "Precio (500g)", color = ""
      ) +
      theme_bw(base_size = 10) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 9)
      )
    
    plots_by_city[[city_name]] <- append(plots_by_city[[city_name]], list(p))
    datasets_list[[city_name]] <- append(datasets_list[[city_name]], list(df_plot))
  }
}

# -----------------------------
# 6. Guardar outputs
# -----------------------------

# Guardar la base de datos (predicción)
forecast_dataset = do.call(rbind, datasets_list[[1]])

write_csv(forecast_dataset, file.path(out_dir, "m0_forecast_dataset.csv"))
write_csv(forecast_dataset, file.path("working-papers/working-paper-0125/input",
                                      "m0_forecast_dataset.csv"))


# Guardar las figuras en páginas (9 por página)
for (city_name in names(plots_by_city)) {
  save_plot_pages(
    plots      = plots_by_city[[city_name]],
    out_prefix = safe_name(city_name),
    plot_dir   = plot_dir,
    ncol = 3, nrow = 3,       
    width = 12, height = 8,
    dpi = 300,
    page_title = paste0(city_name, " — Precio real vs estimado (IPC subclase)")
  )
}

# Guardar las métricas
write_csv(resultados_metricas, file.path(out_dir, "resultados_metricas_ipc.csv"))

# Guardar los alimentos fallidos
fail_df <- if (length(fail_list) == 0) tibble() else bind_rows(fail_list)
write_csv(fail_df, file.path(out_dir, "series_fallidas.csv"))

# Guardar ambos en .xlsx
write_xlsx(
  list(
    resultados_metricas = resultados_metricas,
    series_fallidas     = fail_df
  ),
  file.path(out_dir, "resultados_metricas_ipc.xlsx")
)
