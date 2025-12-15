########################################################
## Estimación precios minoristas usando variación IPC  ##
## (forward-fill con ratio de índice)                  ##
## Periodo: 1999-01 a 2018-03                           ##
## Train: <= 2015-01 | Valid: >= 2015-02                ##
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
# Paths (EDIT ONLY THIS BLOCK)
# -----------------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"

in_prices <- file.path(base_dir, "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")
in_ipc    <- file.path(base_dir, "var-ipc/IPC.xls")
in_corr1  <- file.path(base_dir, "var-ipc/correlativa_ipc.xlsx")
in_corr2  <- file.path(base_dir, "var-ipc/correlativa_ipc_articulos.xlsx")  # optional refinement

out_dir   <- file.path(base_dir, "working-papers/working-paper-1225/m0/output_ipc_fill")
plot_dir  <- file.path(out_dir, "plots")

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

meses_esp <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

# -----------------------------
# Load data
# -----------------------------
dane_99_18 <- read_excel(in_prices)
var_ipc    <- read_excel(in_ipc) %>% clean_names()

# Clean IPC city names
var_ipc <- var_ipc %>%
  mutate(
    ciudad = case_when(
      ciudad == "CARTAGENA DE INDIAS" ~ "CARTAGENA",
      ciudad == "BOGOTÁ, D.C." ~ "BOGOTÁ D.C.",
      TRUE ~ ciudad
    ),
    cod_subclase = substr(subclase, 1, 8)  # IPC subclase code
  )

# Retail prices: build fecha and cod_subclase proxy from articulo code
dane_99_18 <- dane_99_18 %>%
  mutate(
    fecha = as.Date(sprintf("%04d-%02d-01", ano, mes_num)),
    # your original mapping:
    cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0")
  ) %>%
  filter(fecha < as.Date("2018-04-01"))

# Correlatives
correlativa <- read_excel(in_corr1) %>%
  fill(subclase, ipc, .direction = "down") %>%
  mutate(cod_subclase = paste0("0", gasto_basico, "00")) %>%
  select(cod_subclase, subclase)

corr_producto <- read_excel(in_corr2) %>%
  rename(codigo_articulo = cod_dane, subclase = cod_ipc)

# -----------------------------
# Choose cities (edit as needed)
# -----------------------------
ciudad_input <- c("CALI")  

dane_use <- dane_99_18 %>% filter(nombre_ciudad %in% ciudad_input)

city_i <- sort(unique(dane_use$nombre_ciudad))
food_j <- sort(unique(dane_use$articulo))

# -----------------------------
# Outputs containers
# -----------------------------
fail_list <- list()

resultados_metricas <- tibble(
  ciudad   = character(),
  articulo = character(),
  rmse     = numeric(),
  mape     = numeric(),
  n_valid  = integer()
)

# -----------------------------
# Main loop
# -----------------------------
for (ci in seq_along(city_i)) {
  for (fj in seq_along(food_j)) {
    
    city_name <- city_i[ci]
    food_name <- food_j[fj]
    
    message("Procesando: ", city_name, " - ", food_name)
    
    df_aux <- dane_use %>%
      filter(nombre_ciudad == city_name, articulo == food_name) %>%
      arrange(fecha)
    
    # Need anchor price at 2015-01
    train_df <- df_aux %>% filter(fecha <= as.Date("2015-01-01"))
    anchor   <- train_df %>% filter(fecha == as.Date("2015-01-01"))
    
    # Validation starts AFTER 2015-01 (to avoid duplicating anchor month)
    test_df  <- df_aux %>% filter(fecha >= as.Date("2015-02-01"))
    
    if (nrow(anchor) == 0 || nrow(test_df) == 0) {
      fail_list[[length(fail_list) + 1]] <- tibble(
        ciudad = city_name,
        articulo = food_name,
        motivo = "Sin precio ancla 2015-01 o sin periodo de validación"
      )
      next
    }
    
    # -----------------------------
    # Attach IPC subclase code to the article
    # -----------------------------
    test_df2 <- test_df %>%
      left_join(correlativa, by = "cod_subclase")
    
    # If multiple subclases appear, refine with product-level correlativa
    if (n_distinct(na.omit(test_df2$subclase)) > 1) {
      test_df2 <- test_df %>%
        left_join(corr_producto, by = "codigo_articulo")
    }
    
    subclase_ipc <- unique(na.omit(test_df2$subclase))
    
    if (length(subclase_ipc) != 1) {
      fail_list[[length(fail_list) + 1]] <- tibble(
        ciudad = city_name,
        articulo = food_name,
        motivo = "No se pudo asignar una única subclase IPC"
      )
      next
    }
    
    # Format to IPC filter: in your earlier code you appended "00"
    subclase_ipc_full <- paste0(subclase_ipc, "00")
    subclase_ipc_code <- substr(subclase_ipc_full, 1, 8)
    
    # -----------------------------
    # Build IPC series for that city + subclase
    # -----------------------------
    df_ipc <- var_ipc %>%
      filter(ciudad == city_name, cod_subclase == subclase_ipc_code) %>%
      mutate(
        mes_num = match(mes, meses_esp),
        ano = as.numeric(ano),
        ipc = as.numeric(numero_indice)
      ) %>%
      select(ano, mes_num, ipc) %>%
      arrange(ano, mes_num)
    
    if (nrow(df_ipc) == 0) {
      fail_list[[length(fail_list) + 1]] <- tibble(
        ciudad = city_name,
        articulo = food_name,
        motivo = "No hay IPC para ciudad+subclase"
      )
      next
    }
    
    # -----------------------------
    # Merge: build full timeline from 2015-01 onward
    # include anchor month so we can start recursion
    # -----------------------------
    df_fore <- bind_rows(
      anchor %>% select(fecha, ano, mes_num, nombre_ciudad, articulo, precio_500g),
      test_df %>% select(fecha, ano, mes_num, nombre_ciudad, articulo, precio_500g)
    ) %>%
      left_join(df_ipc, by = c("ano", "mes_num")) %>%
      arrange(fecha) %>%
      mutate(precio_hat = NA_real_)
    
    # Check IPC continuity
    if (any(is.na(df_fore$ipc))) {
      fail_list[[length(fail_list) + 1]] <- tibble(
        ciudad = city_name,
        articulo = food_name,
        motivo = "IPC faltante en la ventana de estimación"
      )
      next
    }
    
    # Anchor prediction at 2015-01
    df_fore$precio_hat[1] <- df_fore$precio_500g[1]
    
    # Recursive forward fill using IPC ratio
    for (k in 2:nrow(df_fore)) {
      df_fore$precio_hat[k] <- df_fore$precio_hat[k - 1] * (df_fore$ipc[k] / df_fore$ipc[k - 1])
    }
    
    # -----------------------------
    # Metrics on validation (2015-02 to 2018-03)
    # -----------------------------
    valid_df <- df_fore %>%
      filter(fecha >= as.Date("2015-02-01")) %>%
      drop_na(precio_500g, precio_hat)
    
    if (nrow(valid_df) < 3) {
      fail_list[[length(fail_list) + 1]] <- tibble(
        ciudad = city_name,
        articulo = food_name,
        motivo = "Muy pocas observaciones para validar"
      )
      next
    }
    
    rmse_val <- sqrt(mean((valid_df$precio_500g - valid_df$precio_hat)^2))
    mape_val <- mean(abs(valid_df$precio_500g - valid_df$precio_hat) / valid_df$precio_500g) * 100
    
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
    
    # -----------------------------
    # Plot
    # -----------------------------
    p <- ggplot(df_fore, aes(x = fecha)) +
      geom_line(aes(y = precio_500g, color = "Precio real"), linewidth = 0.7) +
      geom_line(aes(y = precio_hat, color = "Precio estimado"), linewidth = 0.7, linetype = "dashed") +
      scale_color_manual(values = c("Precio real" = "black", "Precio estimado" = "red")) +
      labs(
        title = "Comparación entre precio real y estimado (IPC subclase)",
        subtitle = paste0(city_name, " - ", food_name,
                          " | RMSE=", round(rmse_val, 2),
                          " | MAPE=", round(mape_val, 2), "%"),
        x = "Fecha", y = "Precio (500g)", color = ""
      ) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    out_plot <- file.path(plot_dir, paste0(safe_name(city_name), "__", safe_name(food_name), ".png"))
    ggsave(out_plot, plot = p, width = 9, height = 5, dpi = 300)
  }
}

# -----------------------------
# Save outputs
# -----------------------------
write_csv(resultados_metricas, file.path(out_dir, "resultados_metricas_ipc.csv"))

fail_df <- if (length(fail_list) == 0) tibble() else bind_rows(fail_list)
write_csv(fail_df, file.path(out_dir, "series_fallidas.csv"))

# Optional: also save to xlsx
write_xlsx(
  list(
    resultados_metricas = resultados_metricas,
    series_fallidas = fail_df
  ),
  file.path(out_dir, "resultados_metricas_ipc.xlsx")
)

message("Listo. Outputs en: ", out_dir)
