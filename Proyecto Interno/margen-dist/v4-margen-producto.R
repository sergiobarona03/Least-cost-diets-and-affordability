##################################################################
## Prueba: análisis sobre margen de comercialización (subclase) ##
## Fecha: 31 de julio de 2025                                   ##
##################################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# Cargar datos (debe crear retail_99_18, whole_18_mean, ipc_sipsa, etc.)
source("margen-dist/v1-join-ipc-sipsa.R")

# -------------------------------------------------
# 1. Datos de precios minoristas (IPC)
# -------------------------------------------------
retail <- retail_99_18 %>%
  rename(
    precio_ipc   = precio_500g,
    articulo_ipc = articulo
  ) %>%
  filter(ciudad == "76") %>%   # Cali
  select(articulo_ipc, codigo_articulo, ano, mes_num, precio_ipc)

# -------------------------------------------------
# 2. Datos de precios mayoristas (SIPSA)
# -------------------------------------------------
wholesale <- whole_18_mean %>%
  rename(
    precio_sipsa   = precio_medio,
    alimento_sipsa = Alimento
  ) %>%
  filter(cod_mun == "76001") %>%
  select(alimento_sipsa, Year, Month, precio_sipsa)

# -------------------------------------------------
# 3. Tabla de mapeo IPC–SIPSA
# -------------------------------------------------
mapa <- ipc_sipsa %>%
  select(
    alimento_sipsa = sipsa,
    articulo_ipc   = retail
  )

# -------------------------------------------------
# 4. Unir las tres fuentes
# -------------------------------------------------
data_merged <- wholesale %>%
  left_join(mapa, by = "alimento_sipsa") %>%
  left_join(
    retail,
    by = c("articulo_ipc", "Year" = "ano", "Month" = "mes_num")
  )

# -------------------------------------------------
# 5. Calcular márgenes de comercialización
# -------------------------------------------------
margenes <- data_merged %>%
  mutate(
    factor = precio_ipc / precio_sipsa,
    margen = (factor - 1) * 100
  ) %>%
  # trimming opcional de márgenes extremos
  filter(
    !is.na(margen),
    margen > 0,
    margen < 300
  )

# -------------------------------------------------
# 6. Función bootstrap para IC (mediana del margen)
# -------------------------------------------------
boot_ci_quantile <- function(x, prob = 0.5, B = 1000) {
  x <- x[is.finite(x)]
  
  # Si hay muy pocos datos, devolvemos NA
  if (length(x) < 2) {
    return(tibble(
      q  = NA_real_,
      lo = NA_real_,
      hi = NA_real_
    ))
  }
  
  # Estadístico bootstrap
  qs <- replicate(B, {
    sample_x <- sample(x, size = length(x), replace = TRUE)
    quantile(sample_x, prob, na.rm = TRUE)
  })
  
  tibble(
    q  = quantile(x, prob, na.rm = TRUE),
    lo = quantile(qs, 0.025, na.rm = TRUE),
    hi = quantile(qs, 0.975, na.rm = TRUE)
  )
}

# -------------------------------------------------
# 7. Mediana del margen + IC por producto
# -------------------------------------------------
margen_median_ci <- margenes %>%
  group_by(alimento_sipsa) %>%
  summarise(
    n_obs  = n(),
    median = list(boot_ci_quantile(margen, prob = 0.50, B = 1000)),
    .groups = "drop"
  ) %>%
  mutate(
    median = map(median, ~ as_tibble(.x))
  ) %>%
  unnest_wider(median) %>%
  # Renombrar columnas para que queden claras
  rename(
    margen_mediana   = q,
    margen_med_lo    = lo,
    margen_med_hi    = hi
  )

# -------------------------------------------------
# 8. Guardar resultados en CSV
# -------------------------------------------------
readr::write_csv(
  margen_median_ci,
  "margen-dist/output-ciudades/CALI/111225_margen_mediana_con_IC.csv"
)
