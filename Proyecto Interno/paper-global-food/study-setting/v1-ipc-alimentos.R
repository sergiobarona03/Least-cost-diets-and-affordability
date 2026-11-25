# =============================================================================
# Aumento acumulado del IPC de alimentos y del IPC total
# Diciembre de 2021 a septiembre de 2025, nacional y por ciudad
# Fuente: DANE, anexo del IPC (exportación de la consulta en línea),
#         un archivo por año: anexo-IPC-AAAA.xlsx
# Salida: un archivo Excel con dos hojas
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(writexl)

carpeta_datos <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\paper-global-food\\study-setting\\data"
ruta_salida   <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\paper-global-food\\study-setting\\output"
dir.create(ruta_salida, showWarnings = FALSE)

anio_ini <- "2021"; mes_ini <- "Dic"
anio_fin <- "2025"; mes_fin <- "Sep"

# -----------------------------------------------------------------------------
# 1. Lectura
#    Un archivo por año, con el nombre anexo-IPC-AAAA.xlsx. En cada archivo,
#    la primera fila de datos trae los nombres reales de las nueve primeras
#    columnas y al final hay filas de pie de página, que se descartan.
# -----------------------------------------------------------------------------
archivos <- list.files(carpeta_datos, pattern = "^anexo-IPC-\\d{4}\\.xlsx$", full.names = TRUE)
print(basename(archivos))
if (length(archivos) == 0) stop("No se encontraron archivos anexo-IPC-AAAA.xlsx en la carpeta.")

leer_anexo <- function(ruta) {
  datos <- read_excel(ruta, col_types = "text")
  if (any(str_detect(datos$Medida, "exceeded"), na.rm = TRUE)) {
    warning("Exportación truncada en ", basename(ruta), ": faltan datos.")
  }
  names(datos)[1:9] <- c("anio", "mes", "nivel_ingresos", "nivel_canasta",
                         "division", "grupo", "clase", "subclase", "cobertura")
  datos %>% filter(str_detect(anio, "^\\d{4}$"))
}

ipc <- lapply(archivos, leer_anexo) %>%
  bind_rows() %>%
  mutate(indice = parse_number(`Número Índice`, locale = locale(decimal_mark = ",")))

# Control: meses disponibles por año
print(ipc %>% count(anio, mes) %>% arrange(anio), n = Inf)

# -----------------------------------------------------------------------------
# 2. Selección: IPC total y división 01 (alimentos y bebidas no alcohólicas)
# -----------------------------------------------------------------------------
seleccion <- ipc %>%
  filter((anio == anio_ini & mes == mes_ini) | (anio == anio_fin & mes == mes_fin)) %>%
  mutate(serie = case_when(
    nivel_canasta == "Total"                                        ~ "ipc_total",
    nivel_canasta == "Division" & str_detect(division, "^01000000") ~ "ipc_alimentos",
    TRUE                                                            ~ NA_character_
  )) %>%
  filter(!is.na(serie)) %>%
  mutate(periodo = if_else(anio == anio_ini, "inicio", "fin")) %>%
  select(cobertura, anio, mes, serie, periodo, indice)

# Control: las dos fechas deben estar, con una sola fila por cobertura,
# serie y periodo
fechas <- distinct(seleccion, periodo)
if (nrow(fechas) < 2) stop("Falta alguna de las dos fechas en los archivos.")

duplicados <- seleccion %>% count(cobertura, serie, periodo) %>% filter(n != 1)
if (nrow(duplicados) > 0) {
  print(duplicados)
  stop("Hay filas duplicadas: revisar los filtros.")
}

# -----------------------------------------------------------------------------
# 3. Aumento acumulado = índice final / índice inicial - 1
# -----------------------------------------------------------------------------
acumulado <- seleccion %>%
  select(cobertura, serie, periodo, indice) %>%
  pivot_wider(names_from = c(serie, periodo), values_from = indice) %>%
  mutate(
    aumento_alimentos_pct = 100 * (ipc_alimentos_fin / ipc_alimentos_inicio - 1),
    aumento_total_pct     = 100 * (ipc_total_fin / ipc_total_inicio - 1),
    precio_relativo_alim  = 100 * ((ipc_alimentos_fin / ipc_alimentos_inicio) /
                                     (ipc_total_fin / ipc_total_inicio) - 1)
  ) %>%
  arrange(cobertura != "NACIONAL", desc(aumento_alimentos_pct))

print(acumulado, n = Inf)

# -----------------------------------------------------------------------------
# 4. Salida
# -----------------------------------------------------------------------------
write_xlsx(
  list(acumulado = acumulado, seleccion = seleccion),
  file.path(ruta_salida, "ipc_acumulado_2021_2025.xlsx")
)