# Librería
library(dplyr)

# Definición de data.frame para las fechas
fechas <- data.frame(
  month = rep(1:12, 13),
  year = rep(2013:2025, each = 12) 
)

# Obtener los años únicos
years <- unique(fechas$year)

# Bucle por año
for (y in years) {
  df_year <- data.frame()  # DataFrame vacío para cada año
  
  
  for (m in 1:12) {
    print(paste0("Done ", m, "-", y))
    
    df.aux <- as.data.frame(DataCol3(Month = m, Year = y)) %>%
      mutate(Month = m, Year = y)  
    
    df_year <- bind_rows(df_year, df.aux)  
  }
  
  # Guardar el archivo 
  saveRDS(df_year, file = file.path("C:/Users/danie/OneDrive/Documentos", paste0(y, ".rds")))

}
