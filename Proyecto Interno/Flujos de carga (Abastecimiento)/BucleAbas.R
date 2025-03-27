# Librería
library(dplyr)

# Definición de data.frame para las fechas
fechas <- data.frame(
  month = rep(1:12, 7),
  year = rep(2018:2024, each = 12) 
)

# Obtener los años únicos
years <- unique(fechas$year)

# Bucle por año
for (y in years) {
  df_year <- data.frame()  # DataFrame vacío para cada año
  
<<<<<<< HEAD
  for (m in 1:12) {
    print(paste0("Done ", m, "-", y))
    
    df.aux <- as.data.frame(DataCol2(Month = m, Year = y)) %>%
      mutate(Month = m, Year = y)  
    
    df_year <- bind_rows(df_year, df.aux)  
  }
  
  # Guardar el archivo 
  saveRDS(df_year, file = file.path("C:/Users/danie/OneDrive/Documentos", paste0(y, ".rds")))
  
  
<<<<<<< HEAD

}






=======
final_list = do.call(rbind, list_output)

write.csv(final_list, "C:/Users/danie/Downloads/Práctica/Avance 2/foodlist_2018_2024.csv", row.names = FALSE)
>>>>>>> parent of 76d22ca (Actualización info)
=======
final_list = do.call(rbind, list_output)

write.csv(final_list, "C:/Users/danie/Downloads/Práctica/Avance 2/foodlist_2018_2024.csv", row.names = FALSE)
>>>>>>> parent of 76d22ca (Actualización info)
