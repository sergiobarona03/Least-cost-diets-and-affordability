# =========================================================
# Convertir archivos CSV / DTA / TXT a .rds
# =========================================================

# ---------------------------
# 1. Paquetes
# ---------------------------
library(data.table)
library(haven)
library(tools)

# ---------------------------
# 2. Archivos de entrada
# ---------------------------
files <- c(
  "C:/Users/danie/OneDrive/Documentos/least-cost/EER/cnpv2018/CNPV2018_5PER_A2_05.CSV",
  "C:/Users/danie/OneDrive/Documentos/least-cost/EER/cnpv2018/CNPV2018_5PER_A2_11.CSV",
  "C:/Users/danie/OneDrive/Documentos/least-cost/EER/cnpv2018/CNPV2018_5PER_A2_76.CSV",
  "C:/Users/danie/OneDrive/Documentos/least-cost/EER/ensin-2015/Formato_Stata/AF_ADOLESCENTES.dta",
  "C:/Users/danie/OneDrive/Documentos/least-cost/EER/ensin-2015/Formato_Stata/AF_ADULTOS.dta",
  "C:/Users/danie/OneDrive/Documentos/least-cost/EER/ensin-2015/Formato_Stata/ANTROPOMETRIA.DTA",
  "C:/Users/danie/OneDrive/Documentos/least-cost/EER/base-de-datos-SABE-2015/Base de datos y diccionario/Base de datos - Capitulos/Cap1Parte1.txt",
  "C:/Users/danie/OneDrive/Documentos/least-cost/EER/base-de-datos-SABE-2015/Base de datos y diccionario/Base de datos - Capitulos/Cap4.txt",
  "C:/Users/danie/OneDrive/Documentos/least-cost/EER/base-de-datos-SABE-2015/Base de datos y diccionario/Base de datos - Capitulos/Cap7.txt",
  "C:/Users/danie/OneDrive/Documentos/least-cost/EER/base-de-datos-SABE-2015/Base de datos y diccionario/Base de datos - Capitulos/Cap10.txt"
)

# ---------------------------
# 3. Carpeta de salida
# ---------------------------
out_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/input/requirements"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# ---------------------------
# 4. Función para leer archivos
# ---------------------------
read_any_file <- function(path) {
  
  ext <- tolower(file_ext(path))
  message("\nLeyendo archivo: ", path)
  
  if (ext == "csv") {
    
    # CSV grandes
    obj <- fread(
      input = path,
      encoding = "UTF-8",
      showProgress = TRUE
    )
    
  } else if (ext == "dta") {
    
    # Stata
    obj <- read_dta(path)
    
  } else if (ext == "txt") {
    
    # Intento 1: detección automática
    obj <- tryCatch(
      fread(
        input = path,
        encoding = "UTF-8",
        showProgress = TRUE
      ),
      error = function(e) {
        message("Fallo con UTF-8/autodetección. Probando con Latin-1 y separador tab...")
        NULL
      }
    )
    
    # Intento 2: tab + Latin-1
    if (is.null(obj)) {
      obj <- tryCatch(
        fread(
          input = path,
          sep = "\t",
          encoding = "Latin-1",
          showProgress = TRUE
        ),
        error = function(e) {
          message("Fallo con tab. Probando con separador ; ...")
          NULL
        }
      )
    }
    
    # Intento 3: ; + Latin-1
    if (is.null(obj)) {
      obj <- tryCatch(
        fread(
          input = path,
          sep = ";",
          encoding = "Latin-1",
          showProgress = TRUE
        ),
        error = function(e) {
          message("Fallo con ;. Probando con separador , ...")
          NULL
        }
      )
    }
    
    # Intento 4: , + Latin-1
    if (is.null(obj)) {
      obj <- tryCatch(
        fread(
          input = path,
          sep = ",",
          encoding = "Latin-1",
          showProgress = TRUE
        ),
        error = function(e) {
          stop("No se pudo leer el archivo TXT: ", path)
        }
      )
    }
    
  } else {
    stop("Extensión no soportada: ", ext, " en archivo ", path)
  }
  
  return(obj)
}

# ---------------------------
# 5. Convertir y guardar
# ---------------------------
resultados <- vector("list", length(files))

for (i in seq_along(files)) {
  
  f <- files[i]
  
  if (!file.exists(f)) {
    warning("No existe el archivo: ", f)
    resultados[[i]] <- data.frame(
      archivo_origen = f,
      archivo_salida = NA_character_,
      existe_origen = FALSE,
      guardado = FALSE,
      filas = NA_integer_,
      columnas = NA_integer_,
      stringsAsFactors = FALSE
    )
    next
  }
  
  obj <- read_any_file(f)
  
  out_file <- paste0(file_path_sans_ext(basename(f)), ".rds")
  out_path <- file.path(out_dir, out_file)
  
  saveRDS(obj, out_path, compress = "xz")
  
  message("Guardado en: ", out_path)
  message("Dimensiones: ", nrow(obj), " filas x ", ncol(obj), " columnas")
  
  resultados[[i]] <- data.frame(
    archivo_origen = f,
    archivo_salida = out_path,
    existe_origen = TRUE,
    guardado = file.exists(out_path),
    filas = nrow(obj),
    columnas = ncol(obj),
    stringsAsFactors = FALSE
  )
}

# ---------------------------
# 6. Resumen final
# ---------------------------
resumen <- rbindlist(resultados, fill = TRUE)

print(resumen)

# ---------------------------
# 7. Ver archivos .rds creados
# ---------------------------
rds_creados <- list.files(
  path = out_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

print(rds_creados)
