########################################################
## FUNCION correr_con_cambios()
## 06_sensibilidad_umbral/aux-functions/correr_con_cambios.R
##
## Corre un script del proyecto cambiando en memoria algunas de sus
## lineas (rutas de entrada y salida o un parametro), sin modificar el
## archivo.
##
## Parametros
##   script:  ruta del script relativa a dir_interno
##   cambios: lista con nombre; una clave que empieza con ^ reemplaza
##            la linea completa que la cumple, las demas solo el texto
##            que coincide
##   etiqueta: texto que se imprime junto al nombre del script
##
## Necesita dir_interno definido en el entorno global
########################################################

correr_con_cambios <- function(script, cambios, etiqueta = "") {
  lineas <- readLines(file.path(dir_interno, script), encoding = "UTF-8", warn = FALSE)
  for (clave in names(cambios)) {
    stopifnot(any(grepl(clave, lineas)))
    if (startsWith(clave, "^")) {
      lineas <- sub(paste0(clave, ".*$"), cambios[[clave]], lineas)
    } else {
      lineas <- sub(clave, cambios[[clave]], lineas)
    }
  }
  message("\n== ", script, " | ", etiqueta)
  eval(parse(text = lineas), envir = new.env(parent = globalenv()))
}
