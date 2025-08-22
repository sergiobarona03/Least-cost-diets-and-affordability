# ==== Directorio de trabajo ====
setwd("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno")

library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(purrr)
library(broom)

# ==== Parámetros ====
ciudad_objetivo <- "CALI"
fecha_tope      <- as.Date("2018-03-01")
anio_base       <- 2008

# ==== 1. Leer correlativa ====
ipc_subclase = readxl::read_excel("var-ipc/XYZ_Correlativa-ENPH-IPC-2008.xlsx") %>% 
  janitor::clean_names() %>% mutate(clase = clase_9,articulo = articulo_11) %>%
  select(clase, gasto_basico, articulo, descripcion_ipc)

# ==== 2. Leer precios DANE ====
precios <- read_excel("Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx") %>%
  clean_names() %>%
  transmute(
    anio        = ano,
    mes         = mes_num,
    ciudad      = nombre_ciudad,
    articulo    = codigo_articulo,
    nombre_art  = articulo,
    precio      = precio_500g,
    fecha       = as.Date(sprintf("%d-%02d-01", ano, mes))
  ) %>%
  filter(ciudad == ciudad_objetivo, fecha <= fecha_tope)

# ==== 3. Leer precios IPC ====
var_ipc <- readxl::read_excel("var-ipc/IPC.xls") %>%
  clean_names() %>%
  mutate(
    ciudad = dplyr::recode(ciudad,
                           "CARTAGENA DE INDIAS" = "CARTAGENA",
                           "BOGOTÁ, D.C."        = "BOGOTÁ D.C."
    ),
    mes_num = dplyr::recode(mes,
                            "Ene"=1,"Feb"=2,"Mar"=3,"Abr"=4,"May"=5,"Jun"=6,
                            "Jul"=7,"Ago"=8,"Sep"=9,"Oct"=10,"Nov"=11,"Dic"=12
    ),
    fecha = as.Date(sprintf("%d-%02d-01", as.integer(ano), mes_num)),
    subclase = substr(subclase, 1, 8)   
  ) %>%
  filter(ciudad == ciudad_objetivo, fecha <= fecha_tope) %>%
  transmute(fecha, subclase, lnIPC = log(numero_indice))

# ==== 4. Unir precios con correlativa ====
mapeo_unico <- ipc_subclase %>%
  mutate(subclase_from_art = paste0("0", substr(articulo, 1, 6), "0"),
         match_flag = (clase == subclase_from_art)) %>%
  group_by(articulo) %>%
  slice(if (any(match_flag, na.rm = TRUE)) which.max(match_flag) else 1L) %>%
  ungroup() %>%
  transmute(articulo,
            subclase = if_else(!is.na(clase) & clase == subclase_from_art,
                               clase, subclase_from_art),
            descripcion_ipc) %>%
  distinct(articulo, .keep_all = TRUE)

df <- precios %>% left_join(mapeo_unico, by = "articulo")

# ==== 5. Definir P0  ====
p0_tabla <- df %>%
  filter(anio == anio_base) %>%
  group_by(articulo) %>%
  summarise(p0 = mean(precio, na.rm = TRUE), .groups = "drop")

fallback_p0 <- df %>%
  arrange(articulo, fecha) %>%
  group_by(articulo) %>%
  summarise(p0_fb = first(na.omit(precio)), .groups = "drop")

p0_tabla <- full_join(p0_tabla, fallback_p0, by = "articulo") %>%
  mutate(p0 = if_else(is.na(p0), p0_fb, p0)) %>%
  select(articulo, p0)

# ==== 6. Construir panel con ln_rel y lnIPC ====
panel <- df %>%
  left_join(p0_tabla, by = "articulo") %>%
  mutate(ln_rel = log(precio) - log(p0)) %>%
  left_join(var_ipc, by = c("fecha", "subclase"))

# ==== 7. Pasar a formato ancho ====
df_wide <- panel %>%
  select(subclase, fecha, lnIPC, articulo, ln_rel) %>%
  pivot_wider(names_from = articulo, values_from = ln_rel,
              names_prefix = "ln_rel_")

# ==== 8. Regresiones por subclase ====
xvars <- grep("^ln_rel_", names(df_wide), value = TRUE)

simple_lm <- purrr::possibly(function(d) {
  
  cols <- intersect(c("lnIPC", xvars), names(d))
  if (length(cols) <= 1) return(tibble())  
  
  fit <- lm(lnIPC ~ ., data = d[, cols, drop = FALSE],
            na.action = na.exclude)
  
  broom::tidy(fit) |>
    dplyr::filter(term != "(Intercept)", !is.na(estimate))
}, otherwise = tibble())

res <- df_wide %>%
  dplyr::group_by(subclase) %>%
  dplyr::group_modify(~ safe_lm(.x)) %>%
  dplyr::mutate(alimento = sub("^ln_rel_", "", term)) %>%
  dplyr::select(subclase, alimento, estimate, std.error, statistic, p.value)

n_por_subclase <- res %>%
  count(subclase, name = "n_alimentos")

res_named <- res %>%
  left_join(
    ipc_subclase %>% select(articulo, descripcion_ipc) %>% distinct(),
    by = c("alimento" = "articulo")
  ) %>%
  relocate(descripcion_ipc, .after = alimento)
