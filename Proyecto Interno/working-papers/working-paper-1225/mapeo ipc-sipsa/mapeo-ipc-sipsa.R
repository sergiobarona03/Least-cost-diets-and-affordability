
##------------------------------------##
## Cargar mapeo: DANE (IPC) - SIPSA   ##
##------------------------------------##

source("working-papers\\working-paper-1225\\mapeo ipc-sipsa\\join-ipc-sipsa.R")

# Cargar datos de composición nutricional
sipsa_tcac <- readxl::read_excel("composicion-nut/1823_mapeo_sipsa_tcac v1.0_2025.xlsx") %>%
  janitor::clean_names() %>%
  rename(food_sipsa = alimento_nombre_sipsa)

# Cargar mapeo IPC - SIPSA
ipc_sipsa = readxl::read_excel("composicion-nut\\Copia_DANE_4_DIC_2025act.xlsx") %>%
  janitor::clean_names() %>% mutate(retail = articulo_dane) %>%
  select(retail, mapeo_sipsa, codigo_tcac)

# Recuperar el nombre de sipsa
ipc_sipsa = merge(ipc_sipsa, sipsa_tcac[c("codigo_tcac", "food_sipsa")], by = "codigo_tcac") %>%
  rename(sipsa = food_sipsa) %>% distinct()

# Mantener el primer alimento mapeado
ipc_sipsa <- ipc_sipsa %>%
  group_by(mapeo_sipsa, sipsa) %>%
  mutate(
    retail =
      retail[which.min(nchar(retail))]
  ) %>%
  slice(1) %>%
  ungroup()
