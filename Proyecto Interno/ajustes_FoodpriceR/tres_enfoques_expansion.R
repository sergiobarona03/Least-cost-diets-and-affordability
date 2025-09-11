###----------------------------###
### Enfoque 1                  ###
###----------------------------###
gastos_ingresos_exp <- dataset_def %>%
  dplyr::slice(rep(1:n(), times = dataset_def$fex_c18)) %>%
  na.omit()

gastos_ingresos_exp$per_capita = gastos_ingresos_exp$ingresos/gastos_ingresos_exp$nug
dataset_def_deciles = gastos_ingresos_exp %>% mutate(deciles = ntile(per_capita, 10))
dataset_def_deciles$deciles = revalue(as.factor(dataset_def_deciles$deciles),
                                      c("1" = "Decil 1", "2" = "Decil 2",
                                        "3" = "Decil 3", "4" = "Decil 4",
                                        "5" = "Decil 5", "6" = "Decil 6",
                                        "7" = "Decil 7", "8" = "Decil 8",
                                        "9" = "Decil 9", "10" = "Decil 10"))
geih_ingresos = dataset_def_deciles
dataset_def_deciles = dataset_def_deciles
# Hallar el ingreso promedio por decil
deciles_grupos = c("Decil 1", "Decil 2",
                   "Decil 3", "Decil 4",
                   "Decil 5", "Decil 6",
                   "Decil 7", "Decil 8",
                   "Decil 9", "Decil 10")


resumen_enfoque1 <- dataset_def_deciles %>%
  mutate(deciles = as.factor(deciles)) %>%
  dplyr::group_by(deciles) %>%
  dplyr::summarise(
    mean = mean(per_capita, na.rm = TRUE),
    sd   = sd(per_capita, na.rm = TRUE),
    median = median(per_capita, na.rm = TRUE),
    q1 = quantile(per_capita, 0.25, na.rm = TRUE),
    q3 = quantile(per_capita, 0.75, na.rm = TRUE)
  ) %>%
  mutate(Enfoque = "Enfoque 1")


###----------------------------###
### Enfoque 2                  ###
###----------------------------###
# Usando la librería Hmisc
library(Hmisc)

dataset_def$per_capita <- dataset_def$ingresos / dataset_def$nug

deciles_cut <- wtd.quantile(dataset_def$per_capita,
                            weights = dataset_def$fex_c18,
                            probs = seq(0, 1, 0.1))

dataset_def$deciles <- cut(dataset_def$per_capita,
                           breaks = deciles_cut,
                           labels = paste0("Decil ", 1:10),
                           include.lowest = TRUE)

resumen_enfoque2 <- dataset_def_deciles %>%
  mutate(deciles = as.factor(deciles)) %>%
  dplyr::group_by(deciles) %>%
  dplyr::summarise(
    mean = wtd.mean(per_capita, 
                    weights = fex_c18, na.rm = TRUE),
    sd   = sqrt(wtd.var(per_capita,
                        weights = fex_c18, na.rm = TRUE)),
    median = wtd.quantile(per_capita, 
                          weights = fex_c18, probs = 0.5),
    q1 = wtd.quantile(per_capita, weights = fex_c18, probs = 0.25),
    q3 = wtd.quantile(per_capita, weights = fex_c18, probs = 0.75)
  ) %>%
  mutate(Enfoque = "Enfoque 2")

###----------------------------###
### Enfoque 3                  ###
###----------------------------###

# Usando la librería survey
library(survey)
des <- svydesign(ids = ~1, weights = ~fex_c18, data = dataset_def)

# ingreso per cápita
dataset_def$per_capita <- dataset_def$ingresos / dataset_def$nug

# puntos de corte
deciles_cut <- svyquantile(~per_capita, des, quantiles = seq(0,1,0.1))

# asignación de deciles
dataset_def$deciles <- cut(dataset_def$per_capita,
                             breaks = as.numeric(deciles_cut$per_capita[,1]),
                           labels = paste0("Decil ", 1:10),
                           include.lowest = TRUE)

des <- svydesign(ids = ~1, weights = ~fex_c18, data = dataset_def)

# Survey stats
# Media y varianza por decil
mean_sd <- svyby(~per_capita, ~deciles, des, 
                 FUN = svymean, keep.var = TRUE, na.rm = TRUE)

# Cuantiles por decil (ejemplo con mediana, q1, q3)
mediana <- svyby(~per_capita, ~deciles, 
                 des, 
                 FUN = svyquantile, 
                 quantile = 0.5,na.rm = TRUE)

q1 <- svyby(~per_capita, ~deciles, des, 
            FUN = svyquantile, quantile = 0.25, 
            na.rm = TRUE)

q3 <- svyby(~per_capita, ~deciles, des, 
            FUN = svyquantile, quantile = 0.75,
            na.rm = TRUE)

# Unir todo en un tibble
resumen_enfoque3 <- mean_sd %>%
  as_tibble() %>%
  select(deciles, mean = per_capita, se) %>%
  left_join(as_tibble(mediana) %>% select(deciles, median = per_capita),
            by = "deciles") %>%
  left_join(as_tibble(q1) %>% select(deciles, q1 = per_capita),
            by = "deciles") %>%
  left_join(as_tibble(q3) %>% select(deciles, q3 = per_capita),
            by = "deciles") %>%
  mutate(sd = se * sqrt(length(weights(des)))) %>%
  select(deciles, mean, sd, median, q1, q3) %>%
  mutate(Enfoque = "Enfoque 3")

###----------------------------###
### Enfoque 1-3                ###
###----------------------------###

enfoques_1_3 = rbind(resumen_enfoque1,
                     resumen_enfoque2, resumen_enfoque3)

