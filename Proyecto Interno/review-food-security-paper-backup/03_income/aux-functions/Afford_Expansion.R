#------------------------------------------------#
#           Definición de la función             #
#   CORRECCIÓN: uso de factores de expansión     #
#   (fex_c18) en el cálculo de rate, gap y       #
#   severity (FGT0, FGT1, FGT2)                 #
#------------------------------------------------#

Afford_Exp <- function(Hexpense, Model_CoCA = NULL, Model_CoNA = NULL, Model_CoRD = NULL) {
  
  # Carga de librerías
  Librerias_base <- c("here", "readxl", "tidyverse", "knitr", "moments", "xgboost", "maditr",
                      "mice", "VIM", "dplyr", "finalfit", "plyr", "hdd", "zip", "httr",
                      "caret", "nnet", "quantreg", "gridExtra", "ggpubr", "cowplot", "Hmisc")
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(char = Librerias_base, character.only = TRUE)
  
  #-------------------------------------------------#
  #  Validación de parámetros de la función        #
  #-------------------------------------------------#
  validar_parametros <- function(Hexpense, Model_CoCA, Model_CoNA, Model_CoRD) {
    
    if (!is.data.frame(Hexpense)) {
      stop("Hexpense debe ser un data.frame")
    }
    
    # fex_c18 es necesario para la ponderación
    columnas_esperadas <- c("deciles", "income", "ung", "per_capita_income",
                            "food_exp_per_capita", "fex_c18")
    columnas_faltantes <- setdiff(columnas_esperadas, names(Hexpense))
    
    if (length(columnas_faltantes) > 0) {
      stop("Hexpense le faltan las siguientes columnas: ",
           paste(columnas_faltantes, collapse = ", "))
    }
    
    validar_df_per_capita <- function(model_name, model) {
      if (is.numeric(model)) {
        return(TRUE)
      } else if (is.data.frame(model)) {
        if (!"per_capita" %in% names(model)) {
          stop(paste(model_name, "debe ser un data.frame con la columna 'per_capita'"))
        }
      } else {
        stop(paste(model_name, "debe ser numérico o un data.frame"))
      }
      return(TRUE)
    }
    
    if (!is.null(Model_CoCA)) validar_df_per_capita("Model_CoCA", Model_CoCA)
    if (!is.null(Model_CoNA)) validar_df_per_capita("Model_CoNA", Model_CoNA)
    if (!is.null(Model_CoRD)) validar_df_per_capita("Model_CoRD", Model_CoRD)
    
    if (is.null(Model_CoCA) && is.null(Model_CoNA) && is.null(Model_CoRD)) {
      stop("Al menos uno de los modelos (Model_CoCA, Model_CoNA, Model_CoRD) debe no ser NULL")
    }
  }
  
  validar_parametros(Hexpense, Model_CoCA, Model_CoNA, Model_CoRD)
  
  # -------------------------------------------------------
  # calculate_outcome: FGT0, FGT1, FGT2 ponderados por fex
  # -------------------------------------------------------
  calculate_outcome <- function(dataset, model, deciles_grupos, model_name) {
    
    if (is.data.frame(model)) {
      z <- as.numeric(levels(as.factor(model$per_capita * 365)))
    } else {
      z <- as.numeric(model) * 365
    }
    
    outcome_list <- list()
    
    for (j in seq_along(deciles_grupos)) {
      df_y <- dataset %>% filter(deciles %in% deciles_grupos[j])
      
      # Umbral de inasequibilidad
      df_y$dummy <- ifelse(df_y$food_exp_per_capita_year < z, 1, 0)
      
      df_z <- df_y %>% filter(dummy == 1)
      
      df_z$brecha_rel     <- (z - df_z$food_exp_per_capita_year) / z
      df_z$brecha_rel_sqr <- df_z$brecha_rel^2
      
      # -------------------------------------------------------
      # CORRECCIÓN PRINCIPAL:
      # Usar suma de factores de expansión (fex_c18) como
      # denominador, y ponderar numeradores con fex_c18.
      # Esto produce estimadores poblacionales correctos.
      #
      # FGT0 (rate):     % ponderado de hogares inasequibles
      # FGT1 (gap):      brecha media ponderada / N_ponderado
      # FGT2 (severity): brecha² media ponderada / N_ponderado
      # -------------------------------------------------------
      N_pond <- sum(df_y$fex_c18, na.rm = TRUE)
      
      rate <- (sum(df_z$fex_c18, na.rm = TRUE) / N_pond) * 100
      
      gap <- sum(df_z$brecha_rel * df_z$fex_c18, na.rm = TRUE) / N_pond
      
      severity <- sum(df_z$brecha_rel_sqr * df_z$fex_c18, na.rm = TRUE) / N_pond
      
      df_w <- data.frame(
        deciles  = deciles_grupos[j],
        rate     = rate,
        gap      = gap,
        severity = severity,
        model    = model_name)
      
      outcome_list[[j]] <- df_w
    }
    
    names(outcome_list) <- deciles_grupos
    return(outcome_list)
  }
  
  # Grupos de deciles
  deciles_grupos <- c("Decil 1", "Decil 2", "Decil 3", "Decil 4", "Decil 5",
                      "Decil 6", "Decil 7", "Decil 8", "Decil 9", "Decil 10")
  
  outcome_1_list <- list()
  outcome_2_list <- list()
  outcome_3_list <- list()
  
  suppressWarnings({
    if (!is.null(Model_CoCA)) {
      cat("Ejecutando cálculo para Model_CoCA.\n")
      outcome_1_list <- calculate_outcome(Hexpense, Model_CoCA, deciles_grupos, "CoCA")
    }
    if (!is.null(Model_CoNA)) {
      cat("Ejecutando cálculo para Model_CoNA.\n")
      outcome_2_list <- calculate_outcome(Hexpense, Model_CoNA, deciles_grupos, "CoNA")
    }
    if (!is.null(Model_CoRD)) {
      cat("Ejecutando cálculo para Model_CoRD.\n")
      outcome_3_list <- calculate_outcome(Hexpense, Model_CoRD, deciles_grupos, "CoRD")
    }
    
    poverty_outcome <- do.call(rbind, c(outcome_1_list, outcome_2_list, outcome_3_list))
    
    #--------------------------------------------------#
    # Estadísticas medias por decil (ponderadas)       #
    #--------------------------------------------------#
    mean_income <- data.frame(deciles_grupos)
    mean_income$ingreso_prom          <- NA
    mean_income$size_prom             <- NA
    mean_income$n                     <- NA
    mean_income$min_ing_pc            <- NA
    mean_income$max_ing_pc            <- NA
    mean_income$ing_per_capita_prom   <- NA
    mean_income$share                 <- NA
    mean_income$food                  <- NA
    mean_income$min_food_pc           <- NA
    mean_income$max_food_pc           <- NA
    mean_income$food_per_capita_prom  <- NA
    
    for (k in seq_along(deciles_grupos)) {
      df    <- Hexpense %>% filter(deciles %in% deciles_grupos[k])
      y_1   <- which(mean_income$deciles_grupos == deciles_grupos[k])
      
      mean_income$ingreso_prom[y_1]        <- wtd.mean(df$income,           weights = df$fex_c18)
      mean_income$size_prom[y_1]           <- wtd.mean(df$ung,              weights = df$fex_c18)
      mean_income$n[y_1]                   <- sum(df$fex_c18)
      mean_income$min_ing_pc[y_1]          <- min(df$per_capita_income)
      mean_income$max_ing_pc[y_1]          <- max(df$per_capita_income)
      mean_income$ing_per_capita_prom[y_1] <- wtd.mean(df$per_capita_income, weights = df$fex_c18)
      mean_income$share[y_1]               <- as.numeric(levels(as.factor(df$share)))
      mean_income$food[y_1]                <- wtd.mean(df$food_income,       weights = df$fex_c18)
      mean_income$min_food_pc[y_1]         <- min(df$food_exp_per_capita)
      mean_income$max_food_pc[y_1]         <- max(df$food_exp_per_capita)
      mean_income$food_per_capita_prom[y_1]<- wtd.mean(df$food_exp_per_capita, weights = df$fex_c18)
    }
    
    mean_income_deciles <- mean_income
    names(mean_income_deciles) <- c(
      "deciles", "average_income", "average_size", "n",
      "min_income_per_capita", "max_income_per_capita",
      "average_income_per_capita", "share", "food_income",
      "min_food_per_capita", "max_food_per_capita",
      "average_food_exp_per_capita")
    
    función_detc <- function(model) {
      if (is.data.frame(model)) {
        z <- as.numeric(model$per_capita) * 30
      } else {
        z <- as.numeric(model) * 30
      }
      return(z)
    }
    
    umbral_1 <- if (!is.null(Model_CoCA)) as.numeric(levels(as.factor(función_detc(Model_CoCA)))) else NA
    umbral_2 <- if (!is.null(Model_CoNA)) as.numeric(levels(as.factor(función_detc(Model_CoNA)))) else NA
    umbral_3 <- if (!is.null(Model_CoRD)) as.numeric(levels(as.factor(función_detc(Model_CoRD)))) else NA
    
    mean_income_food <- mean_income_deciles[c("deciles", "average_food_exp_per_capita")]
    mean_income_food$umbral_1 <- umbral_1
    mean_income_food$umbral_2 <- umbral_2
    mean_income_food$umbral_3 <- umbral_3
    mean_income_food$ratio_1  <- if (!is.null(Model_CoCA)) mean_income_food$umbral_1 / mean_income_food$average_food_exp_per_capita else NA
    mean_income_food$ratio_2  <- if (!is.null(Model_CoNA)) mean_income_food$umbral_2 / mean_income_food$average_food_exp_per_capita else NA
    mean_income_food$ratio_3  <- if (!is.null(Model_CoRD)) mean_income_food$umbral_3 / mean_income_food$average_food_exp_per_capita else NA
    names(mean_income_food) <- c("decile_groups", "food_per_capita_avg",
                                 "threshold_1", "threshold_2", "threshold_3",
                                 "ratio_1", "ratio_2", "ratio_3")
    
    rownames(poverty_outcome) <- NULL
    Resultados <- list(poverty_outcome, mean_income_food)
    names(Resultados) <- c("Poverty_outcome", "Mean_income_food")
    
    Sys.sleep(1); cat("Finalizado ✓ \n")
  })
  
  return(invisible(Resultados))
}