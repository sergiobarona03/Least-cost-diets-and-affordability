#------------------------------------------------------------------------------------------#
#         FUNCIÓN: MULTI_CoNA — DIETA ADECUADA EN NUTRIENTES CON PATRÓN DIETÉTICO         #
#                  Multiobjective LP — Weighted sum method                                 #
#                                                                                          #
# Parámetros:                                                                              #
#   data       : data.frame con columnas Food, Price_100g, Energy, nutrientes, Group       #
#   EER_LL     : data.frame con requerimientos mínimos (Age, Sex, Energy, nutrientes)      #
#   UL         : data.frame con límites superiores (Age, Sex, nutrientes)                  #
#   IPC_shares : data.frame con columnas Group y share (participaciones IPC, suma = 1)     #
#   lambda     : escalar en [0,1]. 0 = CoNA estándar, 1 = máxima adherencia IPC            #
#                Si es un vector, devuelve una lista con resultados para cada lambda       #
#   exclude    : vector de alimentos a excluir (opcional)                                  #
#------------------------------------------------------------------------------------------#

Multi_CoNA_paper <- function(data,
                             EER_LL,
                             UL,
                             IPC_shares,
                             lambda     = 0.5,
                             exclude    = NULL) {
  
  #------------------------------------------------------------------------------------------#
  #                       ETAPA 1: VALIDACIÓN DE LIBRERÍAS                                  #
  #------------------------------------------------------------------------------------------#
  
  Librerias_base <- c("tidyverse", "rio", "janitor", "stringdist", "lpSolve", "knitr")
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(char = Librerias_base)
  
  paquetes_faltantes <- Librerias_base[!(Librerias_base %in% pacman::p_loaded())]
  for (paquete in paquetes_faltantes) {
    if (!require(paquete, character.only = TRUE)) {
      install.packages(paquete)
      library(paquete, character.only = TRUE)
    }
  }
  
  #------------------------------------------------------------------------------------------#
  #                       ETAPA 2: VALIDACIÓN DE PARÁMETROS                                 #
  #------------------------------------------------------------------------------------------#
  
  validate_columns <- function(df, required_columns, model_name) {
    if (!is.data.frame(df))
      stop(paste("Error:", model_name, "is not a data frame."))
    missing_columns <- setdiff(required_columns, colnames(df))
    if (length(missing_columns) > 0)
      stop(paste(model_name, "requires the following columns:",
                 paste(missing_columns, collapse = ", ")))
  }
  
  validate_columns(data,      c("Price_100g", "Food", "Energy", "Group"), "data")
  validate_columns(EER_LL,    c("Age", "Energy"),                          "EER_LL")
  validate_columns(UL,        c("Age"),                                    "UL")
  validate_columns(IPC_shares, c("Group", "share"),                        "IPC_shares")
  
  # IPC shares must sum to 1
  if (abs(sum(IPC_shares$share) - 1) > 1e-6)
    stop("IPC_shares$share must sum to 1.")
  
  # lambda must be in [0, 1]
  if (any(lambda < 0) || any(lambda > 1))
    stop("lambda must be a scalar or vector with all values in [0, 1].")
  
  # All groups in IPC_shares must exist in data
  missing_groups <- setdiff(IPC_shares$Group, unique(data$Group))
  if (length(missing_groups) > 0)
    warning("The following groups in IPC_shares are not in data$Group: ",
            paste(missing_groups, collapse = ", "))
  
  # Exclude foods if requested
  if (!is.null(exclude)) {
    if (!is.vector(exclude))
      stop("Error: The 'exclude' parameter must be a vector.")
    data <- data[!(data$Food %in% exclude), ]
  }
  
  #------------------------------------------------------------------------------------------#
  #                       ETAPA 3: PREPARACIÓN COMÚN                                       #
  #------------------------------------------------------------------------------------------#
  
  req_min_ent  <- EER_LL
  req_max_ent  <- UL %>% select(-any_of(c("Age", "Energy")))
  Req_entrantes <- cbind(req_min_ent, req_max_ent)
  
  groups_ipc <- IPC_shares$Group
  n_groups   <- length(groups_ipc)
  
  # Identify sex loop
  if ("Sex" %in% colnames(EER_LL)) {
    Sexos_min  <- split(EER_LL, EER_LL$Sex)
    Sexos_max  <- split(UL,     UL$Sex)
    if (!identical(names(Sexos_min), names(Sexos_max)))
      stop("Error: The genders in both requirements are not the same.")
    sexo_nombre <- names(Sexos_min)
  } else {
    sexo_nombre <- "0"
    DRI_min_i   <- EER_LL
    DRI_max_i   <- UL; DRI_max_i[is.na(DRI_max_i)] <- 999999
  }
  
  #------------------------------------------------------------------------------------------#
  #        ETAPA 4: FUNCIÓN INTERNA — resuelve el LP multiobjetivo para un lambda dado      #
  #                                                                                          #
  # Variables del LP extendido:                                                              #
  #   [x_1, ..., x_n,  d_1+, ..., d_G+,  d_1-, ..., d_G-]                                  #
  #   dimensión total: n_foods + 2 * n_groups                                               #
  #------------------------------------------------------------------------------------------#
  
  solve_multi_lp <- function(Precio, Food_vec, Coef.Restriq,
                             constr_signs, Limitaciones_row,
                             E_star, f1_star, f2_star,
                             lam) {
    
    n_foods <- length(Precio)
    n_vars  <- n_foods + 2 * n_groups
    
    # ---- Objetivo ponderado y normalizado:
    # (1 - lambda) * sum(p_i * x_i) / f1*  +  lambda * sum(d_g+ + d_g-) / f2*
    #
    # Coeficientes:
    #   x_i    -> (1 - lambda) * p_i / f1*
    #   d_g+   -> lambda / f2*
    #   d_g-   -> lambda / f2*
    #
    # Caso especial: si f2* = 0 (patrón IPC perfectamente replicable),
    # el segundo término degenera; usamos un pequeño epsilon para evitar /0
    f2_denom <- max(f2_star, 1e-10)
    
    obj_ws <- c(
      (1 - lam) * Precio / f1_star,
      rep(lam / f2_denom, n_groups),
      rep(lam / f2_denom, n_groups)
    )
    
    # ---- Restricciones nutricionales (solo involucran x_i)
    # Extender Coef.Restriq con ceros para d_g+ y d_g-
    n_nutr_constraints <- nrow(Coef.Restriq)
    A_nutr_pad <- cbind(
      Coef.Restriq,
      matrix(0, nrow = n_nutr_constraints, ncol = 2 * n_groups)
    )
    
    # ---- Restricción de gasto total: sum(p_i * x_i) = E*
    A_exp <- matrix(c(Precio, rep(0, 2 * n_groups)), nrow = 1)
    
    # ---- Restricciones de desviación del patrón IPC (una por grupo):
    # sum_{i in g}(p_i * x_i) - d_g+ * E* + d_g- * E* = s_g * E*
    A_dev   <- matrix(0, nrow = n_groups, ncol = n_vars)
    rhs_dev <- numeric(n_groups)
    
    for (j in seq_len(n_groups)) {
      g   <- groups_ipc[j]
      s_g <- IPC_shares$share[IPC_shares$Group == g]
      
      in_group <- data$Group == g
      # Expenditure coefficient for foods in group g
      A_dev[j, which(in_group)] <- Precio[in_group]
      # d_g+ coefficient: -E*
      A_dev[j, n_foods + j]              <- -E_star
      # d_g- coefficient: +E*
      A_dev[j, n_foods + n_groups + j]   <-  E_star
      # RHS
      rhs_dev[j] <- s_g * E_star
    }
    
    # ---- Ensamblar sistema completo
    A_full   <- rbind(A_nutr_pad, A_exp, A_dev)
    dir_full <- c(constr_signs, "=", rep("=", n_groups))
    rhs_full <- c(Limitaciones_row, E_star, rhs_dev)
    
    # ---- Resolver
    sol <- lp(
      direction    = "min",
      objective.in = obj_ws,
      const.mat    = A_full,
      const.dir    = dir_full,
      const.rhs    = rhs_full,
      compute.sens = TRUE
    )
    
    sol
  }
  
  #------------------------------------------------------------------------------------------#
  #                       ETAPA 5: LOOP POR SEXO Y EDAD                                    #
  #------------------------------------------------------------------------------------------#
  
  # Wrapper: runs the full sex × age loop for a single lambda value
  run_single_lambda <- function(lam) {
    
    cat("\n--- lambda =", lam, "---\n")
    
    # Output collectors (same structure as CoNA_paper)
    Intercambios_Multi <- data.frame(Food = character(), quantity = numeric(),
                                     Demo_Group = character(), Sex = integer(),
                                     Group = character())
    Costo_T  <- data.frame(Demo_Group = character(), Sex = integer(),
                           cost_day = numeric(), Cost_1000kcal = numeric())
    N_limit  <- data.frame()
    S_shadow <- na.omit(data.frame(Age = NA, Sex = NA, Nutrients = NA,
                                   constraint = NA, value_constraint = NA,
                                   SP = NA, SPE = NA))
    
    for (sexo_nombre_i in sexo_nombre) {
      
      if ("Sex" %in% colnames(EER_LL)) {
        DRI_min_i <- Sexos_min[[sexo_nombre_i]]
        DRI_max_i <- Sexos_max[[sexo_nombre_i]]
        DRI_max_i[is.na(DRI_max_i)] <- 999999
      }
      
      DRI_min_i <- arrange(DRI_min_i, Age)
      DRI_max_i <- arrange(DRI_max_i, Age)
      
      if (!identical(levels(as.factor(DRI_min_i$Age)),
                     levels(as.factor(DRI_max_i$Age))))
        stop("Error: Age groups in EER_LL and UL do not match.")
      
      Precio    <- data$Price_100g
      Food_vec  <- data$Food
      Age       <- DRI_min_i$Age
      
      DRI_min_li <- DRI_min_i %>% select(-any_of(c("Age", "Energy", "Sex")))
      DRI_max_li <- DRI_max_i %>% select(-any_of(c("Age", "Energy", "Sex")))
      
      if (!identical(names(DRI_min_li), names(DRI_max_li)))
        stop("Error: UL and EER_LL do not have the same nutrient column names.")
      
      DRI_min_li <- DRI_min_i %>% select(-any_of(c("Age", "Sex")))
      DRI_max_li <- DRI_max_i %>% select(-any_of(c("Age", "Energy", "Sex")))
      
      DF_Nutrientes_Alimentos <- data %>%
        select(-any_of(c("Cod_TCAC", "Food", "Serving", "Price_100g", "Group",
                         "subgrupos_gabas", "ciudad", "fecha", "ano", "mes_num",
                         "Serving_g")))
      
      nombres_comunes <- intersect(names(DF_Nutrientes_Alimentos), names(DRI_min_li))
      
      DF_Nutrientes_Alimentos <- DF_Nutrientes_Alimentos %>% select(any_of(nombres_comunes))
      DRI_min_li <- DRI_min_li %>% select(any_of(nombres_comunes))
      DRI_max_li <- DRI_max_li %>% select(any_of(nombres_comunes))
      
      Sin_EER                 <- DF_Nutrientes_Alimentos %>% select(-Energy)
      DF_Nutrientes_Alimentos <- cbind(DF_Nutrientes_Alimentos, Sin_EER)
      
      Coef.Restriq  <- DF_Nutrientes_Alimentos %>% as.matrix() %>% t()
      constr_signs  <- c("=", rep(">=", ncol(DRI_min_li) - 1),
                         rep("<=", ncol(DRI_max_li)))
      Limitaciones  <- cbind(DRI_min_li, DRI_max_li)
      
      #--------------------------------------------------------------------#
      #  PASO A: Resolver CoNA estándar para obtener f1* y E*              #
      #--------------------------------------------------------------------#
      
      for (i in seq_along(Age)) {
        
        rhs_i <- as.vector(unlist(Limitaciones[i, , drop = FALSE]))
        
        sol_standard <- lp(
          direction    = "min",
          objective.in = Precio,
          const.mat    = Coef.Restriq,
          const.dir    = constr_signs,
          const.rhs    = rhs_i
        )
        
        if (sol_standard$status != 0 || sum(sol_standard$solution) == 0) {
          warning("Standard CoNA infeasible for Sex=", sexo_nombre_i,
                  " Age=", Age[i], ". Skipping.")
          next
        }
        
        f1_star <- sol_standard$objval
        E_star  <- f1_star   # fix total expenditure at CoNA optimum
        
        #------------------------------------------------------------------#
        #  PASO B: Resolver problema de patrón puro para obtener f2*       #
        #  (solo si lambda > 0; si lambda = 0 f2* no se necesita)          #
        #------------------------------------------------------------------#
        
        if (lam > 0) {
          
          n_foods <- length(Precio)
          n_vars  <- n_foods + 2 * n_groups
          
          obj_f2 <- c(rep(0, n_foods), rep(1, n_groups), rep(1, n_groups))
          
          A_nutr_pad <- cbind(Coef.Restriq,
                              matrix(0, nrow = nrow(Coef.Restriq),
                                     ncol = 2 * n_groups))
          A_exp <- matrix(c(Precio, rep(0, 2 * n_groups)), nrow = 1)
          
          A_dev   <- matrix(0, nrow = n_groups, ncol = n_vars)
          rhs_dev <- numeric(n_groups)
          
          for (j in seq_len(n_groups)) {
            g   <- groups_ipc[j]
            s_g <- IPC_shares$share[IPC_shares$Group == g]
            in_group <- data$Group == g
            A_dev[j, which(in_group)]          <- Precio[in_group]
            A_dev[j, n_foods + j]              <- -E_star
            A_dev[j, n_foods + n_groups + j]   <-  E_star
            rhs_dev[j] <- s_g * E_star
          }
          
          A_f2   <- rbind(A_nutr_pad, A_exp, A_dev)
          dir_f2 <- c(constr_signs, "=", rep("=", n_groups))
          rhs_f2 <- c(rhs_i, E_star, rhs_dev)
          
          sol_f2  <- lp(direction = "min", objective.in = obj_f2,
                        const.mat = A_f2, const.dir = dir_f2,
                        const.rhs = rhs_f2)
          
          f2_star <- if (sol_f2$status == 0) sol_f2$objval else 1
        } else {
          f2_star <- 1   # not used when lambda = 0
        }
        
        #------------------------------------------------------------------#
        #  PASO C: Resolver LP multiobjetivo ponderado                     #
        #------------------------------------------------------------------#
        
        sol_multi <- solve_multi_lp(
          Precio       = Precio,
          Food_vec     = Food_vec,
          Coef.Restriq = Coef.Restriq,
          constr_signs = constr_signs,
          Limitaciones_row = rhs_i,
          E_star       = E_star,
          f1_star      = f1_star,
          f2_star      = f2_star,
          lam          = lam
        )
        
        # Fallback: relax constraints iteratively (same logic as CoNA_paper)
        if (sol_multi$status != 0 || sum(sol_multi$solution[1:length(Precio)]) == 0) {
          
          solucion_encontrada <- FALSE
          porcentaje          <- 0.99
          nutrienteid         <- NA
          
          while (!solucion_encontrada && porcentaje > 0.9) {
            
            DRI_min_li_temp <- DRI_min_li %>% select(-Energy)
            
            for (j in seq_len(ncol(DRI_min_li_temp))) {
              DRI_temp <- DRI_min_li_temp
              DRI_temp[i, j] <- DRI_temp[i, j] * porcentaje
              Lim_temp <- cbind(DRI_min_li %>% select(Energy),
                                DRI_temp, DRI_max_li)
              rhs_temp <- as.vector(unlist(Lim_temp[i, , drop = FALSE]))
              
              # Recompute E_star with relaxed constraints
              sol_std_temp <- lp(direction = "min",
                                 objective.in = Precio,
                                 const.mat    = Coef.Restriq,
                                 const.dir    = constr_signs,
                                 const.rhs    = rhs_temp)
              
              if (sol_std_temp$status == 0 && sum(sol_std_temp$solution) != 0) {
                E_star_temp  <- sol_std_temp$objval
                f1_star_temp <- E_star_temp
                
                sol_try <- solve_multi_lp(
                  Precio       = Precio,
                  Food_vec     = Food_vec,
                  Coef.Restriq = Coef.Restriq,
                  constr_signs = constr_signs,
                  Limitaciones_row = rhs_temp,
                  E_star       = E_star_temp,
                  f1_star      = f1_star_temp,
                  f2_star      = f2_star,
                  lam          = lam
                )
                
                if (sol_try$status == 0 &&
                    sum(sol_try$solution[1:length(Precio)]) != 0) {
                  nutrienteid         <- names(DRI_min_li_temp)[j]
                  sol_multi           <- sol_try
                  E_star              <- E_star_temp
                  solucion_encontrada <- TRUE
                  rhs_i               <- rhs_temp
                  break
                }
              }
            }
            
            if (!solucion_encontrada) porcentaje <- porcentaje - 0.01
          }
          
          if (!solucion_encontrada) {
            warning("Multi_CoNA: no feasible solution for Sex=", sexo_nombre_i,
                    " Age=", Age[i], " lambda=", lam)
            next
          } else {
            print(paste0(
              "Multi_CoNA for Sex=", sexo_nombre_i, " Age=", Age[i],
              " lambda=", lam, " estimated at ", round(porcentaje * 100, 1),
              "% of minimum required intake of ", nutrienteid
            ))
          }
        }
        
        #------------------------------------------------------------------#
        #  PASO D: Extraer resultados (misma estructura que CoNA_paper)    #
        #------------------------------------------------------------------#
        
        x_sol      <- sol_multi$solution[1:length(Precio)]
        cost_day_i <- sum(x_sol * Precio)
        
        Alimentos_sol        <- which(x_sol > 1e-8)
        cantidades_sol       <- x_sol[Alimentos_sol]
        
        if ("Group" %in% colnames(data)) {
          idx_grupo  <- match(Food_vec[Alimentos_sol], data$Food)
          Grupo_sex  <- data$Group[idx_grupo]
        } else {
          Grupo_sex <- NA
        }
        
        temp_comp <- data.frame(
          Food       = Food_vec[Alimentos_sol],
          quantity   = cantidades_sol * 100,   # grams
          Demo_Group = Age[i],
          Sex        = as.numeric(sexo_nombre_i),
          Group      = Grupo_sex
        )
        
        Intercambios_Multi <- rbind(Intercambios_Multi, temp_comp)
        
        temp_cost <- data.frame(
          Demo_Group    = Age[i],
          Sex           = as.numeric(sexo_nombre_i),
          cost_day      = cost_day_i,
          Cost_1000kcal = cost_day_i / rhs_i[1] * 1000
        )
        
        Costo_T <- rbind(Costo_T, temp_cost)
        
        # Nutrient adequacy (same as CoNA_paper, only on nutritional constraints)
        n_nutr_rows <- length(names(DRI_min_li))
        Nutrie_limit <- data.frame(
          Nutrients = rownames(Coef.Restriq[1:n_nutr_rows, ] %*%
                                 as.matrix(x_sol)),
          Opt = as.numeric(Coef.Restriq[1:n_nutr_rows, ] %*%
                             as.matrix(x_sol))
        ) %>%
          mutate(
            Rest     = as.vector(unlist(DRI_min_li[i, , drop = FALSE])),
            Diff     = round((Opt - Rest) / Rest * 100, 2),
            Limiting = ifelse(Diff == 0, 1, 0),
            Age      = Age[i],
            Sex      = as.numeric(sexo_nombre_i)
          ) %>%
          filter(Nutrients != "Energy")
        
        N_limit <- rbind(N_limit, Nutrie_limit)
        
        # Shadow prices (only for nutritional constraints, same as CoNA_paper)
        n_constr_nutr <- length(constr_signs)
        Spe <- data.frame(
          Age              = rep(Age[i],           n_constr_nutr),
          Sex              = rep(as.numeric(sexo_nombre_i), n_constr_nutr),
          Nutrients        = names(Limitaciones),
          constraint       = constr_signs,
          value_constraint = rhs_i,
          SP               = sol_multi$duals[1:n_constr_nutr],
          SPE              = (sol_multi$duals[1:n_constr_nutr] *
                                rhs_i / cost_day_i)
        )
        
        S_shadow <- rbind(S_shadow, Spe)
        
        # IPC adherence: actual expenditure shares vs IPC shares
        exp_shares <- sapply(groups_ipc, function(g) {
          in_g <- data$Group == g
          sum(Precio[in_g] * x_sol[in_g]) / cost_day_i
        })
        
        cat(sprintf("  Sex=%s Age=%s | cost=%.1f | IPC MAD=%.4f\n",
                    sexo_nombre_i, Age[i], cost_day_i,
                    mean(abs(exp_shares - IPC_shares$share))))
        
      } # end age loop
      
      assign(paste0("Multi_CoNA_",        sexo_nombre_i), Costo_T)
      assign(paste0("Intercambios_Multi_", sexo_nombre_i), Intercambios_Multi)
      assign(paste0("N_limit_",            sexo_nombre_i), N_limit)
      assign(paste0("S_shadow_",           sexo_nombre_i), S_shadow)
      
    } # end sex loop
    
    #----------------------------------------------------------------------#
    #  Combinar por sexo — dinamico segun los sexos que realmente corrieron #
    #----------------------------------------------------------------------#
    
    if ("Sex" %in% colnames(EER_LL)) {
      Costo_Multi     <- bind_rows(lapply(sexo_nombre, function(s)
        get(paste0("Multi_CoNA_", s))))
      Alimentos_Multi <- bind_rows(lapply(sexo_nombre, function(s)
        get(paste0("Intercambios_Multi_", s))))
      Multi_N_Limit   <- bind_rows(lapply(sexo_nombre, function(s)
        get(paste0("N_limit_", s))))
      Multi_SP        <- bind_rows(lapply(sexo_nombre, function(s)
        get(paste0("S_shadow_", s)))) %>%
        filter(constraint != "=")
      Multi_SP <- Multi_SP[c("Age", "Sex", "Nutrients",
                             "SP", "SPE", "constraint")]
    } else {
      Costo_Multi     <- get(paste0("Multi_CoNA_", sexo_nombre)) %>%
        select(-Sex)
      Alimentos_Multi <- get(paste0("Intercambios_Multi_", sexo_nombre)) %>%
        select(-Sex)
      Multi_N_Limit   <- get(paste0("N_limit_", sexo_nombre)) %>%
        select(-Sex)
      Multi_SP        <- get(paste0("S_shadow_", sexo_nombre)) %>%
        select(-Sex) %>%
        filter(constraint != "=")
      Multi_SP <- Multi_SP[c("Age", "Nutrients", "SP",
                             "SPE", "constraint")]
    }
    
    Alimentos_Multi <- Alimentos_Multi %>%
      select(-where(~ all(is.na(.))))
    
    Multi_SP <- Multi_SP %>%
      mutate(constraint = case_when(
        constraint == ">=" ~ "Min",
        constraint == "<=" ~ "Max",
        TRUE               ~ as.character(constraint)
      ))
    
    list(
      cost    = Costo_Multi,
      comp    = Alimentos_Multi,
      limit   = Multi_N_Limit,
      spe     = Multi_SP,
      lambda  = lam
    )
    
  } # end run_single_lambda
  
  #------------------------------------------------------------------------------------------#
  #                       ETAPA 6: EJECUTAR PARA CADA LAMBDA                               #
  #------------------------------------------------------------------------------------------#
  
  if (length(lambda) == 1) {
    
    result <- run_single_lambda(lambda)
    
    List_out <- list(result$cost,
                     result$comp,
                     result$limit,
                     result$spe,
                     data$Price_100g,
                     data$Food,
                     Req_entrantes,
                     lambda)
    names(List_out) <- c("cost", "comp", "limit", "spe",
                         "p", "x", "constraints", "lambda")
    
    cat("\nMulti_CoNA: Average daily cost per 1000 kcal:",
        mean(result$cost$Cost_1000kcal, na.rm = TRUE), "\n")
    
    return(invisible(List_out))
    
  } else {
    
    # Vector of lambdas: return named list, one element per lambda
    results <- setNames(
      lapply(lambda, run_single_lambda),
      paste0("lambda_", lambda)
    )
    
    cat("\nMulti_CoNA: Pareto frontier estimated for",
        length(lambda), "values of lambda.\n")
    
    return(invisible(results))
  }
}