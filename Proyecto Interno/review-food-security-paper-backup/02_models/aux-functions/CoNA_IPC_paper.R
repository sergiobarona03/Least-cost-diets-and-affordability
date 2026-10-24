

CoNA_IPC_paper <- function(data,
                           EER_LL,
                           UL,
                           IPC_shares,
                           alpha   = 0.5,
                           exclude = NULL) {
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
  
  validate_columns(data,       c("Price_100g", "Food", "Energy", "Group"), "data")
  validate_columns(EER_LL,     c("Age", "Energy"),                          "EER_LL")
  validate_columns(UL,         c("Age"),                                    "UL")
  validate_columns(IPC_shares, c("Group", "share"),                         "IPC_shares")
  
  if (abs(sum(IPC_shares$share) - 1) > 1e-6)
    stop("IPC_shares$share must sum to 1.")
  
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1)
    stop("alpha must be a numeric value in [0, 1].")
  
  missing_groups <- setdiff(IPC_shares$Group, unique(data$Group))
  if (length(missing_groups) > 0)
    warning("The following groups in IPC_shares are not in data$Group: ",
            paste(missing_groups, collapse = ", "))
  
  if (!is.null(exclude)) {
    if (!is.vector(exclude))
      stop("Error: The 'exclude' parameter must be a vector.")
    data <- data[!(data$Food %in% exclude), ]
  }
  
  #------------------------------------------------------------------------------------------#
  #                       ETAPA 3: PREPARACIÓN COMÚN                                       #
  #------------------------------------------------------------------------------------------#
  
  req_min_ent   <- EER_LL
  req_max_ent   <- UL %>% select(-any_of(c("Age", "Energy")))
  Req_entrantes <- cbind(req_min_ent, req_max_ent)
  
  groups_ipc   <- IPC_shares$Group
  n_groups     <- length(groups_ipc)
  n_foods      <- nrow(data)
  Precio       <- data$Price_100g
  
  if ("Sex" %in% colnames(EER_LL)) {
    Sexos_min   <- split(EER_LL, EER_LL$Sex)
    Sexos_max   <- split(UL,     UL$Sex)
    if (!identical(names(Sexos_min), names(Sexos_max)))
      stop("Error: The genders in both requirements are not the same.")
    sexo_nombre <- names(Sexos_min)
  } else {
    sexo_nombre <- "0"
    DRI_min_i   <- EER_LL
    DRI_max_i   <- UL; DRI_max_i[is.na(DRI_max_i)] <- 999999
  }
  
  #------------------------------------------------------------------------------------------#
  #        ETAPA 4: CONSTRUCCIÓN DE LA MATRIZ IPC (PARTICIPACIONES CALÓRICAS)               #
  #                                                                                          #
  # Restricción por grupo g:                                                                 #
  #   sum_{i in g}(a_0i * x_i) >= alpha * s_g * r_E                                        #
  #                                                                                          #
  # Matriz A_ipc (G × n):                                                                    #
  #   A_ipc[g, i] = a_0i   si i pertenece al grupo g                                        #
  #   A_ipc[g, i] = 0      si no                                                             #
  #                                                                                          #
  # El RHS = alpha * s_g * r_E se computa dentro del loop de edad                           #
  # porque r_E varía por grupo demográfico.                                                  #
  # Esta matriz se construye una sola vez porque no depende de edad ni sexo.                 #
  #------------------------------------------------------------------------------------------#
  
  A_ipc   <- matrix(0, nrow = n_groups, ncol = n_foods)
  rhs_ipc <- rep(0, n_groups)
  dir_ipc <- rep(">=", n_groups)
  
  for (j in seq_len(n_groups)) {
    g        <- groups_ipc[j]
    s_g      <- IPC_shares$share[IPC_shares$Group == g]
    in_group <- data$Group == g
    # Alimentos dentro del grupo g: coeficiente = (1 - alpha*s_g)
    A_ipc[j,  which(in_group)] <-  (1 - alpha * s_g)
    # Alimentos fuera del grupo g: coeficiente = -alpha*s_g
    A_ipc[j, !in_group]        <- -alpha * s_g
    
    cat(sprintf("IPC quantity share constraints: alpha = %.2f\n", alpha))
    cat(sprintf("Each group must represent at least %.0f%% of its IPC quantity share\n",
                alpha * 100))
    
    #------------------------------------------------------------------------------------------#
    #                       ETAPA 5: LOOP POR SEXO Y EDAD                                    #
    #------------------------------------------------------------------------------------------#
    
    # Colectores — misma estructura que CoNA_paper
    Intercambios_IPC <- data.frame(Food = character(), quantity = numeric(),
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
      
      Food_vec <- data$Food
      Age      <- DRI_min_i$Age
      
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
      
      Coef.Restriq      <- DF_Nutrientes_Alimentos %>% as.matrix() %>% t()
      constr_signs_nutr <- c("=", rep(">=", ncol(DRI_min_li) - 1),
                             rep("<=", ncol(DRI_max_li)))
      Limitaciones      <- cbind(DRI_min_li, DRI_max_li)
      
      for (i in seq_along(Age)) {
        
        rhs_i <- as.vector(unlist(Limitaciones[i, , drop = FALSE]))
        
        # RHS de restricciones IPC: siempre cero
        # La restricción sum_i[(1[i in g] - alpha*s_g)*x_i] >= 0
        # no depende de la edad ni del sexo
        
        # ---- Ensamblar: restricciones nutricionales + restricciones IPC
        A_full   <- rbind(Coef.Restriq, A_ipc)
        dir_full <- c(constr_signs_nutr, dir_ipc)
        rhs_full <- c(rhs_i,            rhs_ipc)
        
        # ---- Resolver
        sol <- lp(
          direction    = "min",
          objective.in = Precio,
          const.mat    = A_full,
          const.dir    = dir_full,
          const.rhs    = rhs_full,
          compute.sens = TRUE
        )
        
        # ---- Validación: relajar restricciones nutricionales si es infactible
        # (misma lógica que CoNA_paper)
        if (sol$status != 0 || sum(sol$solution) == 0) {
          
          solucion_encontrada <- FALSE
          porcentaje          <- 0.99
          nutrienteid         <- NA
          
          while (!solucion_encontrada && porcentaje > 0.9) {
            
            DRI_min_li_temp <- DRI_min_li %>% select(-Energy)
            
            for (j in seq_len(ncol(DRI_min_li_temp))) {
              DRI_temp       <- DRI_min_li_temp
              DRI_temp[i, j] <- DRI_temp[i, j] * porcentaje
              Lim_temp       <- cbind(DRI_min_li %>% select(Energy),
                                      DRI_temp, DRI_max_li)
              rhs_temp       <- as.vector(unlist(Lim_temp[i, , drop = FALSE]))
              
              # Recompute rhs_ipc with (possibly relaxed) energy requirement
              r_E_temp     <- rhs_temp[1]
              rhs_ipc_temp <- alpha * IPC_shares$share * r_E_temp
              
              sol_try <- lp(
                direction    = "min",
                objective.in = Precio,
                const.mat    = rbind(Coef.Restriq, A_ipc),
                const.dir    = c(constr_signs_nutr, dir_ipc),
                const.rhs    = c(rhs_temp, rhs_ipc_temp),
                compute.sens = TRUE
              )
              
              if (sol_try$status == 0 && sum(sol_try$solution) != 0) {
                nutrienteid         <- names(DRI_min_li_temp)[j]
                sol                 <- sol_try
                solucion_encontrada <- TRUE
                rhs_i               <- rhs_temp
                break
              }
            }
            
            if (!solucion_encontrada) porcentaje <- porcentaje - 0.01
          }
          
          if (!solucion_encontrada) {
            warning("CoNA_IPC: no feasible solution for Sex=", sexo_nombre_i,
                    " Age=", Age[i])
            next
          } else {
            print(paste0(
              "CoNA_IPC for Sex=", sexo_nombre_i, " Age=", Age[i],
              " estimated at ", round(porcentaje * 100, 1),
              "% of minimum required intake of ", nutrienteid
            ))
          }
        }
        
        # ---- Extraer resultados
        x_sol      <- sol$solution
        cost_day_i <- sum(x_sol * Precio)
        
        # Verificar participaciones de cantidad reales vs objetivo IPC
        qty_shares_obs <- sapply(groups_ipc, function(g) {
          in_g <- data$Group == g
          total_qty      <- sum(x_sol)
          if (total_qty > 0) sum(x_sol[in_g]) / total_qty else 0
        })
        
        cat(sprintf("  Sex=%s Age=%s | cost=%.1f | quantity share MAD=%.4f\n",
                    sexo_nombre_i, Age[i], cost_day_i,
                    mean(abs(qty_shares_obs - IPC_shares$share))))
        
        # Composición
        Alimentos_sol  <- which(x_sol > 1e-8)
        cantidades_sol <- x_sol[Alimentos_sol]
        
        if ("Group" %in% colnames(data)) {
          idx_grupo <- match(Food_vec[Alimentos_sol], data$Food)
          Grupo_sol <- data$Group[idx_grupo]
        } else {
          Grupo_sol <- NA
        }
        
        temp_comp <- data.frame(
          Food       = Food_vec[Alimentos_sol],
          quantity   = cantidades_sol * 100,
          Demo_Group = Age[i],
          Sex        = as.numeric(sexo_nombre_i),
          Group      = Grupo_sol
        )
        
        Intercambios_IPC <- rbind(Intercambios_IPC, temp_comp)
        
        # Costo
        temp_cost <- data.frame(
          Demo_Group    = Age[i],
          Sex           = as.numeric(sexo_nombre_i),
          cost_day      = cost_day_i,
          Cost_1000kcal = cost_day_i / rhs_i[1] * 1000
        )
        
        Costo_T <- rbind(Costo_T, temp_cost)
        
        # Nutrientes limitantes
        n_nutr_rows  <- length(names(DRI_min_li))
        Nutrie_limit <- data.frame(
          Nutrients = rownames(Coef.Restriq[1:n_nutr_rows, ] %*% as.matrix(x_sol)),
          Opt       = as.numeric(Coef.Restriq[1:n_nutr_rows, ] %*% as.matrix(x_sol))
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
        
        # Precios sombra
        n_constr_nutr <- length(constr_signs_nutr)
        Spe <- data.frame(
          Age              = rep(Age[i],                    n_constr_nutr),
          Sex              = rep(as.numeric(sexo_nombre_i), n_constr_nutr),
          Nutrients        = names(Limitaciones),
          constraint       = constr_signs_nutr,
          value_constraint = rhs_i,
          SP               = sol$duals[1:n_constr_nutr],
          SPE              = sol$duals[1:n_constr_nutr] * rhs_i / cost_day_i
        ) %>%
          mutate(constraint = case_when(
            constraint == ">=" ~ "Min",
            constraint == "<=" ~ "Max",
            TRUE               ~ as.character(constraint)
          ))
        
        S_shadow <- rbind(S_shadow, Spe)
        
      } # end age loop
      
      assign(paste0("CoNA_IPC_",         sexo_nombre_i), Costo_T)
      assign(paste0("Intercambios_IPC_",  sexo_nombre_i), Intercambios_IPC)
      assign(paste0("N_limit_IPC_",       sexo_nombre_i), N_limit)
      assign(paste0("S_shadow_IPC_",      sexo_nombre_i), S_shadow)
      
    } # end sex loop
    
    #------------------------------------------------------------------------------------------#
    #                       ETAPA 6: COMBINAR POR SEXO Y RETORNAR                            #
    #------------------------------------------------------------------------------------------#
    
    nombres_comunes_sin_energia <- setdiff(nombres_comunes, "Energy")
    cat("\nNutrients used in the model:",
        paste(nombres_comunes_sin_energia, collapse = ", "), "\n")
    
    if ("Sex" %in% colnames(EER_LL)) {
      Costo_Final     <- bind_rows(lapply(sexo_nombre, function(s)
        get(paste0("CoNA_IPC_", s))))
      Alimentos_Final <- bind_rows(lapply(sexo_nombre, function(s)
        get(paste0("Intercambios_IPC_", s))))
      Limit_Final     <- bind_rows(lapply(sexo_nombre, function(s)
        get(paste0("N_limit_IPC_", s))))
      SP_Final        <- bind_rows(lapply(sexo_nombre, function(s)
        get(paste0("S_shadow_IPC_", s)))) %>%
        filter(constraint != "=")
    } else {
      Costo_Final     <- get(paste0("CoNA_IPC_",        sexo_nombre)) %>% select(-Sex)
      Alimentos_Final <- get(paste0("Intercambios_IPC_", sexo_nombre)) %>% select(-Sex)
      Limit_Final     <- get(paste0("N_limit_IPC_",      sexo_nombre)) %>% select(-Sex)
      SP_Final        <- get(paste0("S_shadow_IPC_",     sexo_nombre)) %>%
        select(-Sex) %>% filter(constraint != "=")
    }
    
    Alimentos_Final <- Alimentos_Final %>% select(-where(~ all(is.na(.))))
    
    cat("CoNA_IPC: Average daily cost per 1000 kcal:",
        mean(Costo_Final$Cost_1000kcal, na.rm = TRUE), "\n")
    
    List_out <- list(
      cost        = Costo_Final,
      comp        = Alimentos_Final,
      limit       = Limit_Final,
      spe         = SP_Final,
      p           = data$Price_100g,
      x           = data$Food,
      constraints = Req_entrantes
    )
    
    return(invisible(List_out))
  }}