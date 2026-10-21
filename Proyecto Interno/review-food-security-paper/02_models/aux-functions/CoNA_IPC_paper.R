########################################################################
## CoNA_IPC_paper — Consumption-Consistent Cost of Nutrient Adequacy
##
## FIX (2026-10-20). In the previous version the `for (j in seq_len(
## n_groups))` loop that fills the IPC constraint matrix opened at the
## top of Stage 4 and was never closed: the collectors, the sex loop,
## the age loop, the LP solve, the by-sex combination and the `return`
## were all nested inside it, and the file ended with `}}`. Three
## consequences:
##
##   1. The function returned during the FIRST iteration of j, so A_ipc
##      was used with only its first row filled. With ">=" and a zero
##      RHS the empty rows are satisfied trivially, so the composition
##      constraint was never actually enforced — not even at alpha = 1.
##   2. The collectors were declared inside the j loop but before the
##      sex loop, so they were not reset between sexes. The first sex
##      processed ended up duplicated in every output (observed as the
##      adult male appearing twice in all 1,080 city x date x alpha
##      cells).
##   3. The infeasibility fallback rebuilt the RHS as
##      alpha * share * r_E while A_ipc already carries the alpha * s_g
##      weighting, applying the restriction twice.
##
## The j loop is now closed as soon as A_ipc is built; everything else
## sits at its own level; the collectors are declared inside the sex
## loop; and the fallback keeps the zero RHS that matches the matrix.
##
## SPECIFICATION CHANGE (2026-10-20). The constraint now operates on
## EXPENDITURE shares rather than quantity shares:
##   sum_{i in g} p_i x_i >= alpha * s_g * sum_j p_j x_j
## IPC_shares$share comes from `total_ingresos`, i.e. expenditure, so
## imposing it on grams mixed dimensions and made the restriction bind
## far harder than intended. Once the loop bug above was fixed and the
## constraint was actually enforced, the quantity-share version gave a
## premium of 34% at alpha = 0.25, 69% at 0.50 and 137% at 0.75, with
## alpha = 1 infeasible in all 216 city x month cells. The expenditure
## formulation puts the constraint in the same units as the CPI weights
## and as the objective, and keeps the problem linear.
##
## Methods (3.2.2) and Appendix B must be updated to describe the
## constraint over expenditure shares.
########################################################################

CoNA_IPC_paper <- function(data,
                           EER_LL,
                           UL,
                           IPC_shares,
                           alpha   = 0.5,
                           exclude = NULL,
                           verbose = FALSE) {
  
  #--------------------------------------------------------------------#
  #                       STAGE 1: LIBRARIES                           #
  #--------------------------------------------------------------------#
  
  Librerias_base <- c("tidyverse", "rio", "janitor", "stringdist",
                      "lpSolve", "knitr")
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(char = Librerias_base)
  
  paquetes_faltantes <- Librerias_base[!(Librerias_base %in% pacman::p_loaded())]
  for (paquete in paquetes_faltantes) {
    if (!require(paquete, character.only = TRUE)) {
      install.packages(paquete)
      library(paquete, character.only = TRUE)
    }
  }
  
  #--------------------------------------------------------------------#
  #                       STAGE 2: VALIDATION                          #
  #--------------------------------------------------------------------#
  
  validate_columns <- function(df, required_columns, model_name) {
    if (!is.data.frame(df))
      stop(paste("Error:", model_name, "is not a data frame."))
    missing_columns <- setdiff(required_columns, colnames(df))
    if (length(missing_columns) > 0)
      stop(paste(model_name, "requires the following columns:",
                 paste(missing_columns, collapse = ", ")))
  }
  
  validate_columns(data,       c("Price_100g", "Food", "Energy", "Group"), "data")
  validate_columns(EER_LL,     c("Age", "Energy"),                        "EER_LL")
  validate_columns(UL,         c("Age"),                                  "UL")
  validate_columns(IPC_shares, c("Group", "share"),                       "IPC_shares")
  
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
  
  #--------------------------------------------------------------------#
  #                       STAGE 3: COMMON SETUP                        #
  #--------------------------------------------------------------------#
  
  req_min_ent   <- EER_LL
  req_max_ent   <- UL %>% select(-any_of(c("Age", "Energy")))
  Req_entrantes <- cbind(req_min_ent, req_max_ent)
  
  groups_ipc <- IPC_shares$Group
  n_groups   <- length(groups_ipc)
  n_foods    <- nrow(data)
  Precio     <- data$Price_100g
  
  if ("Sex" %in% colnames(EER_LL)) {
    Sexos_min <- split(EER_LL, EER_LL$Sex)
    Sexos_max <- split(UL,     UL$Sex)
    if (!identical(names(Sexos_min), names(Sexos_max)))
      stop("Error: The genders in both requirements are not the same.")
    sexo_nombre <- names(Sexos_min)
  } else {
    sexo_nombre <- "0"
    DRI_min_i   <- EER_LL
    DRI_max_i   <- UL; DRI_max_i[is.na(DRI_max_i)] <- 999999
  }
  
  #--------------------------------------------------------------------#
  #       STAGE 4: IPC CONSTRAINT MATRIX (EXPENDITURE SHARES)           #
  #                                                                    #
  # Constraint for group g, on EXPENDITURE:                            #
  #   sum_{i in g} p_i x_i  >=  alpha * s_g * sum_j p_j x_j           #
  # which rearranges to                                                #
  #   sum_i [ (1[i in g] - alpha * s_g) * p_i * x_i ] >= 0            #
  #                                                                    #
  # so each coefficient is the quantity-share coefficient multiplied   #
  # by that food's price. The alpha * s_g weighting lives in the       #
  # COEFFICIENTS, so the RHS is zero. The matrix does not depend on    #
  # age or sex, so it is built once here; this loop fills A_ipc and    #
  # nothing else.                                                      #
  #                                                                    #
  # CHANGE (2026-10-20): the constraint was previously imposed on       #
  # QUANTITY shares while IPC_shares$share comes from `total_ingresos`, #
  # i.e. EXPENDITURE shares — the dimensional mismatch flagged between  #
  # Methods (3.2.2) and Appendix B. Forcing expenditure shares onto     #
  # grams made the restriction bind far harder than intended: once it   #
  # was actually enforced the premium reached 34% at alpha = 0.25 and   #
  # 137% at alpha = 0.75, and alpha = 1 was infeasible in all 216       #
  # city x month cells. Weighting by p_i puts the constraint in the     #
  # same units as the CPI weights and as the objective function.        #
  #--------------------------------------------------------------------#
  
  A_ipc   <- matrix(0, nrow = n_groups, ncol = n_foods)
  rhs_ipc <- rep(0, n_groups)
  dir_ipc <- rep(">=", n_groups)
  
  for (j in seq_len(n_groups)) {
    g        <- groups_ipc[j]
    s_g      <- IPC_shares$share[IPC_shares$Group == g]
    in_group <- data$Group == g
    in_group[is.na(in_group)] <- FALSE
    A_ipc[j,  which(in_group)] <-  (1 - alpha * s_g) * Precio[which(in_group)]
    A_ipc[j, which(!in_group)] <- -alpha * s_g       * Precio[which(!in_group)]
  }   # <- the j loop ends HERE; everything below is outside it
  
  stopifnot(nrow(A_ipc) == n_groups, ncol(A_ipc) == n_foods)
  
  # Scaling: the IPC rows are in monetary units while the nutrient rows
  # are in nutrient units, which can leave the LP badly conditioned.
  # Dividing each IPC row by the mean price leaves the constraint
  # unchanged, since its RHS is zero.
  p_scale <- mean(Precio, na.rm = TRUE)
  if (is.finite(p_scale) && p_scale > 0) A_ipc <- A_ipc / p_scale
  
  if (verbose) {
    cat(sprintf("IPC expenditure share constraints: alpha = %.2f\n", alpha))
    cat(sprintf("Each group must hold at least %.0f%% of its IPC expenditure share\n",
                alpha * 100))
  }
  
  #--------------------------------------------------------------------#
  #                    STAGE 5: SEX AND AGE LOOPS                      #
  #--------------------------------------------------------------------#
  
  for (sexo_nombre_i in sexo_nombre) {
    
    # Collectors are declared INSIDE the sex loop so that they reset on
    # every iteration. Declaring them outside is what duplicated the
    # first sex processed in the previous version.
    Intercambios_IPC <- data.frame(Food = character(), quantity = numeric(),
                                   Demo_Group = character(), Sex = integer(),
                                   Group = character())
    Costo_T  <- data.frame(Demo_Group = character(), Sex = integer(),
                           cost_day = numeric(), Cost_1000kcal = numeric())
    N_limit  <- data.frame()
    S_shadow <- na.omit(data.frame(Age = NA, Sex = NA, Nutrients = NA,
                                   constraint = NA, value_constraint = NA,
                                   SP = NA, SPE = NA))
    
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
      
      A_full   <- rbind(Coef.Restriq, A_ipc)
      dir_full <- c(constr_signs_nutr, dir_ipc)
      rhs_full <- c(rhs_i,             rhs_ipc)
      
      sol <- lp(
        direction    = "min",
        objective.in = Precio,
        const.mat    = A_full,
        const.dir    = dir_full,
        const.rhs    = rhs_full,
        compute.sens = TRUE
      )
      
      # ---- Fallback: relax nutrient minimums if infeasible.
      # The IPC rows keep their zero RHS, since the alpha * s_g
      # weighting is already inside A_ipc. The previous version
      # rebuilt the RHS as alpha * share * r_E, applying it twice.
      if (sol$status != 0 || sum(sol$solution) == 0) {
        
        solucion_encontrada <- FALSE
        porcentaje          <- 0.99
        nutrienteid         <- NA
        
        while (!solucion_encontrada && porcentaje > 0.9) {
          
          DRI_min_li_temp <- DRI_min_li %>% select(-Energy)
          
          for (jj in seq_len(ncol(DRI_min_li_temp))) {
            DRI_temp        <- DRI_min_li_temp
            DRI_temp[i, jj] <- DRI_temp[i, jj] * porcentaje
            Lim_temp        <- cbind(DRI_min_li %>% select(Energy),
                                     DRI_temp, DRI_max_li)
            rhs_temp        <- as.vector(unlist(Lim_temp[i, , drop = FALSE]))
            
            sol_try <- lp(
              direction    = "min",
              objective.in = Precio,
              const.mat    = A_full,
              const.dir    = dir_full,
              const.rhs    = c(rhs_temp, rhs_ipc),
              compute.sens = TRUE
            )
            
            if (sol_try$status == 0 && sum(sol_try$solution) != 0) {
              nutrienteid         <- names(DRI_min_li_temp)[jj]
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
                  " Age=", Age[i], " alpha=", alpha)
          next
        } else if (verbose) {
          message(sprintf(
            "CoNA_IPC Sex=%s Age=%s estimated at %.1f%% of the minimum intake of %s",
            sexo_nombre_i, Age[i], porcentaje * 100, nutrienteid))
        }
      }
      
      # ---- Results
      x_sol      <- sol$solution
      cost_day_i <- sum(x_sol * Precio)
      
      if (verbose) {
        exp_shares_obs <- sapply(groups_ipc, function(g) {
          in_g      <- data$Group == g
          in_g[is.na(in_g)] <- FALSE
          total_exp <- sum(x_sol * Precio)
          if (total_exp > 0) sum(x_sol[in_g] * Precio[in_g]) / total_exp else 0
        })
        cat(sprintf("  Sex=%s Age=%s | cost=%.1f | expenditure share MAD=%.4f\n",
                    sexo_nombre_i, Age[i], cost_day_i,
                    mean(abs(exp_shares_obs - IPC_shares$share))))
      }
      
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
      
      temp_cost <- data.frame(
        Demo_Group    = Age[i],
        Sex           = as.numeric(sexo_nombre_i),
        cost_day      = cost_day_i,
        Cost_1000kcal = cost_day_i / rhs_i[1] * 1000
      )
      
      Costo_T <- rbind(Costo_T, temp_cost)
      
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
      
    }   # end age loop
    
    assign(paste0("CoNA_IPC_",          sexo_nombre_i), Costo_T)
    assign(paste0("Intercambios_IPC_",  sexo_nombre_i), Intercambios_IPC)
    assign(paste0("N_limit_IPC_",       sexo_nombre_i), N_limit)
    assign(paste0("S_shadow_IPC_",      sexo_nombre_i), S_shadow)
    
  }   # end sex loop
  
  #--------------------------------------------------------------------#
  #                 STAGE 6: COMBINE BY SEX AND RETURN                 #
  #--------------------------------------------------------------------#
  
  nombres_comunes_sin_energia <- setdiff(nombres_comunes, "Energy")
  if (verbose)
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
    Costo_Final     <- get(paste0("CoNA_IPC_",         sexo_nombre)) %>% select(-Sex)
    Alimentos_Final <- get(paste0("Intercambios_IPC_", sexo_nombre)) %>% select(-Sex)
    Limit_Final     <- get(paste0("N_limit_IPC_",      sexo_nombre)) %>% select(-Sex)
    SP_Final        <- get(paste0("S_shadow_IPC_",     sexo_nombre)) %>%
      select(-Sex) %>% filter(constraint != "=")
  }
  
  Alimentos_Final <- Alimentos_Final %>% select(-where(~ all(is.na(.))))
  
  # Guard against the duplication the previous version produced
  n_expected <- length(sexo_nombre) * length(Age)
  if (nrow(Costo_Final) != n_expected)
    warning("CoNA_IPC: cost has ", nrow(Costo_Final),
            " rows, expected ", n_expected,
            ". Check the collectors inside the sex loop.")
  
  if (verbose)
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
}