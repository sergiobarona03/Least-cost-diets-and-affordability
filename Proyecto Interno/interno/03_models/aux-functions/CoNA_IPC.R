########################################################
## FUNCION CoNA_IPC()
## 03_models/aux-functions/CoNA_IPC.R
##
## CoNA restringido a la composicion del IPC (CC-CoNA). Es el CoNA
## (misma funcion objetivo y mismas restricciones de energia y
## nutrientes que CoNA_paper()) mas una restriccion por subclase g del
## IPC: la subclase debe pesar al menos alpha * s_g de las cantidades
## de la dieta,
##
##   sum_{i en g} x_i >= alpha * s_g * sum_i x_i
##
## donde x_i son los gramos del alimento i y s_g la participacion de la
## subclase g. Con alpha = 0 el problema es el CoNA; con alpha = 1 la
## dieta reproduce las participaciones del IPC.
##
## Solo se restringen las subclases que tienen algun alimento con
## precio en esa ciudad y fecha, y sus participaciones se reescalan
## para que sumen 1.
##
## Parametros
##   data:       Food, Group (subclase), Price_100g, Energy y los
##               nutrientes de CoNA_paper()
##   EER_LL, UL: requerimientos minimos y maximos (Age, Sex, Energy, ...)
##   IPC_shares: Group, share
##   alpha:      entre 0 y 1
##
## Devuelve: list(cost, comp, shares)
##   cost:   Demo_Group, Sex, cost_day, Cost_1000kcal
##   comp:   Food, Group, quantity (g), Demo_Group, Sex
##   shares: participacion objetivo y observada de cada subclase
########################################################

CoNA_IPC <- function(data, EER_LL, UL, IPC_shares, alpha = 1) {

  stopifnot(is.numeric(alpha), alpha >= 0, alpha <= 1)
  stopifnot(all(c("Food", "Group", "Price_100g", "Energy") %in% names(data)))
  stopifnot(abs(sum(IPC_shares$share) - 1) < 1e-6)

  presentes <- IPC_shares %>%
    dplyr::filter(Group %in% unique(data$Group)) %>%
    dplyr::mutate(share = share / sum(share))

  # Alimentos cuya subclase no esta en IPC_shares no entran a ninguna restriccion
  # de subclase, pero si a las de nutrientes.
  Precio <- data$Price_100g
  Food   <- data$Food
  n_food <- nrow(data)

  A_ipc <- matrix(0, nrow = nrow(presentes), ncol = n_food)
  for (j in seq_len(nrow(presentes))) {
    en_g <- data$Group == presentes$Group[j]
    s_g  <- presentes$share[j]
    A_ipc[j, ]     <- -alpha * s_g
    A_ipc[j, en_g] <- 1 - alpha * s_g
  }
  rhs_ipc <- rep(0, nrow(presentes))
  dir_ipc <- rep(">=", nrow(presentes))

  Sexos_min <- split(EER_LL, EER_LL$Sex)
  Sexos_max <- split(UL, UL$Sex)
  if (!identical(names(Sexos_min), names(Sexos_max))) {
    stop("The genders in EER_LL and UL are not the same.")
  }

  costos <- list(); comps <- list(); cuotas <- list()

  for (sexo in names(Sexos_min)) {

    DRI_min_i <- dplyr::arrange(Sexos_min[[sexo]], Age)
    DRI_max_i <- Sexos_max[[sexo]]
    DRI_max_i[is.na(DRI_max_i)] <- 999999
    DRI_max_i <- dplyr::arrange(DRI_max_i, Age)

    if (!identical(levels(as.factor(DRI_min_i$Age)), levels(as.factor(DRI_max_i$Age)))) {
      stop("The age groups in EER_LL and UL are not the same.")
    }

    Age <- DRI_min_i$Age

    DRI_min_li <- DRI_min_i %>% dplyr::select(-dplyr::any_of(c("Age", "Sex")))
    DRI_max_li <- DRI_max_i %>% dplyr::select(-dplyr::any_of(c("Age", "Energy", "Sex")))

    nutrientes <- data %>%
      dplyr::select(-dplyr::any_of(c("Cod_TCAC", "Food", "Serving", "Price_100g", "Group")))
    comunes <- intersect(names(nutrientes), names(DRI_min_li))

    nutrientes <- nutrientes %>% dplyr::select(dplyr::any_of(comunes))
    DRI_min_li <- DRI_min_li %>% dplyr::select(dplyr::any_of(comunes))
    DRI_max_li <- DRI_max_li %>% dplyr::select(dplyr::any_of(comunes))

    nutrientes <- cbind(nutrientes, nutrientes %>% dplyr::select(-Energy))
    Coef       <- t(as.matrix(nutrientes))
    signos     <- c("=", rep(">=", ncol(DRI_min_li) - 1), rep("<=", ncol(DRI_max_li)))
    Limites    <- cbind(DRI_min_li, DRI_max_li)

    resolver <- function(rhs_nutr) {
      lpSolve::lp(direction = "min", objective.in = Precio,
                  const.mat = rbind(Coef, A_ipc),
                  const.dir = c(signos, dir_ipc),
                  const.rhs = c(rhs_nutr, rhs_ipc))
    }

    for (i in seq_along(Age)) {

      rhs_i <- as.vector(unlist(Limites[i, , drop = FALSE]))
      sol   <- resolver(rhs_i)

      # Sin solucion: se relaja un requerimiento minimo a la vez, de 99% a 91%,
      # como en CoNA_paper()
      if (sol$status != 0 || sum(sol$solution) == 0) {
        encontrada <- FALSE
        porcentaje <- 0.99
        while (!encontrada && porcentaje > 0.9) {
          min_sin_energia <- DRI_min_li %>% dplyr::select(-Energy)
          for (j in seq_len(ncol(min_sin_energia))) {
            temp <- min_sin_energia
            temp[i, j] <- temp[i, j] * porcentaje
            lim_temp <- cbind(DRI_min_li %>% dplyr::select(Energy), temp, DRI_max_li)
            rhs_temp <- as.vector(unlist(lim_temp[i, , drop = FALSE]))
            intento  <- resolver(rhs_temp)
            if (intento$status == 0 && sum(intento$solution) != 0) {
              sol <- intento; rhs_i <- rhs_temp; encontrada <- TRUE
              break
            }
          }
          if (!encontrada) porcentaje <- porcentaje - 0.01
        }
        if (!encontrada) {
          costos[[length(costos) + 1]] <- data.frame(
            Demo_Group = Age[i], Sex = as.numeric(sexo),
            cost_day = NA_real_, Cost_1000kcal = NA_real_)
          next
        }
      }

      x <- sol$solution
      costos[[length(costos) + 1]] <- data.frame(
        Demo_Group = Age[i], Sex = as.numeric(sexo),
        cost_day = sum(x * Precio), Cost_1000kcal = sum(x * Precio) / rhs_i[1] * 1000)

      usados <- which(x > 1e-8)
      comps[[length(comps) + 1]] <- data.frame(
        Food = Food[usados], Group = data$Group[usados], quantity = x[usados] * 100,
        Demo_Group = Age[i], Sex = as.numeric(sexo))

      total <- sum(x)
      cuotas[[length(cuotas) + 1]] <- data.frame(
        Demo_Group = Age[i], Sex = as.numeric(sexo), Group = presentes$Group,
        share_objetivo = alpha * presentes$share,
        share_observada = vapply(presentes$Group, function(g) sum(x[data$Group == g]) / total, numeric(1)))
    }
  }

  list(cost = dplyr::bind_rows(costos), comp = dplyr::bind_rows(comps),
       shares = dplyr::bind_rows(cuotas))
}
