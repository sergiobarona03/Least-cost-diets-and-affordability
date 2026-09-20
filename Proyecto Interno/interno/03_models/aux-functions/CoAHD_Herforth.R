########################################################
## FUNCION CoAHD_Herforth()
## 03_models/aux-functions/CoAHD_Herforth.R
##
## Costo de una dieta saludable (CoAHD, FAO / Herforth et al.).
## Misma estructura de CoRD_Herforth() (grupos, alimentos por
## grupo, los mas baratos), con dos cambios:
##   1. el precio de cada alimento se expresa por kilocaloria
##      (precio por 100 g comestibles / kcal por 100 g);
##   2. cada grupo se compra por sus kilocalorias, no por
##      intercambios.
##
## En cada grupo se eligen los `Number` alimentos de menor precio
## por kilocaloria y cada uno aporta Kcal_grupo / Number:
##   costo_grupo = sum(precio_kcal_i * Kcal_grupo / Number)
##   costo_dieta = sum(costo_grupo)
##
## Parametros
##   data:    Food, Group, Price_100g, Energy
##            (Price_100g y Energy por 100 g de parte comestible;
##            los alimentos sin energia quedan por fuera)
##   req:     Age, Sex, Group, Kcal (kcal/dia de cada grupo)
##   diverse: Group, Number (alimentos a elegir por grupo)
##   exclude: vector de alimentos a excluir (opcional)
##
## Devuelve: list(cost, comp)
##   cost: costo por miembro (Demo_Group, Sex, cost_day, Cost_1000kcal)
##   comp: alimentos elegidos, con su cantidad y su costo
########################################################

CoAHD_Herforth <- function(data, req, diverse, exclude = NULL) {

  check_cols <- function(x, nombre, cols) {
    if (!is.data.frame(x)) stop(nombre, " is not a data frame.")
    faltan <- setdiff(cols, names(x))
    if (length(faltan) > 0) {
      stop("CoAHD model requires the following columns in '", nombre, "': ",
           paste(faltan, collapse = ", "))
    }
  }

  check_cols(data,    "data",    c("Food", "Group", "Price_100g", "Energy"))
  check_cols(req,     "req",     c("Age", "Sex", "Group", "Kcal"))
  check_cols(diverse, "diverse", c("Group", "Number"))

  sin_diversidad <- setdiff(unique(req$Group), diverse$Group)
  if (length(sin_diversidad) > 0) {
    stop("Groups in 'req' without an entry in 'diverse': ",
         paste(sin_diversidad, collapse = ", "))
  }

  data <- data %>%
    dplyr::filter(!Food %in% exclude, !is.na(Price_100g), !is.na(Energy), Energy > 0) %>%
    dplyr::mutate(Price_kcal = Price_100g / Energy)

  elegidos <- data %>%
    dplyr::inner_join(diverse[, c("Group", "Number")], by = "Group") %>%
    dplyr::arrange(Group, Price_kcal) %>%
    dplyr::group_by(Group) %>%
    dplyr::filter(dplyr::row_number() <= Number) %>%
    dplyr::ungroup()

  incompletos <- diverse %>%
    dplyr::filter(Group %in% req$Group) %>%
    dplyr::left_join(dplyr::count(elegidos, Group, name = "n"), by = "Group") %>%
    dplyr::filter(is.na(n) | n < Number)

  if (nrow(incompletos) > 0) {
    stop("Not enough foods with energy in group(s): ",
         paste(incompletos$Group, collapse = ", "))
  }

  comp <- req %>%
    dplyr::inner_join(elegidos, by = "Group", relationship = "many-to-many") %>%
    dplyr::mutate(
      Kcal_food  = Kcal / Number,
      Cantidad_g = Kcal_food / Energy * 100,
      Cost       = Price_kcal * Kcal_food
    )

  cost <- comp %>%
    dplyr::group_by(Demo_Group = Age, Sex) %>%
    dplyr::summarise(
      cost_day      = sum(Cost),
      Cost_1000kcal = sum(Cost) / sum(Kcal_food) * 1000,
      .groups = "drop"
    )

  list(
    cost = cost,
    comp = comp %>%
      dplyr::transmute(Food, Group, Demo_Group = Age, Sex,
                       Kcal = Kcal_food, Cantidad_g, Price_kcal, Cost)
  )
}
