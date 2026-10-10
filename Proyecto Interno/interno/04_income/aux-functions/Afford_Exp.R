########################################################
## FUNCION Afford_Exp()
## 04_income/aux-functions/Afford_Exp.R
##
## Asequibilidad de la dieta por decil de ingreso. Un hogar no puede pagar
## la dieta si su gasto per capita mensual (en alimentos, o su ingreso
## total en el contrafactual) es menor que el costo per capita mensual de
## la dieta. Con los factores de expansion se calculan los indicadores FGT:
##
##   FGT0 (rate):     proporcion de la poblacion que vive en hogares que no
##                    pueden pagarla (%)
##   FGT1 (gap):      brecha relativa media, (z - y) / z, sobre toda la poblacion
##   FGT2 (severity): brecha relativa al cuadrado, sobre toda la poblacion
##
## Version generalizada de Afford_Exp() de review-food-security-paper:
## recibe cualquier numero de dietas (CoCA, CoNA, CC-CoNA, CoRD, CoAHD) y permite
## que el umbral dependa del tamano del hogar.
##
## Parametros
##   Hexpense: una persona por fila, con los datos de su hogar y su factor
##             de expansion (como en el paper), con
##             deciles (Decil 1 ... Decil 10), fex_c18 (factor de expansion),
##             ung (personas del hogar), food_exp_per_capita (gasto en
##             alimentos per capita al mes), per_capita_income (ingreso
##             per capita al mes)
##   umbrales: model, per_capita (costo per capita de la dieta, COP/dia) y,
##             si el umbral depende del tamano, tamano (3, 4 o 5): los
##             hogares con menos de 3 personas usan el 3 y los de 5 o mas
##             usan el 5
##   gasto:    "alimentos" (food_exp_per_capita) o "ingreso" (contrafactual:
##             todo el ingreso per capita se dedica a alimentos)
##   agrupar:  "deciles" (indicadores por decil de ingreso) o "tamano"
##             (por tamano del hogar: 3 o menos, 4, 5 o mas)
##   dias:     dias del mes con que se pasa el costo diario a mensual
##
## Devuelve: list(indicadores, medias)
##   indicadores: grupo, model, rate, gap, severity, umbral_medio
##   medias:      grupo, gasto per capita medio ponderado, personas
##                y personas por hogar
########################################################

Afford_Exp <- function(Hexpense, umbrales, gasto = c("alimentos", "ingreso"),
                       agrupar = c("deciles", "tamano"), dias = 30) {

  gasto   <- match.arg(gasto)
  agrupar <- match.arg(agrupar)

  requeridas <- c("deciles", "fex_c18", "ung", "food_exp_per_capita", "per_capita_income")
  faltan <- setdiff(requeridas, names(Hexpense))
  if (length(faltan) > 0) {
    stop("Hexpense le faltan las columnas: ", paste(faltan, collapse = ", "))
  }
  if (!all(c("model", "per_capita") %in% names(umbrales))) {
    stop("umbrales debe tener las columnas model y per_capita")
  }
  por_tamano <- "tamano" %in% names(umbrales)

  Hexpense <- Hexpense %>%
    dplyr::mutate(
      gasto_pc = if (gasto == "alimentos") food_exp_per_capita else per_capita_income,
      tamano   = pmin(pmax(as.numeric(ung), 3), 5),
      grupo    = if (agrupar == "deciles") as.character(deciles) else
        dplyr::case_when(tamano == 3 ~ "3 o menos", tamano == 4 ~ "4", TRUE ~ "5 o más")
    )

  indicadores <- list()

  for (m in unique(umbrales$model)) {

    u <- umbrales %>% dplyr::filter(model == m)

    d <- if (por_tamano) {
      Hexpense %>%
        dplyr::left_join(dplyr::select(u, tamano, per_capita), by = "tamano") %>%
        dplyr::mutate(z = per_capita * dias)
    } else {
      Hexpense %>% dplyr::mutate(z = u$per_capita[1] * dias)
    }

    indicadores[[m]] <- d %>%
      dplyr::mutate(
        sin_pago = gasto_pc < z,
        brecha   = dplyr::if_else(sin_pago, (z - gasto_pc) / z, 0)
      ) %>%
      dplyr::group_by(grupo) %>%
      dplyr::summarise(
        rate     = 100 * sum(fex_c18[sin_pago]) / sum(fex_c18),
        gap      = sum(brecha * fex_c18) / sum(fex_c18),
        severity = sum(brecha^2 * fex_c18) / sum(fex_c18),
        umbral_medio = stats::weighted.mean(z, fex_c18),
        .groups = "drop"
      ) %>%
      dplyr::mutate(model = m, .before = 1)
  }

  indicadores <- dplyr::bind_rows(indicadores)

  medias <- Hexpense %>%
    dplyr::group_by(grupo) %>%
    dplyr::summarise(
      gasto_pc_medio = stats::weighted.mean(gasto_pc, fex_c18),
      hogares        = sum(fex_c18),
      personas_hogar = stats::weighted.mean(as.numeric(ung), fex_c18),
      .groups = "drop"
    )

  list(indicadores = indicadores, medias = medias)
}
