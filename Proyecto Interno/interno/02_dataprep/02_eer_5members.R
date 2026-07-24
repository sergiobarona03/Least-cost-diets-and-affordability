########################################################
## SCRIPT 02_dataprep/02_eer_5members.R
## Prepara los inputs de EER para el hogar representativo de
## 3 miembros (adulto hombre 31-51, adulta mujer 31-51, niña
## 10-14). Toma como insumo el agregado de EER que
## produce 03-EER-agregados.R.
##
## Representative household:
##   Adult male   31-51
##   Adult female 31-51
##   Female child 10-14
##
## Reads:  eer_output_dir/230726_agg_eer.xlsx
##         FoodpriceR::EER_LL
##         FoodpriceR::UL
##
## Writes: household_dir/household_eer.rds
##         household_dir/household_eer_ll.rds
##         household_dir/household_ul.rds
########################################################