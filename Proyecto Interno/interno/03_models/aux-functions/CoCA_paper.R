#------------------------------------------------------------------------------------------#
#                     SEGUNDA FUNCIÓN: MODELO 1: DIETA SUF EN ENERGÍA                      #
#-----------------------------------------------------------------------------------------#

CoCA_paper =function(data,EER,exclude=NULL){
  
  #------------------------------------------------------------------------------------------#
  #                       PRIMERA ETAPA: VALIDACIÓN DE LIBRERIAS                             #
  #-----------------------------------------------------------------------------------------#
  
  Librerias_base = c("tidyverse","rio","janitor","stringdist","lpSolve","knitr")  # Nombra las librerias necesarias
  
  if (!require("pacman")) install.packages("pacman") # Paquete que simplifica la carga de librerias
  pacman::p_load(char = Librerias_base);Librerias_base_print = paste0(paste0("'", Librerias_base, "'"), collapse = ", ") # Instala si es necesario, o en su defecto, sólo llama los paquetes
  
  
  # Instala paquetes individualmente si no se han cargado correctamente
  paquetes_faltantes <- Librerias_base[!(Librerias_base %in% pacman::p_loaded())]
  for (paquete in paquetes_faltantes) {
    if (!require(paquete, character.only = TRUE)) {
      install.packages(paquete)
      library(paquete, character.only = TRUE)
    }
  }
  
  
  
  
  #------------------------------------------------------------------------------------------#
  #         SEGUNDA ETAPA: VALIDACIÓN DE PARÁMETROS OBLIGATORIOS Y OPCIONALES                #
  #-----------------------------------------------------------------------------------------#
  
  # -------------- VERIFICACIÓN DE DATOS DE INSUMO
  
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("data is not a data frame.")
  }
  
  # Check if it has at least 3 columns
  if (ncol(data) < 3) {
    stop("data must have at least 3 columns.")
  }
  required_columns <- c("Price_100g", "Food", "Energy")
  missing_columns <- setdiff(required_columns, colnames(data))
  
  if (length(missing_columns) > 0) {
    stop(paste("Model requires the following columns in the input data: ", paste(missing_columns, collapse = ", "),". Please refer to the documentation for the required column names for the first model."))}
  
  #Filtrar azucar
  if ("Cod_TCAC" %in% colnames(data)) {data = data %>% filter(!Cod_TCAC %in% c("K003", "K004", "K033","D013"))} 
  
  # -------------- VERIFICACIÓN DE EER
  
  
  # Verificar si data es un data frame
  if (!is.data.frame(EER)) {
    stop("EER is not a data frame.")
  }
  
  # Check if it has at least 2 columns
  if (ncol(EER) < 2) {
    stop("Requirements for Model 1 must have at least 2 columns.")
  }
  required_columns_E <- c("Age", "Energy")
  missing_columns_E <- setdiff(required_columns_E, colnames(EER))
  
  if (length(missing_columns_E) > 0) {
    stop(paste("Model 1 requires the following columns in the input data: ", paste(missing_columns_E, collapse = ", "),". Please refer to the documentation for the required column names for the first model."))}
  
  
  # -------------- VALIDACIÓN DE exclude
  
  # Validar si exclude es distinto de NULL
  
  if (!is.null(exclude)) {
    # Validar si exclude es un vector
    if (!is.vector(exclude)) {
      stop("The 'exclude' parameter must be a vector.")
    }
    
    # Filtrar los alimentos que no están en exclude
    data <- data[!(data$Food %in% exclude), ]
  }
  
  
  #------------------------------------------------------------------------------------------#
  #                       TERCERA ETAPA: MODELO 1                                           #
  #-----------------------------------------------------------------------------------------#
  
  #Si no existe la columna sexo
  
  # Extraer sexos disponibles si existe la columna sexo
  if ("Sex" %in% colnames(EER)) {Sexos <- split(EER, EER$Sex);sexo_nombre=names(Sexos)} else {
    sexo_nombre=0
    
  }
  
  
  #--------------------------------------------------------#
  #               CLICLO PARA CADA SEXO                   #
  #-------------------------------------------------------#
  for (sexo_nombre in sexo_nombre) { 
    
    
    
    Salida_CoCA <- data.frame(Alimentos = data$Food);Salida_CoCA <- Salida_CoCA %>% add_row(Alimentos = "cost_day") # Define el df de salida
    
    # REquerimientos por sexo
    if ("Sex" %in% colnames(EER)) {EER_S <- Sexos[[sexo_nombre]]}else{EER_S=EER}
    
    
    # ---Asignación de vectores al modelo
    Precio = data$Price_100g;Food=data$Food;Age=EER_S$Age
    
    # MAtriz de coef de restricción al modelo (ENERGIA)
    Coef.Restriq = matrix(as.vector(data$Energy), ncol = length(Food))
    
    # Vector de limitaciones del modelo
    Limitaciones=EER_S$Energy
    
    
    #------------------------------Solución del modelo:
    
    # Modelo
    
    for( i in seq_along(Limitaciones)) {#ciclo para cada edad
      
      CoCA = lp(direction = "min",
                objective.in = Precio,
                const.mat = Coef.Restriq,
                const.dir = c("="),
                const.rhs = Limitaciones[i],
                compute.sens = TRUE)
      
      #------------------------ POR EDAD Guardar soluciones
      
      
      # Crear dataframe df_1 con alimentos y la solución óptima para la edad actual
      df_1 <- data.frame(Alimentos = Food, Valor = CoCA$solution);colnames(df_1) <- c("Alimentos", as.character(Age[i]))
      
      # Crear dataframe df_2 con el costo asociado para la edad actual
      df_2 <- data.frame(Alimentos = "cost_day", Valor = CoCA$objval);colnames(df_2) <- colnames(df_1);df_combinado <- rbind(df_1, df_2)
      
      # Agregar la información al dataframe modelo_1
      
      Salida_CoCA <- merge(Salida_CoCA, df_combinado, by = "Alimentos")
      
      
      
    }  #Fin del ciclio del modelo por edad
    
    #--------------------------------------------------------#
    #               ETAPA DE ESTRUCTURA PLAZA                #
    #-------------------------------------------------------#
    
    #DF sin ceros
    DF_o <- Salida_CoCA[
      rowSums(Salida_CoCA[, -which(names(Salida_CoCA) %in% c("Alimentos")), drop = FALSE]) != 0, 
    ]
    
    cost_day=DF_o[DF_o=="cost_day",];cost_day=as.vector(t(cost_day[cost_day$Alimentos == "cost_day", -1]))
    Cost_1000kcal=(cost_day/Limitaciones)*1000
    # Identificar grupo si existe la columna en insumos
    
    if ("Group" %in% colnames(data)) {Grupo_sex=na.omit(as.vector(data$Group[match(Food, DF_o$Alimentos)]))[1]}
    
    
    
    # Indetificando el alimento
    df_alimentos <- DF_o[DF_o$Alimentos != "cost_day", ]
    
    # Multiplicar los valores por 100
    df_alimentos[, -1] <- df_alimentos[, -1] * 100
    
    
    #ESTRUCTURA CIAT
    df_transformado <- df_alimentos %>%
      pivot_longer(cols = -Alimentos, names_to = "Demo_Group", values_to = "quantity") %>%
      mutate(
        Food = Alimentos,
        Sex = as.numeric(sexo_nombre),
        Group = if ("Group" %in% colnames(data)) Grupo_sex else NA
      ) %>%
      select(Food, quantity, Demo_Group, Sex, Group)
    
    df_transformado_limpio <- df_transformado %>%
      select(-where(~all(is.na(.))))
    
    
    assign(paste("CoCA_", sexo_nombre, sep = ""), cbind(df_transformado_limpio, cost_day,Cost_1000kcal))
    
    
    #--------------------------------------------------------#
    #        FIND DEL       CLICLO PARA CADA SEXO            #
    #-------------------------------------------------------#
    
    
  }
  # Unir ambos df para cada sexo (si existe)
  if ("Sex" %in% colnames(EER)) {Costo_CoCA=rbind(CoCA_1,CoCA_0)} else {
    Costo_CoCA <- CoCA_0 %>%
      select(-Sex)
  }
  
  #----------------------------#
  #     ASGINACIONES DE LISTA  #
  #----------------------------#
  
  List_CoCA=list(Costo_CoCA,Precio,Food,EER);names(List_CoCA)=c("cost","p","x","energy")
  
  # retorno
  
  cat("CoCA: Average daily cost per 1000 kilocalories is ", mean(Costo_CoCA$Cost_1000kcal)) 
  
  return(invisible(List_CoCA))
  
  #------------------------------------------------------------------------------------------#
  #                       FIN DEL SEGUNDO MÓDULO COMO FUNCIÓN                               #
  #-----------------------------------------------------------------------------------------#
  
}
