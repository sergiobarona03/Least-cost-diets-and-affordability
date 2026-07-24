
#------------------------------------------------------------------------------------------#
#                    CUARTA FUNCIÓN: MODELO 3: DIETA SALUDABLE                             #
#-----------------------------------------------------------------------------------------#

# Datos de ejemplo



CoRD_Herforth =function(data,serv,diverse,exclude=NULL){
  
  
  #------------------------------------------------------------------------------------------#
  #                       PRIMERA ETAPA: VALIDACIÓN DE LIBRERIAS                             #
  #-----------------------------------------------------------------------------------------#
  
  Librerias_base = c("tidyverse","rio","janitor","stringdist","lpSolve","knitr") # Nombra las librerias necesarias
  
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
  
  
  #cat("\n")
  #cat("Se instalaron y cargaron todas la librerias corectamente")
  #cat("\n")
  
  #------------------------------------------------------------------------------------------#
  #         SEGUNDA ETAPA.1: VALIDACIÓN DE PARÁMETROS OBLIGATORIOS Y OPCIONALES                #
  #-----------------------------------------------------------------------------------------#
  #-------------- VERIFICACIÓN DE DATOS DE INSUMO
  
  # Verifying if data is a data frame
  if (!is.data.frame(data)) {
    stop("data is not a data frame.")
  }
  
  # Verifying if it has at least 3 columns
  if (ncol(data) < 4) {
    stop("data must have at least 4 columns.")
  }
  required_columns <- c("Food", "Serving_g", "Price_serving", "Group")
  missing_columns <- setdiff(required_columns, colnames(data))
  
  if (length(missing_columns) > 0) {
    stop(paste("CoRD model requires the following columns in the input data: ", paste(missing_columns, collapse = ", "), ". Please refer to the documentation for the required column names for the CoRD model."))}
  
  
  # -------------- VERIFICACIÓN DE REQ
  # Verifying if serv is a data frame
  if (!is.data.frame(serv)) {
    stop("serv is not a data frame.")
  }
  
  # Verifying if it has at least 2 columns
  if (ncol(serv) < 2) {
    stop("Requirements for the CoRD model must have at least 2 columns.")
  }
  
  required_columns_E <- c("Age", "Serving")
  missing_columns_E <- setdiff(required_columns_E, colnames(serv))
  
  if (length(missing_columns_E) > 0) {
    stop(paste("CoRD model requires the following columns in the 'serv' parameter: ", paste(missing_columns_E, collapse = ", "), ". Please refer to the documentation for the required column names for the CoRD model."))}
  
  
  # -------------- VERIFICATION OF diverse
  
  
  # Verifying if diverse is a data frame
  if (!is.data.frame(diverse)) {
    stop("diverse is not a data frame.")
  }
  
  # Verifying if it has at least 2 columns
  if (ncol(diverse) < 2) {
    stop("The quantity of groups to select for the CoRD model must have at least 2 columns.")
  }
  
  required_columns_E <- c("Number")
  missing_columns_E <- setdiff(required_columns_E, colnames(diverse))
  
  if (length(missing_columns_E) > 0) {
    stop(paste("CoRD model requires the following columns in the 'diverse' parameter: ", paste(missing_columns_E, collapse = ", "), ". Please refer to the documentation for the required column names for the CoRD model."))}
  
  
  # -------------- VALIDACIÓN DE exclude
  
  # Validar si exclude es distinto de NULL
  
  if (!is.null(exclude)) {
    # Validar si exclude es un vector
    if (!is.vector(exclude)) {
      stop("Exclude parameter must be a vector.")
    }
    
    # Filtrar los alimentos que no están en exclude
    data <- data[!(data$Food %in% exclude), ]
  }
  
  
  #------------------------------------------------------------------------------------------#
  #                       SEGUNDA ETAPA: FILTRAR RECOMENDACIONES                            #
  #-----------------------------------------------------------------------------------------#
  
  
  # Validar si existen Grupos o Subgrupos
  
  
  
  #se excluyen los alimentos sin categorías
  data = data %>% filter(!Group %in% "Sin categoría")
  
  
  # Definir códigos y alimentos a eliminar
  codigos_a_eliminar <- c("L017", "D020", "K018", "K018")
  alimentos_a_eliminar <- c("Carne de cerdo, espinazo", "Yuca ICA", "Papa Betina", "Papa única")
  
  # Filtrar el dataframe
  if ("Cod_TCAC" %in% colnames(data)){
    data <- data %>%
      filter(!(Cod_TCAC %in% codigos_a_eliminar) & !(Food %in% alimentos_a_eliminar))}
  
  #--------------------------------------------------------------------------------------------------------------------#
  #                       TERCER  ETAPA: SELECCIÓN Y VALDIACIÓN DE GRUPOS EN MODELO FEMENINO                      #
  #------------------------------------------------------------------------------------------------------------------#
  
  # Verificar si existe la columna "Sex"
  
  # Extraer sexos disponibles si existe la columna sexo
  if ("Sex" %in% colnames(serv)) {Sexos <- split(serv, serv$Sex);sexo_nombre=names(Sexos)} else {
    sexo_nombre=0
    
  }
  
  Req_entrantes=serv
  Food=data$Food
  Precio=data$Price_serving
  #--------------------------------------------------------#
  #               CLICLO PARA CADA SEXO                   #
  #-------------------------------------------------------#
  
  for (sexo_nombre in sexo_nombre) { 
    
    if ("Sex" %in% colnames(serv)) {serv <- Sexos[[sexo_nombre]]}
    
    # Requerimiento y edad
    Req_Int_i=serv
    Age=levels(as.factor(as.character(Req_Int_i$Age)))
    
    
    #---------------- VALIDACIÓN Y SELECIÓN DE GRUPOS----------------
    
    # validación de subgrupo
    if (("Subgroup" %in% colnames(diverse))) {
      
      
      # Verificar si los dataframes tienen la columna "Subgroup"
      if (!("Subgroup" %in% colnames(serv))) {
        stop("The serv dataframe does not have the 'Subgroup' column.")
      }
      
      if (!("Subgroup" %in% colnames(data))) {
        stop("The data dataframe does not have the 'Subgroup' column.")
      }
      
    }
    
    
    
    #Identificar grupos o subgrupos
    if ("Subgroup" %in% colnames(data) && "Subgroup" %in% colnames(diverse) && "Subgroup" %in% colnames(serv)) {
      Grupos_Insumo=levels(as.factor(data$Subgroup))
      Grupos_Cantidad_Sel <- unique(diverse$Subgroup) #grupos de cantidad
      grupos_req=levels(as.factor(Req_Int_i$Subgroup)) 
      
    } else {
      
      if ("Group" %in% colnames(data) && "Group" %in% colnames(diverse) && "Group" %in% colnames(serv)) {
        
        Grupos_Insumo=levels(as.factor(data$Group)) 
        Grupos_Cantidad_Sel <- unique(diverse$Group) #grupos de cantidad
        grupos_req=levels(as.factor(Req_Int_i$Group))
        
      } else {
        stop("All three parameters must have the 'Group' column if they do not have 'Subgroup'")
        
      }
      
    }
    
    
    #-------------------
    # 1. Validar que la cantidad de grupos en el vector sea mayor que 5
    if (length(Grupos_Insumo) < 5) {
      stop("Error:The quantity of groups in the vector must be greater than or equal to 5.")
    }
    
    
    # 2. Identificar los grupos que son iguales de datos insumo y cantidad a selecionar
    Grupos_comunes <- intersect(Grupos_Insumo, Grupos_Cantidad_Sel)
    
    
    
    # Grupos no comunes
    grupos_faltantes <- union(setdiff(Grupos_Insumo, Grupos_Cantidad_Sel),setdiff(Grupos_Cantidad_Sel,Grupos_Insumo))
    
    
    
    if(length(grupos_faltantes)>0){
      paste("Caution: There are groups not common between the input data groups and the quantity to select from them:",paste(grupos_faltantes,collapse = ", "))
    }
    
    
    
    if(length(grupos_faltantes)>0){
      paste("We will then work only with the equal groups, which are:",paste(Grupos_comunes,collapse = ", "))
    }
    
    # Validar la intersección entre grupos de req y los demás
    Grupos_comunes_req <- intersect(Grupos_comunes, grupos_req)
    
    grupos_faltantes_req=union(setdiff(Grupos_comunes, grupos_req),setdiff(grupos_req,Grupos_comunes))
    
    if(length(grupos_faltantes_req)>0){
      paste("Caution: There are groups not common among the input data groups, the quantity to select, and the requirements. These are:",paste(grupos_faltantes_req,collapse = ", "))
    }
    
    if (length(Grupos_comunes_req)<5){stop("Caution: The common groups among the requirements, input data, and quantity to select are very few; they must be more than 5")}
    
    if(length(grupos_faltantes_req)>0){
      paste("We will then work only with the common groups in the three group vectors; these are:",paste(Grupos_comunes_req,collapse = ", "))
    }
    
    # Number a selcionar, sólo los comunes
    
    if ("Subgroup" %in% colnames(data) && "Subgroup" %in% colnames(diverse) && "Subgroup" %in% colnames(serv)) {Grupos_finales <- subset(diverse, Subgroup %in% Grupos_comunes_req)}
    
    if ("Group" %in% colnames(data) && "Group" %in% colnames(diverse) && "Group" %in% colnames(serv)) {Grupos_finales <- subset(diverse, Group %in% Grupos_comunes_req)}
    
    
    
    #--------------------------------------------------------------------------------------------------------------------#
    #                       CUARTA ETAPA : MODELO      3                                                                #
    #------------------------------------------------------------------------------------------------------------------#
  
    CoRD_INT <- data.frame()  # DATA DE SALIDA
    Edad_CoRD <- c()  # Age de salida
    CoRD_COST <- data.frame() # DF de los costos
    
    # -------------------------------- EXTRACCIONES
    
    # Extraer reque según la edad
    for (i in 1:length(Age)) {
      
      # Extraer req por edad
      E_i=Age[i]
      Req_i = subset(Req_Int_i, Age == E_i)
      
      for (j in 1:nrow(Grupos_finales)) {
        
        # Extraer el grupo y la cantidad a seleccionar
        
        if ("Subgroup" %in% colnames(data) && "Subgroup" %in% colnames(diverse) && "Subgroup" %in% colnames(serv)) {Grupo_i = Grupos_finales$Subgroup[j] } 
        
        if ("Group" %in% colnames(data) && "Group" %in% colnames(diverse) && "Group" %in% colnames(serv)) {Grupo_i = Grupos_finales$Group[j] }
        
        # Usar j para el bucle interior
        Cantidad_i = Grupos_finales$Number[j]  # Usar j para el bucle interior
        
        # Extraer y requerimientos datos por grupo o subgrupo
        
        if ("Subgroup" %in% colnames(data) && "Subgroup" %in% colnames(diverse) && "Subgroup" %in% colnames(serv)) {Datos_grupo_i = subset(data, Subgroup == Grupo_i) } 
        
        if ("Group" %in% colnames(data) && "Group" %in% colnames(diverse) && "Group" %in% colnames(serv)) {Datos_grupo_i = subset(data, Group == Grupo_i)}
        
        
        
        Datos_grupo_i = Datos_grupo_i[order(Datos_grupo_i$Price_serving), ]
        
        if ("Subgroup" %in% colnames(data) && "Subgroup" %in% colnames(diverse) && "Subgroup" %in% colnames(serv)) {    Req_i_g = subset(Req_i, Subgroup == Grupo_i) } 
        
        if ("Group" %in% colnames(data) && "Group" %in% colnames(diverse) && "Group" %in% colnames(serv)) {    Req_i_g = subset(Req_i, Group == Grupo_i) }
        
        
        
        # Dejar columnas útiles en Datos_grupo_i
        if (Cantidad_i == 0) {
          stop("The quantity of elements to select must be an integer greater than zero.")
        }
        
        if ("Subgroup" %in% colnames(data) && "Subgroup" %in% colnames(diverse) && "Subgroup" %in% colnames(serv)) { Datos_grupo_i = Datos_grupo_i %>% select(any_of(c("Food", "Price_100g", "Serving_g", "Price_serving", "Subgroup","Energy")))}
        
        if ("Group" %in% colnames(data) && "Group" %in% colnames(diverse) && "Group" %in% colnames(serv)) { Datos_grupo_i = Datos_grupo_i %>% select(any_of(c("Food", "Price_100g", "Serving_g", "Price_serving", "Group","Energy")))}
        
        
        Datos_grupo_i = Datos_grupo_i[1:Cantidad_i, ]
        
        # Determinar N serving como el requerimiento/2
        Number_Serving = rep(Req_i_g$Serving/Cantidad_i, Cantidad_i)
        
        Cantidad_g = Number_Serving * Datos_grupo_i$Serving_g
        
        # Crear dataframe directamente y agregar las filas a CoRD_INT_F
        if ("Energy" %in% colnames(data)) {CoRD_F = cbind(Datos_grupo_i %>% select(-c("Energy",
                                                                                      "Serving_g")),
                                                          Number_Serving = Number_Serving, 
                                                          Cantidad_g = Cantidad_g,Demo_Group=Age[i],
                                                          Sex=sexo_nombre,Energy=Datos_grupo_i$Energy,
                                                          Serving_g=Datos_grupo_i$Serving_g) }else{CoRD_F = cbind(Datos_grupo_i, Number_Serving = Number_Serving, Cantidad_g = Cantidad_g,Demo_Group=Age[i],Sex=sexo_nombre)}
        
        CoRD_INT = rbind(CoRD_INT, CoRD_F)
        
      }
      #SALIDA CON EDAD 
      
    }
    
    
    
    #------------------ CÁLCULO DEL COSTO POR EDAD
    Aporte=data.frame()
    
    for (E in Age) {

      # Filtrar el dataframe por edad
      df_edad <- subset(CoRD_INT, Demo_Group == E)
      
      
      # Calcular el costo para la edad actual
      costo_edad <- sum(df_edad$Price_serving * df_edad$Number_Serving)
      
      # calcular costo * 1000kc o no
      if ("Energy" %in% colnames(data)){df_temp <- data.frame(Demo_Group = E, cost_day = costo_edad,      Cost_1000kcal= (costo_edad/(sum((df_edad$Energy/100)*df_edad$Cantidad_g)))*1000)}else {df_temp <- data.frame(Demo_Group = E, cost_day = costo_edad)}
      
      # Agregar el dataframe temporal a costo
      CoRD_COST <- rbind(CoRD_COST, df_temp)
      
    };CoRD_COST$Sex=as.numeric(sexo_nombre)
    
    
    # ----------- ESTRUCTURA CIAT PARA INTERCAMBIOS
    
    if ("Subgroup" %in% colnames(data) && "Subgroup" %in% colnames(diverse) && "Subgroup" %in% colnames(serv)) {
      
      suppressWarnings({CoRD_INT <- merge(CoRD_INT, data, by = "Food", all.x = TRUE)})# recuperar insumos
      CoRD_INT= CoRD_INT %>% select(any_of(c("Food","Group","Number_Serving","Demo_Group","Sex")))
    }
    
    
    
    if ("Group" %in% colnames(data) && "Group" %in% colnames(diverse) && "Group" %in% colnames(serv)) {CoRD_INT= CoRD_INT %>% select(any_of(c("Food","Group","Number_Serving","Demo_Group","Sex")))}
    
    # Asignaciones por sexo
    assign(paste("CoRD_", sexo_nombre, sep = ""), CoRD_COST)
    assign(paste("Intercambios_CoRD_", sexo_nombre, sep = ""), CoRD_INT)
  }
  
  #--------------------------------------------------------#
  #        FIND DEL       CLICLO PARA CADA SEXO            #
  #-------------------------------------------------------#
  
  
  # Unir ambos df para cada sexo (si existe)
  if ("Sex" %in% colnames(serv)) {
    
    Costo_CORD=rbind(CoRD_1,CoRD_0)
    Intercambios_CoRD=rbind(Intercambios_CoRD_0,Intercambios_CoRD_1)
    
  } else {
    
    Costo_CORD <- CoRD_0 %>%
      select(-Sex)
    Intercambios_CoRD<- Intercambios_CoRD_0 %>%
      select(-Sex)
  }
  
  
  
  #----------------------------#
  #     ASGINACIONES DE LISTA  #
  #----------------------------#
  
  if ("Energy" %in% colnames(data)){  
    List_CoRD=list(Costo_CORD,Intercambios_CoRD,Precio,Food,Energy=data$Energy,Req_entrantes);names(List_CoRD)=c("cost","comp","p","x","Energy","serv")
    
  }else {
    List_CoRD=list(Costo_CORD,Intercambios_CoRD,Precio,Food,Req_entrantes);names(List_CoRD)=c("cost","comp","p","x","serv")
    
  } 
  
  
  
  
  
  if ("Energy" %in% colnames(data)){cat("CoRD: Average daily cost per 1000 kilocalories is", mean(Costo_CORD$Cost_1000kcal))}else {
    cat("CoRD")
  } 
  
  return(invisible(List_CoRD))
  
  
}
