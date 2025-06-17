
##-----------------------------------##
## Carga y procesamiento: Datos DANE ##
## Fecha: 14 de junio de 2025        ##
##-----------------------------------##

# Cargar bibliotecas
import pandas as pd
import numpy as np
import os

##----------------------------------------------##
## Definir funciones para la limpieza de datos  ##
##----------------------------------------------##

# Definir función f1 (rango 2017 - 2018)
def f1(df, year):
    
    # Identificar el rango
    meses = ['Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio',
         'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre']
    indices_meses = [df.columns.get_loc(col) for col in meses]
    
    # Recodificar: nombre de variables
    df.iloc[0, indices_meses] = df.columns[indices_meses]
    df.columns = df.iloc[0]
    df = df.drop(index=0).reset_index(drop=True)
    
    # Limpiar nombres de columnas
    df.columns = (
        df.columns
        .str.strip()
        .str.lower()
        .str.replace(' ', '_')
        .str.replace(r'[^\w_]', '', regex=True)
    )
    
    # Hay columnas que no son necesarias, porque se repiten
    df = df[['año', 'ciudad', 'nombre_ciudad', 'codigo_articulo', 'articulo',
             'unidad', 'enero', 'febrero', 'marzo', 'abril', 'mayo',
           'junio', 'julio', 'agosto', 'septiembre', 'octubre', 'noviembre',
           'diciembre']]
    
    
    # Eliminar filas con NAs y mostrar cuáles fueron eliminadas
    print("Filas con NA eliminadas:\n", df[df.isna().any(axis=1)])
    df = df.dropna()

    # Melt
    df_melt = pd.melt(
        df,
        id_vars=['año', 'ciudad', 'nombre_ciudad', 'codigo_articulo', 'articulo',
                 'unidad'],
        value_vars=['enero', 'febrero', 'marzo', 'abril', 'mayo',
      'junio', 'julio', 'agosto', 'septiembre', 'octubre', 'noviembre',
      'diciembre'],
        var_name='mes',
        value_name='precio'
    )

    return df_melt

##----------------------------##
## Carga y limpieza de datos  ##
##----------------------------##

# Definir directorio de trabajo
working_dir = r"C:\Users\Portatil\Desktop\Least-cost-diets-and-affordability\Proyecto Interno\Precios DANE"
os.chdir(working_dir)

# Cargar archivos (años 2017 - 2018)
p_2015 = pd.read_excel(os.path.join("2015", "Publicado_Precios_promedio _2015_Dic15.xls"))
p_2016 = pd.read_excel(os.path.join("2016", "Publicado_Precios_promedio _Dic2016.xlsx"))
p_2017 = pd.read_excel(os.path.join("2017", "Precios Promedio_Alimentos_17.xlsx"))
p_2018 = pd.read_excel(os.path.join("2018", "precios promedio.xlsx"))

# Procesamiento del tipo 1 (rango 2016 - 2018)
p_melt_2016 = f1(p_2016, 2016)
p_melt_2017 = f1(p_2017, 2017)
p_melt_2018 = f1(p_2018, 2018)


