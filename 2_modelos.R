# ----------------------------- Retornados Honduras ---------------------------#
install.packages("tidymodels")
library(tidymodels)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

rm(list = ls())

# ----------> 1) Rutas y Data
path <- "/Users/bautistagianfrancisco/Library/Mobile Documents/com~apple~CloudDocs/Flor/Retornados"
data_path <- paste(path,"/data", sep = "")
setwd(path)

retronados <- read_csv(paste(data_path, "/retornados_clean.csv", sep = ""))

# ----------> 2) Modelado Predictivo: ¿Quién vuelve a ser deportado?

# ----- 2.1) Definicion de la muestra

# Separo en train y test
retornados <- retornados %>%
  mutate(re_retorno = if_else(total_retorno >= 2, 1L, 0L))

set.seed(42)
split   <- initial_split(retornados, prop = 0.7, strata = re_retorno)
train   <- training(split)
testing <- testing(split)

# Winsorizo algunas variables
corte_edad  <- quantile(train$edad_al_retornar, 0.9999, na.rm = TRUE)
corte_npers <- quantile(train$numero_personas_nucleo, 0.99, na.rm = TRUE)

# Winsorización en train
train <- train %>%
  mutate(
    edad_al_retornar = if_else(edad_al_retornar > corte_edad, corte_edad, edad_al_retornar),
    numero_personas_nucleo = if_else(numero_personas_nucleo > corte_npers, corte_npers, numero_personas_nucleo)
  )

# Winsorización en test usando los mismos cortes
testing <- testing %>%
  mutate(
    edad_al_retornar = if_else(edad_al_retornar > corte_edad, corte_edad, edad_al_retornar),
    numero_personas_nucleo = if_else(numero_personas_nucleo > corte_npers, corte_npers, numero_personas_nucleo)
  )

# Setting
names(retornados)
rec <- recipe(
  recidiva ~ numero_personas_nucleo + delegacion_ingreso + frontera_salida + edad_al_retornar + 
    sexo + departemento_nacimiento  + estado_civi + cantidad_hijos_ninas_honduras +
    cantidad_hijos_ninas_eua + cantidad_hijos_ninas_otro_pais + cantidad_hijos_ninos_honduras +                         
    cantidad_hijos_ninos_eua + cantidad_hijos_ninos_otro_pais + departemento_dirige +
    razones_economicas + razones_familiares_reunificacion_familiar + salud + estudios +           
    violencia_domestica + violencia_o_inseguridad + solicito_proteccion +pais_dirigia +
    discapacidad_para_oir_parcial_o_total + discapacidad_visual_parcial_o_total +               
    discapacidad_para_hablar_parcial_o_total + discapacidad_uso_de_brazos_y_manos_piernas_y_pies +      
    discapacidad_mental_o_intelectual +ingles +conocimiento_informatica + vivienda_propia +
    piso_tierra + agua_potable + tratamiento_agua + servicio_sanitario + problema_salud +
    paredes_bdm + techo_pd + cocinar_lr,   # educación ?   
  data = train) %>%
  step_impute_median(all_numeric()) %>%             # Reemplaza sus NA por la mediana de esa columna (en el set de train).
  step_other(all_nominal(), threshold = 0.01) %>%   # Agrupa las categorías que aparecen en menos del 1 % de los casos en una categoría “other”.
  step_dummy(all_nominal())                         # Transforma todos los factores en variables dummy


# Análisis de supervivencia (“time‐to‐event”)
# Clustering y segmentación de perfiles