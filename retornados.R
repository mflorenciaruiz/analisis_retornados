# ------------------------ Análsis de retornados Honduras ---------------------#
install.packages("tidyr")
install.packages("psych") 
install.packages("skimr")
install.packages("janitor")

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(psych)

rm(list = ls())

# -------> 1) Defino las rutas y el directorio
path <- "/Users/bautistagianfrancisco/Library/Mobile Documents/com~apple~CloudDocs/Flor/Retornados"
data_path <- paste(path,"/data", sep = "")
setwd(path)

# -------> 2) Data
retornados_raw <- read_xlsx(paste(data_path,"/Datos_Anonimizados_BM_al_03_06_2025.xlsx", sep=""))

summary(retornados_raw)
sapply(retornados_raw, class)

# ---> 2.1) NAs
NAs <- c("NULL", "NA", "N/A", "N / A", "SIN DATO", "Sin Dato", "sin dato", "")
retornados <- retornados_raw %>%
  mutate(across(where(is.character), ~ ifelse(.x %in% NAs, NA, .x)))

# ---> 2.2) Variables

### Información personal y familiar
table(retornados_raw$Sexo)
table(retornados_raw$EstadoCivi)
unique(retornados_raw$DescripcionProfesion)
table(retornados_raw$Embarazada)
table(retornados_raw$MadreViva)
table(retornados_raw$PadreVivo)
table(retornados_raw$ParentescoTutor)
unique(retornados_raw$OtroParentescoTutor)

retornados <- retornados %>% 
  mutate(
    # Variables como factor
    Sexo = ifelse(Sexo=="M", 1, 0),
    Sexo = factor(Sexo, levels = c(0, 1),           
                       labels = c("F", "M")),
    EstadoCivi = case_when(EstadoCivi == "Soltero/a"     ~ 1,
                               EstadoCivi == "Unión Libre"   ~ 2,
                               EstadoCivi == "Casado/a"      ~ 3,
                               EstadoCivi == "Separado/a"    ~ 4,
                               EstadoCivi == "Divorciado/a"  ~ 5,
                               EstadoCivi == "Viudo/a"       ~ 6,
                               TRUE                          ~ NA_real_),
    EstadoCivi = factor(EstadoCivi,levels = 1:6, 
                        labels = c("Soltero/a", "Unión Libre", "Casado/a",
                                    "Separado/a", "Divorciado/a", "Viudo/a")),
    MadreViva = case_when(MadreViva == "Viva"      ~ 1,
                          MadreViva == "Fallecida" ~ 2,
                          MadreViva == "No sabe"   ~ 3,
                          TRUE                     ~ NA_real_),
    MadreViva = factor(MadreViva, levels= 1:3, labels = c("Viva", "Fallecida", 
                                                               "No sabe")),
    PadreVivo = case_when(PadreVivo == "Viva"      ~ 1,
                          PadreVivo == "Fallecida" ~ 2,
                          PadreVivo == "No sabe"   ~ 3,
                          TRUE                    ~ NA_real_),
    PadreVivo = factor(PadreVivo, levels= 1:3, labels = c("Vivo", "Fallecido", 
                                                          "No sabe")),
    ParentescoTutor = case_when(ParentescoTutor == "Madre"              ~ 1,
                                ParentescoTutor == "Padre"              ~ 2,
                                ParentescoTutor == "Abuelo/a"           ~ 3,
                                ParentescoTutor == "Hermano/a"          ~ 4,
                                ParentescoTutor == "Tio/a"              ~ 5,
                                ParentescoTutor == "Primo/a"            ~ 6,
                                ParentescoTutor == "Otro (Especifique)" ~ 7,
                                TRUE                                    ~ NA_real_),
    ParentescoTutor = factor(ParentescoTutor, levels = 1:7, labels =
                               c("Madre", "Padre", "Abuelo/a", "Hermano/a", 
                                 "Tio/a", "Primo/a", "Otro")),
    # Variables numéricas 
    CantidadHijosNinasGuatemala = as.numeric(CantidadHijosNinasGuatemala),
    CantidadHijosNinasMexico = as.numeric(CantidadHijosNinasMexico),
    CantidadHijosNinosGuatemala = as.numeric(CantidadHijosNinosGuatemala),
    CantidadHijosNinosMexico = as.numeric(CantidadHijosNinosMexico),
    Embarazada = as.numeric(ifelse(Embarazada == "Si",1, 0))
    )
   
# Chequeo
table(retornados_raw$Sexo)
table(retornados$Sexo)

table(retornados_raw$EstadoCivi)
table(retornados$EstadoCivi)

table(retornados_raw$Embarazada)
table(retornados$Embarazada)

table(retornados_raw$MadreViva)
table(retornados$MadreViva)

table(retornados_raw$PadreVivo)
table(retornados$PadreVivo)

table(retornados_raw$ParentescoTutor)
table(retornados$ParentescoTutor)

### Motivo de migración / salida / retorno
table(retornados_raw$SolicitoProteccion)
table(retornados_raw$PaisDirigia)
table(retornados_raw$OtroPaisDirigia)
table(retornados_raw$VecesIrregularIngPaisRetorno)
table(retornados_raw$VecesLegalIngPaisRetornado)

retornados <- retornados %>% 
  mutate(
    # Variables factor
    SolicitoProteccion = case_when(SolicitoProteccion == "No"                                       ~ 1,
                                   SolicitoProteccion == "Si, pero abandone el proceso."            ~ 2,
                                   SolicitoProteccion == "Si, pero me la denegaron y me deportaron" ~ 3,
                                   TRUE                                                      ~ NA_real_),
    SolicitoProteccion = factor(SolicitoProteccion, levels = 1:3, 
                                labels = c("No","Si y abandonó", "Si y la denegaron")),
    # Variables numéricas
    VecesIrregularIngPaisRetorno = as.numeric(VecesIrregularIngPaisRetorno),
    VecesLegalIngPaisRetornado = as.numeric(VecesLegalIngPaisRetornado),
  ) 

# Chequeo
table(retornados_raw$SolicitoProteccion)
table(retornados$SolicitoProteccion)

### Salud
table(retornados_raw$PresentaAlgunProblemaSalud)
class(retornados_raw$PresentaAlgunProblemaSalud)

retornados <- retornados %>% 
  mutate(
    ProblemaSalud = factor(PresentaAlgunProblemaSalud,
                           levels=c(1,2,3), 
                           labels=c("No sabe", "Si", "No"))) %>% 
  rename(ProblemaSalud_esp = `Problema de Salud`) %>% 
  select(-PresentaAlgunProblemaSalud)

# Chequeo
table(retornados_raw$PresentaAlgunProblemaSalud)
table(retornados$ProblemaSalud)

### Educación
table(retornados_raw$`Nivel Educativo`)
class(retornados_raw$`Nivel Educativo`)
unique(retornados_raw$PaisEstudio) # fix me: mapear con paises
table(retornados_raw$ConocimientoInformatica)
table(retornados_raw$OtroConocimiento)

retornados <- retornados %>% 
  mutate(
    `Nivel Educativo` = case_when(`Nivel Educativo`== "No realizó estudios"            ~ 1,
                                  `Nivel Educativo`== "Ninguno"                        ~ 1,
                                  `Nivel Educativo`== "Pre Escolar (1-3)"              ~ 2,
                                  `Nivel Educativo`== "Básica (Elementary)"            ~ 3,
                                  `Nivel Educativo`== "Primaria (1-6)"                 ~ 4,
                                  `Nivel Educativo`== "Básica III ciclo (Junior High)" ~ 5,
                                  `Nivel Educativo`== "Secundaria (1-6)"               ~ 6,
                                  `Nivel Educativo`== "Media (High School)"            ~ 7,
                                  `Nivel Educativo`== "Técnico (1-3)"                  ~ 8,
                                  `Nivel Educativo`== "Universitaria"                  ~ 9,
                                  `Nivel Educativo`== "Universitario (1-8)"            ~ 9,
                                  TRUE                                                  ~ NA_real_),
    
    `Nivel Educativo` = factor(`Nivel Educativo`, levels = 1:9,
                                labels=c("Ninguno", "Pre Escolar (1-3)","Básica (Elementary)", 
                                          "Primaria (1-6)", "Básica III ciclo (Junior High)",
                                          "Secundaria (1-6)", "Media (High School)", "Técnico (1-3)",
                                          "Universitaria"))
    
  )

# Chequeo
table(retornados_raw$`Nivel Educativo`)
table(retornados$`Nivel Educativo`)

### Trabajo / Datos económicos
unique(retornados_raw$DetalleOtroConocimiento)
unique(retornados_raw$ExperienciaLaboral) # fix me: agrupar en categorías
unique(retornados_raw$QueOficioGustariaAprender) # fix me: agrupar en categorías
unique(retornados_raw$DeseaEmprender)
class(retornados_raw$DeseaEmprender)
unique(retornados_raw$RubroEnprendimiento) # fix me: agrupar en categorías
table(retornados_raw$DispuestoFortalecerHabilidadesEmpresariales)
class(retornados_raw$DispuestoFortalecerHabilidadesEmpresariales)
table(retornados_raw$DeseaAcompanamientoTecnicoFinanciero)
class(retornados_raw$DeseaAcompanamientoTecnicoFinanciero)
table(retornados_raw$DispuestoLegalizarEmprendimiento)
class(retornados_raw$DispuestoLegalizarEmprendimiento)
table(retornados_raw$DispuestoVinculacionMercadosNacionalesSENPRENDE)
class(retornados_raw$DispuestoVinculacionMercadosNacionalesSENPRENDE)
table(retornados_raw$DispuestoVinculacionServiciosFinancierosSENPRENDE)
class(retornados_raw$DispuestoVinculacionServiciosFinancierosSENPRENDE)
table(retornados_raw$ConsentimientoSETRASS)
class(retornados_raw$ConsentimientoSETRASS)
unique(retornados_raw$ActividadAspiraOLD)
table(retornados_raw$SalarioNacional)
table(retornados_raw$SalarioExtranjero)
unique(retornados$DetalleRubros)   # fix me: agrupar en categorías
unique(retornados$CausasMigratoria)
table(retornados$Propiedades)  # categoías superpuestas y muchos NAs
table(retornados$ActividadAspira) # Muchos Nas
table(retornados$Certificaciones) # Muchos Nas

retornados <- retornados %>% 
  mutate(
    DeseaEmprender = as.numeric(DeseaEmprender),
    fortalecer_h_emp = as.numeric(DispuestoFortalecerHabilidadesEmpresariales),
    desea_acomp_tf = as.numeric(DeseaAcompanamientoTecnicoFinanciero),
    legalizar_emp = as.numeric(DispuestoLegalizarEmprendimiento),
    vincular_mercados = as.numeric(DispuestoVinculacionMercadosNacionalesSENPRENDE),
    vincular_servicios = as.numeric(DispuestoVinculacionServiciosFinancierosSENPRENDE),
    consen_SETRASS = as.numeric(ConsentimientoSETRASS),
    SalarioNacional = case_when(SalarioNacional == "Menos del salario minimo"         ~ 1,
                                SalarioNacional == "Salario minimo"                   ~ 2,
                                SalarioNacional == "De dos a tres salarios minimos"   ~ 3,
                                SalarioNacional == "Superior a tres salarios minimos" ~ 4,
                                TRUE                                                  ~ NA_real_),
    SalarioNacional = factor(SalarioNacional, levels = 1:4, 
                             labels = c("< salario mínimo", "Salario mínimo", 
                                        "2 a 3 salarios mínimos", "> 3 salarios mínimos")),
    SalarioExtranjero = case_when(SalarioExtranjero == "De $5 a $10 por hora"  ~ 1,
                                  SalarioExtranjero == "De $11 a $15 por hora" ~ 2,
                                  SalarioExtranjero == "De $16 a $20 por hora" ~ 3,
                                  SalarioExtranjero == "De $21 a $25 por hora" ~ 4,
                                  SalarioExtranjero == "De $26 o más por hora" ~ 5,
                                  TRUE                                         ~ NA_real_),
    SalarioExtranjero = factor(SalarioExtranjero, levels = 1:5, 
                               labels = c("$5-$10 c/hora","$11-$15 c/hora", "$16-$20 c/hora",
                                          "$21-$25 c/hora", "$26 o más c/hora"))
  ) %>% 
  select(-DispuestoFortalecerHabilidadesEmpresariales,-DeseaAcompanamientoTecnicoFinanciero,
         -DispuestoVinculacionMercadosNacionalesSENPRENDE)

# Chequeo
table(retornados_raw$SalarioNacional)
table(retornados$SalarioNacional)

table(retornados_raw$SalarioExtranjero)
table(retornados$SalarioExtranjero)

### Vivienda
table(retornados_raw$ViviendaPropia)
table(retornados_raw$PisoTierra)
table(retornados_raw$ParedesBaharequeDesechosMadera)
table(retornados_raw$TechoPajaDesecho)
table(retornados_raw$AguaPotable)
table(retornados_raw$TratamientoAgua)
table(retornados_raw$ServicioSanitario)
table(retornados_raw$CocinarLenaResiduos)

retornados <- retornados %>% 
  mutate(
    ViviendaPropia = ifelse(ViviendaPropia == "Si", 1, 0),
    PisoTierra = ifelse(PisoTierra == "Si", 1, 0),
    Paredes_BDM = ifelse(ParedesBaharequeDesechosMadera == "Si", 1, 0),
    Techo_PD = ifelse(TechoPajaDesecho == "Si", 1, 0),
    AguaPotable = ifelse(AguaPotable == "Si", 1, 0),
    TratamientoAgua = ifelse(TratamientoAgua == "Si", 1, 0),
    ServicioSanitario = ifelse(ServicioSanitario== "Si", 1, 0),
    Cocinar_LR = ifelse(CocinarLenaResiduos== "Si", 1, 0)
  )

# Chequeo
table(retornados_raw$ViviendaPropia)
table(retornados$ViviendaPropia)

table(retornados_raw$PisoTierra)
table(retornados$PisoTierra)

table(retornados_raw$ParedesBaharequeDesechosMadera)
table(retornados$ParedesBaharequeDesechosMadera)

table(retornados_raw$TechoPajaDesecho)
table(retornados$TechoPajaDesecho)

table(retornados_raw$AguaPotable)
table(retornados$AguaPotable)

table(retornados_raw$TratamientoAgua)
table(retornados$TratamientoAgua)

table(retornados_raw$ServicioSanitario)
table(retornados$ServicioSanitario)

table(retornados_raw$CocinarLenaResiduos)
table(retornados$CocinarLenaResiduos)

### Control de ingreso
retornados_raw %>%
  select(NumeroPersonasNucleo, NumeroMiembro) %>% 
  skimr::skim() # reemplazar el percentil 99
table(retornados_raw$NumeroPersonasNucleo)
table(retornados_raw$NumeroMiembro)

unique(retornados_raw$DelegacionIngreso)
unique(retornados_raw$FronteraSalida)
unique(retornados_raw$CAMR)
unique(retornados_raw$OtraFronteraSalida)

# Elimino edades, años y meses de salida negativos  
retornados <- retornados %>% 
  filter(MesUltimaSalidaPais>0, AnioUltimaSalidaPais>0, EdadAlRetornar>0, EdadActual>0)

# Nombres limpios
retornados <- retornados %>% 
  janitor :: clean_names()

# Ids unicos
length(unique(retornados$id_personas))
retornados <- retornados %>% 
  arrange(id_personas, fecha_arribo)

# ---> 2.3) Guardo la data limpia
write_csv(retornados, paste(data_path, "/retornados_clean.csv", sep = ""))

  