# ----------------------------- Retornados Honduras ---------------------------#
#                                Limpieza de data                              #

#install.packages("tidyr")
#install.packages("psych") 
#install.packages("skimr")
#install.packages("janitor")
#install.packages(c("tidytext", "stopwords"))
#install.packages("stringi") 
#install.packages("stringdist")

library(lubridate)
library(stringdist)
library(stringi)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(psych)
library(tidytext)
library(stringr)
library(stopwords)

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
                       labels = c("Mujeres", "Hombres")),
    EstadoCivi = case_when(EstadoCivi == "Soltero/a"     ~ 1,
                               EstadoCivi == "Unión Libre"   ~ 2,
                               EstadoCivi == "Casado/a"      ~ 3,
                               EstadoCivi == "Separado/a"    ~ 4,
                               EstadoCivi == "Divorciado/a"  ~ 5,
                               EstadoCivi == "Viudo/a"       ~ 6,
                               TRUE                          ~ NA_real_),
    EstadoCivi = factor(EstadoCivi,levels = 1:6, 
                        labels = c("Soltero", "Unión Libre", "Casado",
                                    "Separado", "Divorciado", "Viudo")),
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
class(retornados_raw$PaisDirigia)
table(retornados_raw$OtroPaisDirigia)
table(retornados_raw$VecesIrregularIngPaisRetorno)
table(retornados_raw$VecesLegalIngPaisRetornado)
table(retornados_raw$`Violencia Intrafamiliar`)
table(retornados_raw$`Violencia Domestica`)
table(retornados_raw$`Violencia en el Hogar`)
table(retornados_raw$Salud)
table(retornados_raw$Estudios)
table(retornados_raw$`Razones Económicas`)
table(retornados_raw$`Razones Familiares (Reunificación Familiar)`)
table(retornados_raw$`Explicación de sus derechos y procedimiento de retorno`)
table(retornados_raw$Alimentación)
table(retornados_raw$`Le permitieron comunicarse con su familia`)
table(retornados_raw$`Un lugar adecuando para descanso`)
table(retornados_raw$`Un buen trato al dirigirse a usted`)
table(retornados_raw$`Atención de salud cuando lo requirío`)
table(retornados_raw$`Ninguna Atencion de Migracion`)      
table(retornados_raw$`Contacto (telefónico, personal, skype)`)
table(retornados_raw$`Información sobre su deportación o situación migratoria`)
table(retornados_raw$`Asesoría general`)
table(retornados_raw$`Ninguna Atencion por Consulado Hondureño`)
        
retornados <- retornados %>% 
  mutate(
    # Variables factor
    SolicitoProteccion = case_when(SolicitoProteccion == "No"                                       ~ 1,
                                   SolicitoProteccion == "Si, pero abandone el proceso."            ~ 2,
                                   SolicitoProteccion == "Si, pero me la denegaron y me deportaron" ~ 3,
                                   TRUE                                                      ~ NA_real_),
    SolicitoProteccion = factor(SolicitoProteccion, levels = 1:3, 
                                labels = c("No","Si y abandonó", "Si y la denegaron")),
    `Violencia Domestica` = ifelse(`Violencia Domestica`==1 | `Violencia en el Hogar` == 1 
                                   | `Violencia Intrafamiliar`==1, 1,0),
    PaisDirigia = case_when(PaisDirigia == "USA"       ~ 1,
                            PaisDirigia == "México"    ~ 2,
                            PaisDirigia == "Guatemala" ~ 3,
                            PaisDirigia ==  "Otro"     ~ 4,
                            TRUE                       ~ NA_real_),
    PaisDirigia = factor(PaisDirigia, levels = 1:4, labels = c("USA", "México",
                                                               "Guatemala", "Otro")),
    # Variables numéricas
    VecesIrregularIngPaisRetorno = as.numeric(VecesIrregularIngPaisRetorno),
    VecesLegalIngPaisRetornado = as.numeric(VecesLegalIngPaisRetornado)
  ) %>% 
  rename(
    explicacion_derechos = `Explicación de sus derechos y procedimiento de retorno`,
    comunicacion_flia = `Le permitieron comunicarse con su familia`,
    lugar_descanso = `Un lugar adecuando para descanso`,
    buen_trato = `Un buen trato al dirigirse a usted`,
    atencion_salud = `Atención de salud cuando lo requirío`,
    no_atencion_migracion = `Ninguna Atencion de Migracion`,
    contacto = `Contacto (telefónico, personal, skype)`,
    informacion = `Información sobre su deportación o situación migratoria`,
    asesoria_gral = `Asesoría general`,
    no_atencion_consulado = `Ninguna Atencion por Consulado Hondureño`
  ) %>% 
  mutate(
    atenciones = case_when(explicacion_derechos  == 1 ~ 1,
                           comunicacion_flia     == 1 ~ 2,
                           lugar_descanso        == 1 ~ 3,
                           buen_trato            == 1 ~ 4,
                           atencion_salud        == 1 ~ 5,
                           contacto              == 1 ~ 6,
                           informacion           == 1 ~ 7,
                           asesoria_gral         == 1 ~ 8,
                           no_atencion_migracion == 1 ~ 9,
                           no_atencion_consulado == 1 ~ 10,
                           TRUE  ~ NA),
    atenciones = factor(atenciones, levels = 1:10,
                        labels = c("Explicación de sus derechos \ny procedimiento de retorno",
                        "Le permitieron comunicarse con su familia", "Un lugar adecuando para descanso",
                        "Un buen trato al dirigirse a usted", "Atención de salud cuando lo requirío",
                        "Contacto (telefónico, personal, skype)", "Información sobre su situación migratoria",
                        "Asesoría general", "Ninguna atencion de migracion", "Ninguna Atencion por Consulado Hondureño"))
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
table(retornados_raw$Ingles)
table(retornados_raw$ConocimientoInformatica)
table(retornados_raw$OtroConocimiento)

retornados <- retornados %>% 
  mutate(
    `Nivel Educativo Raw` = `Nivel Educativo`,
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
                                          "Universitaria")),
    
    # Creo el nivel educativo agrupado
    nivel_ed_ag = case_when(`Nivel Educativo Raw`== "No realizó estudios"            ~ 1,
                            `Nivel Educativo Raw`== "Ninguno"                        ~ 1,
                            `Nivel Educativo Raw`== "Pre Escolar (1-3)"              ~ 2,
                            `Nivel Educativo Raw`== "Básica (Elementary)"            ~ 2,
                            `Nivel Educativo Raw`== "Primaria (1-6)"                 ~ 2,
                            `Nivel Educativo Raw`== "Básica III ciclo (Junior High)" ~ 3,
                            `Nivel Educativo Raw`== "Secundaria (1-6)"               ~ 3,
                            `Nivel Educativo Raw`== "Media (High School)"            ~ 3,
                            `Nivel Educativo Raw`== "Técnico (1-3)"                  ~ 4,
                            `Nivel Educativo Raw`== "Universitaria"                  ~ 4,
                            `Nivel Educativo Raw`== "Universitario (1-8)"            ~ 4,
                            TRUE                                                  ~ NA_real_),
    nivel_ed_ag = factor(nivel_ed_ag, levels = 1:4, labels = 
                           c("Ninguno", "Pre-escolar o Primaria", "Secundaria", "Superior")),
    
    ConocimientoInformatica = ifelse(ConocimientoInformatica==2, 0, 1),
    ingles_informatica = case_when(Ingles==1 & ConocimientoInformatica==1 ~ 1,
                                   Ingles==0 & ConocimientoInformatica==1 ~ 2,
                                   Ingles==1 & ConocimientoInformatica==0 ~ 3,
                                   Ingles==0 & ConocimientoInformatica==0 ~ 4,
                                   TRUE                                   ~ NA_real_),
    ingles_informatica = factor(ingles_informatica, levels = 1:4, 
                                labels = c("Inglés y informatica", "Informatica",
                                           "Inglés", "Ninguno"))
    
  )

# Chequeo
table(retornados_raw$`Nivel Educativo`)
table(retornados$`Nivel Educativo`)

### Trabajo / Datos económicos
unique(retornados_raw$DetalleOtroConocimiento)
unique(retornados_raw$ExperienciaLaboral) 
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
    
    emprendedurismo = case_when(DeseaEmprender     == 1 ~ 1,
                                fortalecer_h_emp   == 1 ~ 2,
                                legalizar_emp      == 1 ~ 3,
                                vincular_mercados  == 1 ~ 4,
                                vincular_servicios == 1 ~ 5,
                                TRUE                    ~ NA_real_),
    emprendedurismo = factor(emprendedurismo, levels = 1:5, 
                             labels = c("Desea emprender", "Dispuesto a fortalecer \nhabilidades empresariales",
                                        "Dispuesto a legalizar \nemprendimiento", "Dispuesto a vincular con \nmercados nacionales",
                                        "Dispuesto a vincular con \nservicios financieros")),
    
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

# CATEGORIAS DE EXPERIENCIA LABORAL
retornados <- retornados %>%
  mutate(
    exp_clean = ExperienciaLaboral %>%
      str_to_lower() %>%
      stri_trans_general("Latin-ASCII") %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  ) %>% 
  mutate(
    categoria_exp = case_when(
      # 1. Agricultura
      str_detect(exp_clean, "recolector|gricul|jonalero|pastor|campesino|granja|jornal|agr|agi|agric|campo|cultiv|sembrar") ~ 1,
      
      # 2. Construcción
      str_detect(exp_clean, "labanil|contr|co0ntrucion|obrero|contruccion|contrucion|vidrio|soldador|soldadu|carpintero|carpinteria|albañil|constru|alban|pintura|pint|soldador|estructura|techo") ~ 2,
      
      # 3. Transporte
      str_detect(exp_clean, "transporte|motorist|conductor|bus|camion|taxi|chofer") ~ 3,
      
      # 4. Comercio
      str_detect(exp_clean, "vendante|emplaste|cliente|cajer[ao]|comerc|venta|vendedor|tienda|comerciante|abarroteria") ~ 4,
      
      # 5. Trabajo Doméstico
      str_detect(exp_clean, "am de casa|casa|niñera|ninera|ama|cuidad|ama de casa|limpieza|domestica|casas") ~ 5,
      
      # 6. Industria
      str_detect(exp_clean, "empaque|empacador|maquila|fabrica|operari|costura|maquinista") ~ 6,
      
      # 7. Servicios de comida
      str_detect(exp_clean, "chef|pastel|cocina|panader|meser|comida|cociner|tortilla|restaurante|bar|cantina") ~ 7,
      
      # 8. Educación
      str_detect(exp_clean, "maestr[ao]|profesor|docente|educador|jardin") ~ 8,
      
      # 9. Servicios técnicos (electricista, mecánico)
      str_detect(exp_clean, "elevtricista|plomer|maca|mecanic|electric|tecnic|reparacion|electri") ~ 9,
      
      # 10. Seguridad
      str_detect(exp_clean, "guardia|seguridad|vigilante") ~ 10,
      
      # 11. Administracion
      str_detect(exp_clean, "atencion|public[ao]|administrativ|oficina") ~ 11,
      
      # 12. Arte, diseño, estetica
      str_detect(exp_clean, "estsilista|teatro|manicurista|unas|uñas|arte|artist|plastic[ao]|diseño|disenio|disenador|estilista") ~ 12,
      
      # 13. Estudiante
      str_detect(exp_clean, "estudiante|estud|univer") ~ 13,

      # 14. Profesionales
      str_detect(exp_clean, "pericto|abogado|administracion|contador|locutor|enfermer|bachill|enfermer[ao]|licencia|derecho|militar|licda|perito|arqueologo") ~ 14,
      
      # 15. Sin experiencia / Otro
      str_detect(exp_clean, "no|sin|sin experiencia|nada|ninguna|nula|ningun|solo en el campo|n o|n/o|no tiene") ~ 15,
      
      # Las que no matchean nada y son NA
      is.na(exp_clean) ~ NA_integer_,
      
      TRUE ~ 16
    ),
    
    categoria_exp = factor(categoria_exp, levels = 1:16,
                           labels = c("Agricultura", "Construcción", "Transporte", "Comercio",
                                      "Trabajo Doméstico", "Industria", "Servicios de comida",
                                      "Educación", "Servicios de reparación", "Seguridad",
                                      "Administración", "Arte, diseño, estética", "Estudiante", 
                                      "Profesional", "Sin experiencia", "Otro"))
  )

table(retornados$categoria_exp)
retornados %>% 
  filter(categoria_exp=="Otro / No clasificado", !is.na(exp_clean)) %>% 
  select(exp_clean) %>%  View()

sum(is.na(table(retornados$categoria_exp)))

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
    Cocinar_LR = ifelse(CocinarLenaResiduos== "Si", 1, 0),
    vivienda_precaria = ifelse(PisoTierra==1 | Paredes_BDM==1 | Techo_PD==1
                               | AguaPotable==0 | ServicioSanitario==0 |
                                 Cocinar_LR==1, 1, 0),
    AguaPotable_no = ifelse(AguaPotable==0, 1, 0),
    ServicioSanitario_no = ifelse(ServicioSanitario==0, 1, 0),
    
    # Cantidad de carencias (si todas son NA sigue con NA)
    num_carencias = if_else(
      if_all(c(PisoTierra, Paredes_BDM, Techo_PD, 
               AguaPotable_no, ServicioSanitario_no, Cocinar_LR),
             is.na),
      NA_integer_,  # si todos son NA → poner NA
      rowSums(across(c(PisoTierra, Paredes_BDM, Techo_PD, 
                       AguaPotable_no, ServicioSanitario_no, Cocinar_LR)), 
              na.rm = TRUE))
  ) %>% 
  select(-CocinarLenaResiduos, -ParedesBaharequeDesechosMadera, -TechoPajaDesecho)

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

retornados <- retornados %>% 
  mutate(FronteraSalida = as.factor(FronteraSalida))
levels(retornados$FronteraSalida)

# Fechas de salida y arribo
class(retornados_raw$FechaArribo)
min(retornados_raw$FechaArribo)
max(retornados_raw$FechaArribo)
class(retornados_raw$MesUltimaSalidaPais)
unique(retornados_raw$MesUltimaSalidaPais)
class(retornados_raw$AnioUltimaSalidaPais)
unique(retornados_raw$AnioUltimaSalidaPais)

retornados <- retornados %>% 
  mutate(
    # Corrijo los negativos
    AnioUltimaSalidaPais = ifelse(AnioUltimaSalidaPais<0, NA_real_, AnioUltimaSalidaPais),
    MesUltimaSalidaPais = ifelse(MesUltimaSalidaPais<0, NA_real_, MesUltimaSalidaPais),
    
    # Variables en formato fecha
    FechaArribo = as.Date(FechaArribo),
    ano_arribo = year(FechaArribo),
    ano_mes_arribo = make_date(year = year(FechaArribo), month = month(FechaArribo), day = 1),
    ano_mes_u_salida = make_date(year = as.integer(AnioUltimaSalidaPais),
                                    month = as.integer(MesUltimaSalidaPais),
                                    day = 1),
    
    # Diferencia
    meses_fuera = interval(ano_mes_u_salida, ano_mes_arribo) %/% months(1)
  )

table(retornados$meses_fuera)
round(prop.table(table(retornados$ano_arribo)) * 100, 1)

View(retornados[retornados$meses_fuera<0, c("IdPersonas", "ano_mes_arribo", "ano_mes_u_salida", "FechaArribo")])
# Remplazo los negativos por NA
retornados <- retornados %>% 
  mutate(
    meses_fuera = ifelse(meses_fuera<0, NA_Date_, meses_fuera)
  )

# Elimino edades, años y meses de salida negativos  
retornados <- retornados %>% 
  mutate(EdadAlRetornar = ifelse(EdadAlRetornar<0, NA_real_, EdadAlRetornar),
         EdadActual = ifelse(EdadActual<0, NA_real_, EdadActual))

# Nombres limpios
retornados <- retornados %>% 
  janitor :: clean_names()

# NAs
na <- retornados %>%
        summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Porcentaje_NA") %>%
        arrange(desc(Porcentaje_NA))

# Valores extremos
Hmisc::describe(retornados$ultimo_grado_aprobado)
p99_ultimo_grado <- quantile(retornados$ultimo_grado_aprobado, 
                             0.99, na.rm = TRUE)  # 12

retornados <- retornados %>% 
  mutate(ultimo_grado_r = ifelse(ultimo_grado_aprobado>p99_ultimo_grado, 
                                 NA, ultimo_grado_aprobado))

Hmisc::describe(retornados$edad_al_retornar)
p99_edad <- quantile(retornados$edad_al_retornar, 0.99, na.rm = TRUE)  # 54 (muy bajo)

retornados <- retornados %>% 
  mutate(edad_al_retornar_r = ifelse(edad_al_retornar>p99_edad, 
                                 NA, edad_al_retornar))

Hmisc::describe(retornados$meses_fuera)
p95_meses <- quantile(retornados$meses_fuera, 0.95, na.rm = TRUE) 
p95_meses
p98_meses <- quantile(retornados$meses_fuera, 0.98, na.rm = TRUE) 
p98_meses
p99_meses <- quantile(retornados$meses_fuera, 0.99, na.rm = TRUE) 
p99_meses

retornados <- retornados %>% 
  mutate(meses_fuera_r = ifelse(meses_fuera>p95_meses, 
                                     NA, meses_fuera))

# Ids unicos
length(unique(retornados$id_personas))

# nº de retornos
retornados <- retornados %>% 
  group_by(id_personas) %>%
  arrange(id_personas, fecha_arribo) %>%
  mutate(n_retorno = row_number(),
         total_retorno = n()) %>%
  ungroup() %>% 
  select(id_personas, numero_personas_nucleo, numero_miembro, fecha_arribo, n_retorno, total_retorno, everything())

# Outliers en nª de retornos
retornados <- retornados %>%
  arrange(id_personas, n_retorno) %>% 
  select(id_personas, n_retorno, total_retorno, everything())
  
Hmisc::describe(retornados$total_retorno)
p99_retornos <- quantile(retornados$total_retorno, 0.99, na.rm = TRUE)

retornados <- retornados %>% 
  mutate(total_retorno_r = ifelse(total_retorno>p99_retornos, 
                                  NA, total_retorno))

# Tipo de retornados en funcion de la cantidad de veces que retornaron
View(retornados[retornados$id_personas==166, ])

retornados <- retornados %>% 
  group_by(id_personas) %>%
  mutate(
    tipo_retorno = case_when(
      total_retorno == 1 ~ 1,
      total_retorno > 1 & n_retorno == 1 ~ 2,
      total_retorno > 1 & n_retorno == total_retorno ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(
    tipo_retorno = factor(tipo_retorno,
                          levels = c(1, 2, 3),
                          labels = c("Único retorno", "Primer retorno", "Último retorno")
    )
  )

View(retornados[retornados$id_personas==166, c("id_personas", "total_retorno", "n_retorno", "tipo_retorno")])
any(is.na(retornados$tipo_retorno))
View(retornados[is.na(retornados$tipo_retorno), c("id_personas", "total_retorno", "n_retorno", "tipo_retorno")])

# ---> 2.3) Guardo la data limpia
write_csv(retornados, paste(data_path, "/retornados_clean.csv", sep = ""))


  