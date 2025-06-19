# ----------------------------- Retornados Honduras ---------------------------#
install.packages("ggplot2")
install.packages("forcats")
install.packages("Hmisc")
install.packages("sf")
install.packages("ggspatial")
install.packages("showtext")
install.packages("patchwork")
library(patchwork)
library(showtext)
library(forcats)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(psych)
library(ggplot2)
library(sf)
library(ggspatial)

options(scipen = 999)

rm(list = ls())

# ----------> 1) Rutas y Data
path <- "/Users/bautistagianfrancisco/Library/Mobile Documents/com~apple~CloudDocs/Flor/Retornados"
data_path <- paste(path,"/data", sep = "")
setwd(path)

retornados <- read_csv(paste(data_path, "/retornados_clean.csv", sep = ""))

# Elimina todos los archivos de la carpeta
carpeta <- paste(path,"/output", sep = "")
unlink(list.files(carpeta, full.names = TRUE), recursive = FALSE)

# Agregar la fuente desde Google Fonts
font_add_google(name = "Source Sans Pro", family = "source")
showtext_auto()

# ----------> 2) Caracterización de retornados

# Distribución de retornos
data_plot <- retornados %>%
      select(id_personas, total_retorno_r) %>%
      distinct() %>%
      count(total_retorno_r, name = "n_personas") %>%
      arrange(total_retorno_r) %>% 
      mutate(
        pct_personas = round(n_personas / sum(n_personas) * 100, 4)  
      ) %>% 
  filter(!is.na(total_retorno_r))

p1<- ggplot(data_plot, aes(x = factor(total_retorno_r), y = pct_personas)) +
  geom_col(fill = "steelblue", color = "white", width = 0.8) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Cantidad de Retornos",
    y = "Porcentaje de Personas",
  ) +
  theme_classic(base_family = "source") +
  theme(axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.title = element_text(color="black", size=13),
        axis.text = element_text(color="black", size=13),
        panel.grid      = element_blank())
p1
ggsave("output/retornos.png", plot = p1, width = 8, height = 6, dpi = 300)

# Evolución por año
table(retornados$ano_arribo)

data_plot <- retornados %>% 
  group_by(ano_arribo) %>% 
  summarise(cant_anual = n()) %>% 
  arrange(ano_arribo) %>% 
  filter(ano_arribo>2005)

p2<- ggplot(data_plot, aes(x = ano_arribo, y = cant_anual)) +
  geom_col(fill = "steelblue", color = "white", width = 0.8) +
  geom_text(aes(label = scales::comma(cant_anual)), 
            vjust = -0.5, size = 3.5, color = "black") +
  labs(
    x = "Año",
    y = "Cantidad de retornos"
  ) +
  scale_x_continuous(breaks = seq(2014, 2025, 1)) +
  scale_y_continuous(breaks = seq(10000, 100000, 15000),
                     labels = scales::comma) +
  theme_classic(base_family = "source") +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title  = element_text(color = "black", size = 13),
    axis.text   = element_text(color = "black", size = 13),
  )
p2
ggsave("output/evolucion.png", plot = p2, width = 8, height = 6, dpi = 300)

# Variacion inetranual
retornados <- retornados %>%
  mutate(mes_arribo = month(fecha_arribo))

table(retornados$mes_arribo)

mayo_data <- retornados %>%
  filter(mes_arribo == 5) %>%
  group_by(ano_arribo) %>%
  summarise(retornos_mayo = n()) %>%
  filter(ano_arribo %in% c(2023:2025))

variacion <- junio_data %>%
  arrange(ano_arribo) %>%
  mutate(variacion_pct = (retornos_junio / lag(retornos_junio) - 1) * 100)

# Frontera de salida
resumen_fronteras <- retornados %>%
  filter(!is.na(frontera_salida)) %>% 
  count(frontera_salida, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) 

# Frontera terrestre o aerea
retornados <- retornados %>%
  filter(!is.na(frontera_salida)) %>%
  separate(frontera_salida, into = c("frontera_salida", "medio_salida"), sep = "/", remove = FALSE) %>%
  mutate(
    medio_salida = case_when(
      str_detect(medio_salida, regex("Aérea", ignore_case = TRUE)) ~ "Aérea",
      str_detect(medio_salida, regex("Terrestre", ignore_case = TRUE)) ~ "Terrestre",
      TRUE ~ NA_character_
    )
  )

resumen_medio <- retornados %>%
  count(medio_salida, name = "n") %>%
  mutate(pct = n / sum(n) * 100)

# ------> 2.1) Cageorizo a los retornados
retornados1 <- retornados %>% 
  filter(total_retorno ==1, ano_arribo %in% c(2024,2025))

na1 <- retornados1 %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Porcentaje_NA") %>%
  arrange(desc(Porcentaje_NA))


# ---> Información personal y familiar

# Edad al retornar
p4 <- ggplot(retornados1, aes(x = edad_al_retornar_r)) +
  geom_histogram(aes(y = ..count.. / sum(..count..) * 100),
                 binwidth = 10,
                 boundary = 0,
                 closed   = "left",
                 fill     = "steelblue",
                 color    = "white") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), 
                     labels = seq(0, 100, by = 10)) + # Ajustás el eje X para que ponga marcas cada 10 años
  labs( 
    x = "Edad",
    y = "Porcentaje de personas",
  ) +
  theme_classic(base_family = "source") +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size=13, color = "black"),
    axis.text       = element_text(size=13, color="black"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank()  
  )
p4
ggsave("output/edad.png", plot = p4, width = 8, height = 6, dpi = 300)

conteo_entre_20_y_30 <- retornados1 %>%
  filter(edad_al_retornar_r >= 20 & edad_al_retornar_r <= 29) %>%
  nrow()
conteo_total <- nrow(retornados1)
porcentaje_entre_20_y_30 <- (conteo_entre_20_y_30 / conteo_total) * 100
porcentaje_entre_20_y_30
cat("Porcentaje de personas entre 20 y 30 años:", round(porcentaje_entre_20_y_30, 2), "%\n")

conteo_menores_edad <- retornados1 %>%
  filter(edad_al_retornar_r <= 18) %>%
  nrow()
pct_menores_edad <- (conteo_menores_edad / conteo_total) * 100
cat("Porcentaje de menores de 18:", round(pct_menores_edad, 2), "%\n")

conteo_niños <- retornados1 %>%
  filter(edad_al_retornar_r <= 10) %>%
  nrow()
pct_niños <- (conteo_niños / conteo_total) * 100
cat("Porcentaje de menores de 10:", round(pct_niños, 2), "%\n")

# Sexo
data_plot <- retornados1 %>%
  filter(!is.na(sexo)) %>%        # eliminar NAs
  count(sexo, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  mutate(sexo = fct_reorder(sexo, pct))   # Reordenar niveles de menor a mayor porcentaje

p4 <- ggplot(data_plot, aes(x = sexo, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 3.5, color = "black") +
  labs(
    x = " ",
    y = "Porcentaje de personas",
  ) +
  theme_classic(base_family = "source") +                                  
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title  = element_text(size=13, color = "black"),
    axis.text   = element_text(size=13, color="black")
  )
p4
ggsave("output/sexo.png", plot = p4, width = 8, height = 6, dpi = 300)


# Edad por sexo
ggplot(retornados1, aes(x = edad_al_retornar_r)) +
  geom_histogram(aes(y = ..count.. / sum(..count..) * 100),
                 binwidth = 10,
                 boundary = 0,
                 closed   = "left",
                 fill     = "steelblue",
                 color    = "white") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    x     = "Edad al retornar",
    y     = "Porcentaje de personas (%)",
    title = "Distribución de edad al retornar, por sexo\n(por personas con un solo retorno)"
  ) +
  theme_classic() +
  theme(
    plot.title      = element_text(hjust = 0.5, size=11),
    panel.grid      = element_blank(),
    axis.title = element_text(size=11),
    axis.text       = element_text(color="black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 11, face = "bold", color = "black"),
  ) +
  facet_wrap(~ sexo)

# Estado civil
data_plot <- retornados1 %>%
  filter(!is.na(estado_civi)) %>%        # eliminar NAs
  count(estado_civi, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  mutate(estado_civi = fct_reorder(estado_civi, pct))   # Reordenar niveles de menor a mayor porcentaje

p5 <- ggplot(data_plot, aes(x = estado_civi, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 3.5, color="black") +
  labs(
    x = " ",
    y = "Porcentaje de personas"
  ) +
  theme_classic(base_family = "source") +                                  
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size=13, color="black"),
    axis.text = element_text(size=13, color="black")
  )

ggsave("output/estado_civil.png", plot = p5, width = 8, height = 6, dpi = 300)

# ---> Motivo de Migración / Salida / Retorno

# Razones
razones <- c("razones_economicas",
             "razones_familiares_reunificacion_familiar",
             "salud",
             "estudios",
             "violencia_domestica",
             "violencia_o_inseguridad")

data_plot <- retornados1 %>%            
  select(id_personas, all_of(razones)) %>%         
  pivot_longer(-id_personas,
               names_to  = "razon",
               values_to = "tiene") %>%         
  group_by(razon) %>%
  summarise(pct = mean(tiene, na.rm = TRUE) * 100) %>%  
  ungroup() %>%
  mutate(razon = fct_reorder(razon, pct))  

p6 <- ggplot(data_plot, aes(x = razon, y = pct)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.1, size = 3.5, color = "black") +
  scale_x_discrete(labels = c(
    razones_economicas = "Económicas",
    razones_familiares_reunificacion_familiar = "Reunificación\nfamiliar",
    salud = "Salud",
    estudios = "Estudios",
    violencia_domestica = "Violencia\ndoméstica",
    violencia_o_inseguridad = "Violencia o\ninseguridad"
  )) +
  coord_flip() +
  labs(
    x     = " ",
    y     = "Porcentaje de personas"
  ) +
  theme_classic(base_family = "source") +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size=13, color="black"),
    axis.text  = element_text(size=13, color="black")
  )
p6
ggsave("output/razones.png", plot = p6, width = 8, height = 6, dpi = 300)

# País al que se dirigen
data_plot <- retornados1 %>%
  filter(!is.na(pais_dirigia)) %>%        # eliminar NAs
  count(pais_dirigia, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  mutate(pais_dirigia = fct_reorder(pais_dirigia, pct))  


p7 <- ggplot(data_plot, aes(x = pais_dirigia, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 2), "%")),
            vjust = -0.5, size = 3.5, color = "black") +
  labs(
    x = " ",
    y = "Porcentaje de personas"
  ) +
  theme_classic(base_family = "source") +                                  
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size=13, color = "black"),
    axis.text  = element_text(size=13, color="black")
  )
p7
ggsave("output/pais_dirigia.png", plot = p7, width = 8, height = 6, dpi = 300)

# Asistencias durante el proceso de migracion
data_plot <- retornados1 %>%
  filter(!is.na(atenciones)) %>%        # eliminar NAs
  count(atenciones, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>% 
  mutate(atenciones = fct_reorder(atenciones, pct))

data_plot <- data_plot %>%
  mutate(atenciones = case_when(
    atenciones == "Explicación de sus derechos y procedimiento de retorno" ~ 
    "Explicación de sus derechos \ny procedimiento de retorno",
    atenciones == "Le permitieron comunicarse con su familia" ~ 
      "Le permitieron comunicarse \ncon su familia",
    atenciones == "Ninguna Atencion por Consulado Hondureño" ~
      "Ninguna Atencion por Consulado \nHondureño",
    atenciones == "Información sobre su situación migratoria" ~
      "Información sobre su situación \nigratoria",
    atenciones == "Contacto (telefónico, personal, skype)" ~
      "Contacto (telefónico, personal, \nskype)",
    atenciones == "Atención de salud cuando lo requirío" ~
      "Atención de salud cuando lo \nrequirío",
    atenciones == "Un lugar adecuando para descanso" ~
      "Un lugar adecuando para \ndescanso",
    TRUE ~ atenciones),
    atenciones = fct_reorder(atenciones, pct))

p8 <- ggplot(data_plot, aes(x = atenciones, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.1, size = 3.5, color = "black") +
  labs(
    x = "Tipo de atención",
    y = "Porcentaje de personas (%)",
  ) +
  theme_classic(base_family = "source") +                                  
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size=13, color = "black"),
    axis.text  = element_text(size = 13, color="black")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  coord_flip()
p8
ggsave("output/asistencias.png", plot = p8, width = 8, height = 6, dpi = 300)

# Asistencias durante el proceso de migracion por sexo
data_plot <- retornados1 %>%
  filter(!is.na(atenciones), !is.na(sexo)) %>% 
  count(atenciones, sexo, name = "n_personas") %>%
  group_by(sexo) %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  ungroup()

data_plot <- data_plot %>%
  mutate(atenciones = case_when(
    atenciones == "Explicación de sus derechos y procedimiento de retorno" ~ 
      "Explicación de sus derechos \ny procedimiento de retorno",
    atenciones == "Le permitieron comunicarse con su familia" ~ 
      "Le permitieron comunicarse \ncon su familia",
    atenciones == "Ninguna Atencion por Consulado Hondureño" ~
      "Ninguna Atencion por Consulado \nHondureño",
    atenciones == "Información sobre su situación migratoria" ~
      "Información sobre su situación \nmigratoria",
    atenciones == "Contacto (telefónico, personal, skype)" ~
      "Contacto (telefónico, personal, \nskype)",
    atenciones == "Atención de salud cuando lo requirío" ~
      "Atención de salud cuando lo \nrequirío",
    atenciones == "Un lugar adecuando para descanso" ~
      "Un lugar adecuando para \ndescanso",
    TRUE ~ atenciones),
    atenciones = fct_reorder(atenciones, pct)
  ) %>% 
  mutate(sexo = factor(sexo, levels = c("Mujeres", "Hombres")))

p9 <- ggplot(data_plot, aes(x = atenciones, y = pct, fill = sexo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(width = 0.8),
            hjust = -0.2, size = 3.5, color = "black") +
  scale_fill_manual(
    values = c("Hombres" = "steelblue", "Mujeres" = "lightsteelblue"),
    breaks = c("Hombres", "Mujeres")  # orden explícito en la leyenda
  ) +
  labs(
    x = "Tipo de atención",
    y = "Porcentaje de personas",
    fill = " "
  ) +
  theme_classic(base_family = "source") +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size = 13, color = "black"),
    axis.text = element_text(size = 13, color = "black"),
    legend.text = element_text(size = 13, color = "black"),
    legend.position = "bottom"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  coord_flip()
p9

ggsave("output/atenciones_sexo.png", plot = p9, width = 8, height = 6, dpi = 300)

# Asistencias durante el proceso de migracion por grupo etario
retornados1 <- retornados1 %>%
  mutate(grupo_edad = case_when(
    edad_al_retornar >= 0  & edad_al_retornar <= 14  ~ "Niños",
    edad_al_retornar >= 15 & edad_al_retornar <= 29  ~ "Jóvenes",
    edad_al_retornar >= 30                           ~ "Adultos",
    TRUE ~ NA_character_),
    grupo_edad = factor(grupo_edad,
                        levels = c("Niños", "Jóvenes", "Adultos")))

data_plot <- retornados1 %>%
  filter(!is.na(atenciones), !is.na(grupo_edad)) %>% 
  count(atenciones, grupo_edad, name = "n_personas") %>%
  group_by(grupo_edad) %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  ungroup()

data_plot <- data_plot %>%
  mutate(atenciones = case_when(
    atenciones == "Explicación de sus derechos y procedimiento de retorno" ~ 
      "Explicación de sus derechos \ny procedimiento de retorno",
    atenciones == "Le permitieron comunicarse con su familia" ~ 
      "Le permitieron comunicarse \ncon su familia",
    atenciones == "Ninguna Atencion por Consulado Hondureño" ~
      "Ninguna Atencion por Consulado \nHondureño",
    atenciones == "Información sobre su situación migratoria" ~
      "Información sobre su situación \nmigratoria",
    atenciones == "Contacto (telefónico, personal, skype)" ~
      "Contacto (telefónico, personal, \nskype)",
    atenciones == "Atención de salud cuando lo requirío" ~
      "Atención de salud cuando lo \nrequirío",
    atenciones == "Un lugar adecuando para descanso" ~
      "Un lugar adecuando para \ndescanso",
    TRUE ~ atenciones),
    atenciones = fct_reorder(atenciones, pct)
  )

p10 <- ggplot(data_plot, aes(x = atenciones, y = pct, fill = grupo_edad)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(width = 0.8),
            hjust = -0.2, size = 3.5, color = "black") +
  labs(
    x = "Tipo de atención",
    y = "Porcentaje de personas",
    fill = " "
  ) +
  scale_fill_manual(
    values = c("Niños" = "steelblue", "Jóvenes" = "lightsteelblue", "Adultos" = "steelblue4"),
    breaks = c("Adultos", "Jóvenes", "Niños")) +
  theme_classic(base_family = "source") +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size = 13, color = "black"),
    axis.text = element_text(size = 13, color = "black"),
    legend.text = element_text(size = 13, color = "black"),
    legend.position = "bottom"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  coord_flip()
p10

ggsave("output/atenciones_edad.png", plot = p10, width = 8, height = 6, dpi = 300)

# A dónde se dirigen
table(retornados1$departemento_dirige)

dptos <- read_sf("data/cartografia/departamentos_hn.shp") %>% 
 mutate(departemento_dirige = str_to_upper(NOMBRE),
        departemento_dirige = str_squish(departemento_dirige)) 

data_plot <- retornados1 %>%
  filter(!is.na(departemento_dirige)) %>%        # eliminar NAs
  count(departemento_dirige, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  mutate(departemento_dirige = fct_reorder(departemento_dirige, pct),
         departemento_dirige = str_squish(departemento_dirige))  

dptos_plot <- data_plot %>% 
  full_join(dptos, by = "departemento_dirige") %>% 
  filter(!is.na(departemento_dirige))

dptos_plot <- st_as_sf(dptos_plot)
st_geometry(dptos_plot)

p11 <- ggplot(data = dptos_plot) +
  geom_sf(aes(fill = pct), color = "white", size = 0.2) +
  scale_fill_gradient(low = "#deebf7", high = "steelblue", 
                      name = "Porcentaje de personas") +
  labs(title = "Departemento de destino") + 
  theme_minimal() +
  annotation_scale(
    location = "bl",
    width_hint = 0.2,     #  ancho
    height = unit(0.2, "cm"),  # altura
    style = "bar",
    text_cex = 0.6
  ) +
  theme_void(base_family = "source") +
  theme(
    plot.title =  element_text(hjust = 0.5, size = 13, color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 12, color = "black"),
    legend.title =  element_text(size = 13, color = "black")
  )
p11

# Departaentos de origen
table(retornados1$departemento_nacimiento)

dptos <- dptos %>% 
  rename(departemento_nacimiento = departemento_dirige) 

data_plot <- retornados1 %>%
  filter(!is.na(departemento_nacimiento)) %>%        # eliminar NAs
  count(departemento_nacimiento, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  mutate(departemento_nacimiento = fct_reorder(departemento_nacimiento, pct),
         departemento_nacimiento = str_squish(departemento_nacimiento))  

dptos_plot <- data_plot %>% 
  full_join(dptos, by = "departemento_nacimiento") %>% 
  filter(!is.na(departemento_nacimiento))

dptos_plot <- st_as_sf(dptos_plot)
st_geometry(dptos_plot)

p12 <- ggplot(data = dptos_plot) +
  geom_sf(aes(fill = pct), color = "white", size = 0.2) +
  scale_fill_gradient(low = "#deebf7", high = "steelblue", name = "Porcentaje de personas") +
  theme_minimal() +
  labs(title = "Departemento de nacimiento") + 
  annotation_scale(
    location = "bl",
    width_hint = 0.2,     #  ancho
    height = unit(0.2, "cm"),  # altura
    style = "bar",
    text_cex = 0.6
  ) +
  theme_void(base_family = "source") +
  theme(
    plot.title =  element_text(hjust = 0.5, size = 13, color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 12, color = "black"),
    legend.title =  element_text(size = 13, color = "black")
  ) # muy parecido al que se dirigen

p11_12 <- p12 | p11
p11_12
ggsave("output/deptos.png", plot = p11_12, width = 10, height = 6, dpi = 300)

# ---> Educación

# Nivel educativo
data_plot <- retornados1 %>%
  filter(!is.na(nivel_ed_ag)) %>%        # eliminar NAs
  count(nivel_ed_ag, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) 

p13 <- ggplot(data_plot, aes(x = nivel_ed_ag, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 3.5, color= "black") +
  labs(
    x = "Nivel educativo máximo",
    y = "Porcentaje de personas"
  ) +
  theme_classic(base_family = "source") +                                  
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size = 13, color = "black"),
    axis.text = element_text(size = 13, color = "black")
  )
p13
ggsave("output/educ.png", plot = p13 , width = 8, height = 6, dpi = 300)

# Último grado aprobado
data_plot <- retornados1 %>%
  filter(!is.na(ultimo_grado_r)) %>%        # eliminar NAs
  count(ultimo_grado_r, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) 

ggplot(data_plot, aes(x = ultimo_grado_r, y = pct)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 3) +
  labs(
    x = "Último grado aprobado",
    y = "Porcentaje de personas (%)",
    title = "Último grado aprobado\n(por personas con un solo retorno)"
  ) +
  theme_classic() +                                  
  theme(
    plot.title = element_text(hjust = 0.5, size=11)           
  )

# Conocimiento ingles e informatica
data_plot <- retornados1 %>%
  filter(!is.na(ingles_informatica)) %>%        # eliminar NAs
  count(ingles_informatica, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>% 
  mutate(ingles_informatica = fct_reorder(ingles_informatica, pct))

p14 <- ggplot(data_plot, aes(x = ingles_informatica, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 3.5, color="black") +
  labs(
    x = "Tipo de conocimiento",
    y = "Porcentaje de personas"
  ) +
  theme_classic(base_family = "source") +                                  
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size = 13, color = "black"),
    axis.text = element_text(size = 13, color = "black")
  )
p14
ggsave("output/conocimiento.png", plot = p14, width = 8, height = 6, dpi = 300)

# ---> Trabajo

# Experiencia laboral

sum(is.na(table(retornados1$categoria_exp)))
sum(is.na(table(retornados$categoria_exp)))

data_plot <- retornados1 %>%
  filter(!is.na(categoria_exp)) %>%        # eliminar NAs
  count(categoria_exp, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  mutate(categoria_exp = fct_reorder(categoria_exp, pct))  

p15 <- ggplot(data_plot, aes(x = categoria_exp, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.1, size = 3.5, color = "black") +
  labs(
    x = "Sector de experiencia laboral",
    y = "Porcentaje de personas",
  ) +
  theme_classic(base_family = "source") +                                  
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size = 13, color = "black"),
    axis.text = element_text(size = 13, color = "black")
  )+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  coord_flip()
p15
ggsave("output/experiencia.png", plot = p15, width = 8, height = 6, dpi = 300)

# Perspectivas laborales
sum(is.na(retornados1$emprendedurismo))

data_plot <- retornados1 %>%
  filter(!is.na(emprendedurismo)) %>%        # eliminar NAs
  count(emprendedurismo, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  mutate(emprendedurismo = fct_reorder(emprendedurismo, pct))  # tiene muchos NA

# Evolución en el tiempo
data_plot <- retornados %>%
  filter(!is.na(categoria_exp)) %>% 
  group_by(ano_arribo, categoria_exp) %>%
  summarise(n_personas = n(), .groups = "drop") %>%
  group_by(ano_arribo) %>%
  mutate(
    pct = n_personas / sum(n_personas) * 100
  ) %>%
  slice_max(pct, n = 5) %>%
  ungroup() %>%
  mutate(
    categoria_exp = as.factor(categoria_exp),
    categoria_exp = fct_rev(fct_reorder(categoria_exp, pct))  # ordena de mayor a menor
  ) %>% 
  filter(ano_arribo>2005)

 p22 <- ggplot(data_plot, aes(x = fct_rev(fct_inorder(categoria_exp)), y = pct, fill = categoria_exp)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.2, size = 4, color = "black") +
  facet_wrap(~ ano_arribo, ncol = 3, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "#f0a390",
      "#8ecae6",
      "steelblue",
      "#023047",
      "#287271",
      "#9ee493",
      "#e9c46a"
    )
  ) +
  labs(
    x = NULL,
    y = "Porcentaje de retornos",
    fill = " "
  ) +
  theme_classic(base_family = "source") +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.text.y  = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 13, color = "black"),
    axis.title   = element_text(size = 13),
    legend.text  = element_text(size = 13),
    strip.text   = element_text(size = 13)
  ) 

ggsave("output/experiencia_evol.png", plot = p22, width = 12, height = 6, dpi = 300)


# ---> Bienestar

# Embarazo
data_plot <- retornados1 %>%
  filter(!is.na(embarazada)) %>%        # eliminar NAs
  count(embarazada, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) 

p16 <- ggplot(data_plot, aes(x = embarazada, y = pct)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 4, color= "black") +
  labs(
    x = "",
    y = "Porcentaje de mujeres",
    title = "¿Está embarazada?"
  ) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Sí"))+
  theme_classic(base_family = "source") +                                  
  theme(
    plot.title = element_text(size = 14, color = "black", hjust = 0.5),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 14, color = "black")
  )
p16
ggsave("output/vivenda_propia.png", plot = p16, width = 8, height = 6, dpi = 300)

# Vivienda propia
data_plot <- retornados1 %>%
  filter(!is.na(vivienda_propia)) %>%        # eliminar NAs
  count(vivienda_propia, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) 

p17 <- ggplot(data_plot, aes(x = vivienda_propia, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 3.5, color= "black") +
  labs(
    x = "",
    y = "Porcentaje de personas",
  ) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Sí"))+
  theme_classic(base_family = "source") +                                  
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size = 13, color = "black"),
    axis.text = element_text(size = 13, color = "black")
  )
p17
ggsave("output/vivenda_propia.png", plot = p17, width = 8, height = 6, dpi = 300)


# Carencias en la vivienda
retornados1 %>% 
  summarise(pct_con_alguna = mean(num_carencias > 0, na.rm = TRUE) * 100)

  # Distribucipon de carencias
data_plot <- retornados1 %>%
  filter(!is.na(num_carencias)) %>%        # eliminar NAs
  count(num_carencias, name = "n_personas") %>%
  mutate(suma = sum(n_personas),
         pct = n_personas / sum(n_personas) * 100) 

data_plot %>%
  filter(num_carencias >= 2) %>%
  summarise(pct_total = sum(pct))

p18 <- ggplot(data_plot, aes(x = num_carencias, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 4.5, color="black") +
  labs(
    x = "Cantidad de carencias",
    y = "Porcentaje de personas",
    title = "Cantidad de carencias en la vivienda"
  ) +
  scale_x_continuous(breaks = seq(0, 6, by = 1)) +
  theme_classic(base_family = "source") +                                  
  theme(
    plot.title = element_text(size = 15, color = "black", hjust = 0.5),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    axis.text = element_text(size = 15, color = "black")
  )
p18

  # Moda de carencias
data_long <- retornados1 %>%
  pivot_longer(cols = c(agua_potable_no, cocinar_lr, piso_tierra, paredes_bdm,
                        techo_pd, servicio_sanitario_no),
               names_to = "carencia",
               values_to = "presente") %>% 
  select(id_personas, numero_personas_nucleo, numero_miembro, carencia, presente)

data_plot <- data_long %>%
  group_by(carencia) %>%
  summarise(pct = mean(presente, na.rm = TRUE) * 100)

table(data_long$presente, useNA = "ifany")
table(retornados1$num_carencias, useNA = "ifany")

p19 <- ggplot(data_plot, aes(x = reorder(carencia, -pct), y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.1, 
            size = 4.5, color = "black") +
  labs(
    x = "Carencia",
    y = "Porcentaje de personas",
    title = "Distribución de carencias en la vivienda"
  ) +
  scale_x_discrete(labels = c(
    agua_potable_no = "Sin agua potable",
    cocinar_lr = "Cocina con leña \no desechos",
    piso_tierra = "Piso de tierra",
    paredes_bdm = "Paredes de \ndesechos o madera",
    techo_pd = "Techo de paja \no desechos" ,
    servicio_sanitario_no = "Sin sanitario"
  )) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  theme_classic(base_family = "source")+
  theme(
    plot.title = element_text(size = 15, color = "black", hjust = 0.5),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    axis.text = element_text(size = 15, color = "black")
  )
p19
p18_19 <- p18 + p19 
p18_19
ggsave("output/vivenda_carencias.png", plot = p18_19, width = 15.5, height = 6, dpi = 300)

# Vivienda precaria (al menos una carencia)
data_plot <- retornados1 %>%
  filter(!is.na(vivienda_precaria)) %>%        # eliminar NAs
  count(vivienda_precaria, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) 

ggplot(data_plot, aes(x = vivienda_precaria, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.2, size = 3) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Sí"))+
  labs(
    x = "Vivienda precaria",
    y = "Porcentaje de personas",
    title = "Vivienda con al menos una carencia\n(por personas con un solo retorno)"
  ) +
  theme_classic() +                                  
  theme(
    plot.title = element_text(hjust = 0.5, size=11)           
  )

# Problemas de salud
data_plot <- retornados1 %>%
  filter(!is.na(problema_salud)) %>%        # eliminar NAs
  count(problema_salud, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100)

p20 <- ggplot(data_plot, aes(x = problema_salud, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 4, color = "black") +
  labs(
    x = " ",
    y = "Porcentaje de personas",
    title = "¿Tiene un problema de salud?"
  ) +
  theme_classic(base_family = "source") +                                  
  theme(
    plot.title = element_text(size = 14, color = "black", hjust = 0.5),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 14, color = "black")
  )
p20

ggsave("output/salud.png", plot = p20, width = 8, height = 6, dpi = 300)

p20_16 <- p20 + p16
ggsave("output/indiadores_salud.png", plot = p20_16, width = 12, height = 6, dpi = 300)

# Discapacidades (el 99% no tiene)

# ---> Tiempo fuera del pais

# Dsitribucion en los ultimos dos años
retornados1 <- retornados1 %>%
  mutate(anios_fuera = ceiling(meses_fuera_r / 12),
         anios_fuera_c = round((meses_fuera_r / 12), 2))
table(retornados1$anios_fuera)
table(retornados1$anios_fuera_c)

table(retornados1$anios_fuera)

ggplot(retornados1, aes(x = anios_fuera)) +
  geom_bar(aes(y = ..count.. / sum(..count..) * 100),
           fill = "steelblue",
           color = "white",
           width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(
    breaks = seq(0, max(retornados1$anios_fuera_discretos, na.rm = TRUE), by = 1)
  ) +
  labs(
    x = "Cantidad de años",
    y = "Porcentaje de personas"
  ) +
  theme_classic(base_family = "source") +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title  = element_text(size = 13, color = "black"),
    axis.text   = element_text(size = 13, color = "black"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank()
  )

p21 <- ggplot(retornados1, aes(x = meses_fuera_r)) +
  geom_histogram(aes(y = ..count.. / sum(..count..) * 100),
                 binwidth = 6,  # cada bin cubre 6 meses
                 boundary = 0,
                 closed = "left",
                 fill = "steelblue",
                 color = "white") +
  scale_x_continuous(breaks = seq(0, 120, by = 6)) +
  scale_y_continuous(breaks = seq(0, 100, by = 15), labels = scales::percent_format(scale = 1)) +
  labs(
    x = "Meses fuera del país",
    y = "Porcentaje de personas"
  ) +
  theme_classic(base_family = "source")+
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title  = element_text(size = 13, color = "black"),
    axis.text   = element_text(size = 13, color = "black"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank()
  )
p21
ggsave("output/meses_fuera.png", plot = p21, width = 8, height = 6, dpi = 300)

# Evolución en el tiempo
promedios_tiempo <- retornados %>%
  group_by(ano_arribo) %>%
  summarise(
    prom_meses_fuera = mean(meses_fuera_r, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  filter(ano_arribo>2005)

retornados %>% filter(ano_arribo == 2015) %>% select(id_personas, ano_arribo, meses_fuera_r, 
                                                     fecha_arribo, ano_mes_u_salida, ano_mes_arribo,
                                                     anio_ultima_salida_pais, mes_ultima_salida_pais) %>%  View()
retornados_raw  %>% 
  mutate(ano_arribo = year(FechaArribo)) %>% 
  filter(ano_arribo == 2015) %>% 
  select(AnioUltimaSalidaPais, MesUltimaSalidaPais, FechaArribo) %>%
  View() # no hay data

ggplot(promedios_tiempo, aes(x = ano_arribo, y = prom_meses_fuera)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") +
  labs(
    x = "Fecha de arribo",
    y = "Años fuera del país"
  ) +
  theme_classic(base_family = "source") +
  theme(
    axis.title = element_text(size = 13),
    axis.text  = element_text(size = 12)
  )

retornados2025 <- retornados %>% filter(ano_arribo==2025)

Hmisc::describe(retornados2025$meses_fuera_r)

retornados <- retornados %>% 
  mutate(menos1ano_afuera = ifelse(meses_fuera<=12, 1,0),
         menos6mes_afuera = ifelse(meses_fuera<=6, 1,0))

table(retornados$menos1ano_afuera , retornados$ano_arribo)
table(retornados$ano_arribo)
table(retornados$meses_fuera, retornados$ano_arribo)

retornados %>%
  group_by(ano_arribo) %>%
  summarise(
    total_obs = n(),
    na_ano_mes_u_salida = sum(is.na(ano_mes_u_salida)),
    pct_na = mean(is.na(ano_mes_u_salida)) * 100
  ) # hay muchos NA en 2014 y 2015

promedios_tiempo <- retornados %>% 
  group_by(ano_arribo) %>%
  summarise(
    menos1ano_afuera_prom = mean(menos1ano_afuera, na.rm = TRUE),
    menos6mes_afuera_prom = mean(menos6mes_afuera, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  filter(ano_arribo>=2016)


data_long <- promedios_tiempo %>%
  pivot_longer(
    cols = c(menos1ano_afuera_prom, menos6mes_afuera_prom),
    names_to = "indicador",
    values_to = "valor"
  )

p22 <- ggplot(data_long, aes(x = ano_arribo, y = valor)) +
  geom_line(color = "steelblue", size = 1) +
  facet_wrap(~ indicador, ncol = 1, labeller = as_labeller(c(
    menos1ano_afuera_prom = "1 año o menos fuera",
    menos6mes_afuera_prom = "6 meses o menos fuera"
  ))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(min(data_long$ano_arribo), max(data_long$ano_arribo), 1)) +
  labs(
    x = "Año de arribo",
    y = "Porcentaje de retornos"
  ) +
  theme_classic(base_family = "source") +
  theme(
    strip.text = element_text(size = 13),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.title  = element_text(size = 13, color = "black"),
    axis.text   = element_text(size = 13, color = "black"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank()
  )
ggsave("output/meses_fuera_evol.png", plot = p22, width = 10, height = 6, dpi = 300)

# pais donde estudio

