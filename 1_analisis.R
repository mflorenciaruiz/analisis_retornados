# ----------------------------- Retornados Honduras ---------------------------#
install.packages("ggplot2")
install.packages("forcats")
install.packages("Hmisc")
install.packages("sf")
install.packages("ggspatial")
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

# ----------> 2) Caracterización de retornados

# Distribución de retornos
freq_retornos <- retornados %>%
      select(id_personas, total_retorno) %>%
      distinct() %>%
      count(total_retorno, name = "n_personas") %>%
      arrange(total_retorno) %>% 
      mutate(
        pct_personas = round(n_personas / sum(n_personas) * 100, 4)  
      )

p1<- ggplot(freq_retornos, aes(x = factor(total_retorno), y = pct_personas)) +
  geom_col(fill = "steelblue", color = "white", width = 0.8) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Cantidad de Retornos",
    y = "Porcentaje de Personas (%)",
    title = "Distribución de Retornos",
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size=11),
        axis.title = element_text(size=11),
        panel.grid      = element_blank(),
        axis.text = element_text(color="black"))

ggsave("output/p1.png", plot = p1, width = 8, height = 6, dpi = 300)

# ------> 2.1) Retornados una sola vez
retornados1 <- retornados %>% 
  filter(total_retorno ==1)

# ---> Información personal y familiar

# Edad al retornar
p2 <- ggplot(retornados1, aes(x = edad_al_retornar_r)) +
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
    y = "Personas",
    title = "Distribución de edad de retornados\n(por personas con un solo retorno)"
  ) +
  theme_classic() +
  theme(
    plot.title      = element_text(hjust = 0.5, size=11),
    panel.grid      = element_blank(),
    axis.title = element_text(size=11),
    axis.text       = element_text(color="black")
  )

ggsave("output/p2.png", plot = p2, width = 8, height = 6, dpi = 300)

# Sexo
data_plot <- retornados1 %>%
  filter(!is.na(sexo)) %>%        # eliminar NAs
  count(sexo, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  mutate(sexo = fct_reorder(sexo, pct))   # Reordenar niveles de menor a mayor porcentaje

p3 <- ggplot(data_plot, aes(x = sexo, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 3) +
  labs(
    x = " ",
    y = "Porcentaje de personas",
    title = "Sexo de los retornados\n(por personas con un solo retorno)"
  ) +
  theme_classic() +                                  
  theme(
    plot.title = element_text(hjust = 0.5, size=11),
    panel.grid      = element_blank(),
    axis.title = element_text(size=11),
    axis.text       = element_text(color="black")
  )

ggsave("output/p3.png", plot = p3, width = 8, height = 6, dpi = 300)

# Edad por sexo
p4 <- ggplot(retornados1, aes(x = edad_al_retornar_r)) +
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

ggsave("output/p4.png", plot = p4, width = 8, height = 6, dpi = 300)

# Estado civil
data_plot <- retornados1 %>%
  filter(!is.na(estado_civi)) %>%        # eliminar NAs
  count(estado_civi, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  mutate(estado_civi = fct_reorder(estado_civi, pct))   # Reordenar niveles de menor a mayor porcentaje

p5 <- ggplot(data_plot, aes(x = estado_civi, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 3) +
  labs(
    x = " ",
    y = "Porcentaje de personas",
    title = "Estado civil de los retornados\n(por personas con un solo retorno)"
  ) +
  theme_classic() +                                  
  theme(
    plot.title = element_text(hjust = 0.5, size=11),
    panel.grid      = element_blank(),
    axis.title = element_text(size=11),
    axis.text       = element_text(color="black")
  )

ggsave("output/p5.png", plot = p5, width = 8, height = 6, dpi = 300)

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
            hjust = -0.1, size = 3) +
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
    y     = "Porcentaje de personas",
    title = "Razones de migración\n(por personas con un solo retorno)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size=11),
    panel.grid = element_blank(),
    axis.title = element_text(size=11),
    axis.text  = element_text(color="black")
  )

ggsave("output/p6.png", plot = p6, width = 8, height = 6, dpi = 300)

# País al que se dirigen
data_plot <- retornados1 %>%
  filter(!is.na(pais_dirigia)) %>%        # eliminar NAs
  count(pais_dirigia, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  mutate(pais_dirigia = fct_reorder(pais_dirigia, pct))  


p7 <- ggplot(data_plot, aes(x = pais_dirigia, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 2), "%")),
            vjust = -0.5, size = 3) +
  labs(
    x = " ",
    y = "Porcentaje de personas",
    title = "País al que se dirigía\n(por personas con un solo retorno)"
  ) +
  theme_classic() +                                  
  theme(
    plot.title = element_text(hjust = 0.5, size=11),
    panel.grid = element_blank(),
    axis.title = element_text(size=11),
    axis.text  = element_text(color="black")
  )

ggsave("output/p7.png", plot = p7, width = 8, height = 6, dpi = 300)

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
            hjust = -0.1, size = 3) +
  labs(
    x = "Tipo de atención",
    y = "Porcentaje de personas (%)",
  ) +
  theme_classic() +                                  
  theme(
    plot.title = element_text(hjust = 0.5, size=11),
    panel.grid = element_blank(),
    axis.title = element_text(size=11),
    axis.text  = element_text(color="black")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  coord_flip()

ggsave("output/p8.png", plot = p8, width = 8, height = 6, dpi = 300)

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

st_geometry(dptos_plot)
dptos_plot <- st_as_sf(dptos_plot)

p9 <- ggplot(data = dptos_plot) +
  geom_sf(aes(fill = pct), color = "white", size = 0.2) +
  scale_fill_gradient(low = "#deebf7", high = "steelblue", name = "Porcentaje") +
  theme_minimal() +
  labs(title = "Distribución de pct por zona") +
  annotation_scale(
    location = "bl",
    width_hint = 0.2,     #  ancho
    height = unit(0.2, "cm"),  # altura
    style = "bar",
    text_cex = 0.6
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size =11),
    legend.position = "right"
  )

ggsave("output/p9.png", plot = p9, width = 8, height = 6, dpi = 300)

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

ggplot(data = dptos_plot) +
  geom_sf(aes(fill = pct), color = "white", size = 0.2) +
  scale_fill_gradient(low = "#deebf7", high = "steelblue", name = "Porcentaje") +
  theme_minimal() +
  labs(title = "Distribución de pct por zona") +
  annotation_scale(
    location = "bl",
    width_hint = 0.2,     #  ancho
    height = unit(0.2, "cm"),  # altura
    style = "bar",
    text_cex = 0.6
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size =11),
    legend.position = "right"
  ) # muy parecido al que se dirigen

# ---> Educación

# Nivel educativo
data_plot <- retornados1 %>%
  filter(!is.na(nivel_educativo)) %>%        # eliminar NAs
  count(nivel_educativo, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  mutate(nivel_educativo = fct_reorder(nivel_educativo, pct))   # Reordenar niveles de menor a mayor porcentaje

ggplot(data_plot, aes(x = nivel_educativo, y = pct)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 3) +
  labs(
    x = "Nivel educativo máximo",
    y = "Porcentaje de personas (%)",
    title = "Nivel educativo alcanzado\n(por personas con un solo retorno)"
  ) +
  theme_classic() +                                  
  theme(
    plot.title = element_text(hjust = 0.5, size=11)           
  )

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

p11 <- ggplot(data_plot, aes(x = ingles_informatica, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.2, size = 3) +
  labs(
    x = "Conocimiento",
    y = "Porcentaje de personas (%)",
    title = "Conocimientos en inglés e informatica\n(por personas con un solo retorno)"
  ) +
  theme_classic() +                                  
  theme(
    plot.title = element_text(hjust = 0.5, size=11),
    panel.grid = element_blank(),
    axis.title = element_text(size=11),
    axis.text  = element_text(color="black")
  )

ggsave("output/p11.png", plot = p11, width = 8, height = 6, dpi = 300)


# ---> Trabajo

# Experiencia laboral
data_plot <- retornados1 %>%
  filter(!is.na(categoria_exp)) %>%        # eliminar NAs
  count(categoria_exp, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) %>%
  mutate(categoria_exp = fct_reorder(categoria_exp, pct))  

p12 <- ggplot(data_plot, aes(x = categoria_exp, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.1, size = 3) +
  labs(
    x = "Sector de experiencia laboral",
    y = "Porcentaje de personas",
  ) +
  theme_classic() +                                  
  theme(
    plot.title = element_text(hjust = 0.5, size=11),
    panel.grid      = element_blank(),
    axis.title = element_text(size=11),
    axis.text       = element_text(color="black")
  )+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  coord_flip()
p12
ggsave("output/p12.png", plot = p12, width = 8, height = 6, dpi = 300)


# ---> Vivienda

# Vivienda propia
data_plot <- retornados1 %>%
  filter(!is.na(vivienda_propia)) %>%        # eliminar NAs
  count(vivienda_propia, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) 

p13 <- ggplot(data_plot, aes(x = vivienda_propia, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.3, size = 3) +
  labs(
    x = "",
    y = "Porcentaje de personas (%)",
    title = "Vivienda propia\n(por personas con un solo retorno)"
  ) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Sí"))+
  theme_classic() +                                  
  theme(
    plot.title = element_text(hjust = 0.5, size=11),
    panel.grid = element_blank(),
    axis.title = element_text(size=11),
    axis.text  = element_text(color="black")
  )
ggsave("output/p13.png", plot = p13, width = 8, height = 6, dpi = 300)

# Carencias en la vivienda
  # Distribucipon de carencias
data_plot <- retornados1 %>%
  filter(!is.na(num_carencias)) %>%        # eliminar NAs
  count(num_carencias, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100) 

p14 <- ggplot(data_plot, aes(x = num_carencias, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.3, size = 3) +
  labs(
    x = "Cantidad de carencias",
    y = "Porcentaje de personas",
    title = "Carencias en la vivienda\n(por personas con un solo retorno)"
  ) +
  scale_x_continuous(breaks = seq(0, 6, by = 1)) +
  theme_classic() +                                  
  theme(
    plot.title = element_text(hjust = 0.5, size=11),
    panel.grid = element_blank(),
    axis.title = element_text(size=11),
    axis.text  = element_text(color="black")
  )

ggsave("output/p14.png", plot = p14, width = 8, height = 6, dpi = 300)
  

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

p15 <- ggplot(data_plot, aes(x = reorder(carencia, -pct), y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.1, size = 3) +
  labs(
    title = "Porcentaje de personas con cada carencia de vivienda\n(por personas con un solo retorno)",
    x = "Carencia",
    y = "Porcentaje de personas"
  ) +
  scale_x_discrete(labels = c(
    agua_potable_no = "Sin agua potable",
    cocinar_lr = "Cocina con leña o\ndesechos",
    piso_tierra = "Piso de tierra",
    paredes_bdm = "Paredes de bahareque,\ndesechos o madera",
    techo_pd = "Techo de paja o\ndesechos" ,
    servicio_sanitario_no = "Sin sanitario"
  )) +
  coord_flip() +
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5, size=11) ,
    axis.text = element_text(color= "black")
  )

ggsave("output/p15.png", plot = p15, width = 8, height = 6, dpi = 300)

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

# ---> Salud

# Problemas de salud
data_plot <- retornados1 %>%
  filter(!is.na(problema_salud)) %>%        # eliminar NAs
  count(problema_salud, name = "n_personas") %>%
  mutate(pct = n_personas / sum(n_personas) * 100)

p16 <- ggplot(data_plot, aes(x = problema_salud, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.2, size = 3) +
  labs(
    x = " ",
    y = "Porcentaje de personas",
    title = "Problemas de salud\n(por personas con un solo retorno)"
  ) +
  theme_classic() +                                  
  theme(
    plot.title = element_text(hjust = 0.5, size=11)           
  )
ggsave("output/p16.png", plot = p15, width = 8, height = 6, dpi = 300)

# Discapacidades (el 99% no tiene)

# Primer retorno de todos los retornados


# Retornados más de una vez: primer retorno


# Retornados más de una vez: último retorno


