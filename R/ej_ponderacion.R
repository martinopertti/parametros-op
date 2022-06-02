
## *************************************************************************
## Script ejemplo de post-estratificación 
## *************************************************************************

library(tidyverse)
library(anesrake)
library(srvyr)

## 1. Leer data ejemplo ----

# La base descargada del Roper Center, en formato Stata de Cifra junio 2015
data <- readstata13::read.dta13("data/cifra_0615_v13.dta",
                              convert.factors = TRUE,
                              nonint.factors=TRUE)

glimpse(data)


## 2. Recodificar variables ----

data <- data %>%
  rename(sd_edad = P2,
         sd_sexo = P1) %>% 
  mutate(sexedad = case_when(
    sd_edad >= 15 & sd_edad <= 39  & sd_sexo == "Masculino" ~ "Hombres_18_39",
    sd_edad >= 40 & sd_edad <= 59  & sd_sexo == "Masculino" ~ "Hombres_40_59",
    sd_edad >= 60 & sd_edad <= 110 & sd_sexo == "Masculino" ~ "Hombres_60_mas",
    sd_edad >= 15 & sd_edad <= 39  & sd_sexo == "Femenino" ~ "Mujeres_18_39",
    sd_edad >= 40 & sd_edad <= 59  & sd_sexo == "Femenino" ~ "Mujeres_40_59",
    sd_edad >= 60 & sd_edad <= 110 & sd_sexo == "Femenino" ~ "Mujeres_60_mas",
  )) %>% 
  mutate(sexedad = as.factor(sexedad)) %>% 
  mutate(sd_edu = case_when(
    P56 == "Ninguno" ~ "Primaria",
    P56 == "Primaria incompleta" ~ "Primaria",
    P56 == "Primaria completa" ~ "Primaria",
    P56 == "Secund., UTU incompleta" ~ "Secundaria",
    P56 == "Secund., UTU completa" ~ "Secundaria",
    P56 == "Terciaria NO Universitaria incompleta" ~ "Terciaria",
    P56 == "Terciaria NO Universitaria completa" ~ "Terciaria",
    P56 == "Universitaria completa" ~ "Terciaria",
    P56 == "Universitaria incompleta" ~ "Terciaria",
    P56 == "Posgrado" ~ "Terciaria",
  )) %>% 
  mutate(sd_edu = as.factor(sd_edu)) %>% 
  mutate(dpto_rec = case_when(
    dpto != "MONTEVIDEO" ~ "Interior",
    dpto == "MONTEVIDEO" ~ "Montevideo"
  )) %>% 
  mutate(dpto_rec = as.factor(dpto_rec)) %>% 
  mutate(id = row_number())


## 3. Leer y recodificar base con parametros ----

load(file = "data/data_parametros.rda")

par_15 <- base_parametros %>% 
  filter(anio == 2015) %>% 
  mutate(categoria = case_when(
    variable == "poblacion" & categoria != "Montevideo" ~ "Interior",
    TRUE ~ categoria
  )) %>% 
  group_by(categoria) %>% 
  summarise(anio = first(anio),
            parametro = sum(parametro),
            variable = first(variable))


## 4. Preparar vectores para la ponderación ----

sexedad <- par_15 %>% 
  filter(variable == "sexedad") %>% 
  select(parametro) %>% 
  pull()

names(sexedad) <- par_15 %>% 
  filter(variable == "sexedad") %>% 
  select(categoria) %>% 
  pull()

# Extraigo parámetro de educacion de base_parametros  
sd_edu <- par_15 %>% 
  filter(variable == "educacion") %>% 
  select(parametro) %>% 
  pull()

names(sd_edu) <- par_15 %>% 
  filter(variable == "educacion") %>% 
  select(categoria) %>% 
  pull()

# Extraigo parámetro de departamento de base_parametros  
dpto_rec <- par_15 %>%
  filter(variable == "poblacion") %>% 
  select(parametro) %>% 
  pull()

names(dpto_rec) <- par_15 %>% 
  filter(variable == "poblacion") %>% 
  select(categoria) %>% 
  pull()

# Diferencias entre muestra y parámetros
prop.table(table(data$dpto_rec))
dpto_rec

prop.table(table(data$sd_edu))
sd_edu

prop.table(table(data$sexedad))
sexedad


## 5. Ponderación ----
targets <- list(sexedad, sd_edu, dpto_rec)
names(targets) <- c("sexedad", "sd_edu", "dpto_rec")

outsave <- anesrake(targets, # Parametros 
                    data, # Base
                    caseid = data$id, # Identificador
                    cap = 5, # Lo trunco en 5 
                    pctlim = 0.05, # Diferencia de al menos 5
                    choosemethod = "total", 
                    verbose = FALSE, 
                    type = "nolim",
                    iterate = TRUE , 
                    force1 = TRUE)

# Resumen
summary(outsave)

# Pego ponderador a la base  
data <- data %>% 
  mutate(pond_sd = unlist(outsave[1]))


## 6. Chequeo con tablas ponderadas ----

# Declarar como encuesta con srvyr (incluyendo ponderador)
svy <- as_survey_design(data, weights = pond_sd) 

# Tabla con proporciones y comparación no ponderada
svy %>%
  group_by(dpto_rec) %>%
  summarize(proportion = survey_mean(),
            total = survey_total(),
            unw_n = unweighted(n()))



