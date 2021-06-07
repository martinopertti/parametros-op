
## **************************************************************************
## Construcción de base de parámetros
## **************************************************************************

##  1. Paquetes =============================================================

library(tidyverse)
library(readstata13)
library(stringr)
library(iterators)
library(foreach)
library(anesrake)
library(Boreluy)
library(openxlsx)
library(tidyr)

rm(list = ls())

source("parametros-op/R/moda.R") # Cargo la función moda
source("parametros-op/R/theme_m.R") # Cargo la función moda

##  2. Sexedad (Censo)  =====================================================
d_sexedad <- read.xlsx("Censo/sexedad_censo.xlsx")

# Filtrar mayores a 18, agrupar por categorías de sexo y edad, calcular
# porcentajes 
d_sexedad <- d_sexedad %>% 
  pivot_longer(cols = "1996":"2050",
               values_to = "personas", 
               names_to = "anio") %>% 
  mutate(anio = as.numeric(anio)) %>% 
  mutate(edad = case_when(
    edad == "90 y más" ~ 91,
    edad != "90 y más" ~ as.numeric(edad))) %>% 
  filter(edad >= 18,
         anio <= 2020) %>% 
  mutate(categoria = case_when(
    edad >= 18 & edad <= 39  & sexo == "Hombres" ~ "Hombres_18_39",
    edad >= 40 & edad <= 59  & sexo == "Hombres" ~ "Hombres_40_59",
    edad >= 60 & edad <= 110 & sexo == "Hombres" ~ "Hombres_60_mas",
    edad >= 18 & edad <= 39  & sexo == "Mujeres" ~ "Mujeres_18_39",
    edad >= 40 & edad <= 59  & sexo == "Mujeres" ~ "Mujeres_40_59",
    edad >= 60 & edad <= 110 & sexo == "Mujeres" ~ "Mujeres_60_mas",
  )) %>% 
  group_by(anio, categoria) %>% 
  summarise(personas = sum(personas)) %>% 
  pivot_wider(names_from = categoria, values_from = personas) %>% 
  mutate(total = `Hombres_18_39` + `Hombres_40_59` + `Hombres_60_mas` + 
           `Mujeres_18_39` + `Mujeres_40_59` + `Mujeres_60_mas`) %>% 
  mutate(`Hombres_18_39` = `Hombres_18_39` / total * 100,
         `Hombres_40_59` = `Hombres_40_59` / total * 100,
         `Hombres_60_mas` = `Hombres_60_mas` / total * 100,
         `Mujeres_18_39` = `Mujeres_18_39` / total * 100,
         `Mujeres_40_59` = `Mujeres_40_59` / total * 100,
         `Mujeres_60_mas` = `Mujeres_60_mas` / total * 100) %>%
  pivot_longer(cols = `Hombres_18_39`:`Mujeres_60_mas`, 
               names_to = "categoria", 
               values_to = "parametro") %>% 
  select(-total) %>% 
  mutate(variable = "sexedad")
head(d_sexedad)

# Gráfico
ggplot(d_sexedad, 
       aes(x = anio, y = parametro, fill = categoria)) +
  geom_col(alpha = .8) +
  geom_text(aes(label = round(parametro, digits = 1)),
            position = position_stack(vjust = .5)) +
  theme_m() +
  theme(legend.position = "bottom") +
  labs(title = "Parámetros de sexo x edad",
       y = "% de la población mayor a 18 años",
       x = "") +
  scale_fill_brewer(name = "", palette = "RdYlBu")

ggsave("parametros-op/plots/sexedad.png", 
       width = 30, height = 20, units = "cm")



##  3. Población por dpto (Censo)  ==========================================
d_poblacion <- read.xlsx("Censo/poblacion_xdpto.xlsx")
d_poblacion <- d_poblacion %>% 
  pivot_longer(cols = "Montevideo":"Treinta.y.Tres", 
               names_to = "categoria",
               values_to = "parametro") %>% 
  mutate(variable = "poblacion")

# Gráfico
ggplot(d_poblacion %>% 
         mutate(dpto_rec = case_when(
           categoria %in% c("Montevideo", "Canelones", "Maldonado", "Salto") ~ categoria,
           TRUE ~ "Otros"
         )) %>% 
         group_by(anio, dpto_rec) %>% 
         summarise(parametro = sum(parametro)), 
       aes(x = anio, y = parametro, fill = fct_reorder(dpto_rec, parametro))) +
  geom_col(alpha = .8) +
  geom_text(aes(label = round(parametro, digits = 1)),
            position = position_stack(vjust = .5)) +
  theme_m() +
  theme(legend.position = "bottom") +
  labs(title = "Población por departamento",
       y = "",
       x = "") +
  scale_fill_brewer(name = "", palette = "Set2")

ggsave("parametros-op/plots/region.png", 
       width = 30, height = 20, units = "cm")

##  4. Educación (ECH)  =====================================================

load(file = "parametros-op/data/ech_combined.rda")

# 18 o mas
data <- ech_combined %>% 
  filter(bc_pe3 >= 18)

# Educacion ponderado (2007 interpolado)
d_edu <- data %>% 
  filter(bc_edu != -15) %>% 
  mutate(categoria = case_when(
    bc_edu <= 6 ~ "Primaria",
    bc_edu >= 7 & bc_edu <= 12 ~ "Secundaria",
    bc_edu >= 13 ~ "Terciaria"
  )) %>% 
  filter(!is.na(categoria)) %>% 
  select(bc_anio, categoria, bc_pesoan) %>% 
  group_by(bc_anio, categoria) %>% 
  summarise(n = sum(bc_pesoan, na.rm = TRUE)) %>% 
  mutate(parametro = round(n / sum(n), digits = 4) * 100) %>% 
  select(-n) %>% 
  rename(anio = bc_anio) %>% 
  mutate(variable = "educacion") 

# Gráfico
ggplot(d_edu, 
       aes(x = anio, y = parametro, fill = categoria)) +
  geom_col(alpha = .8) +
  geom_text(aes(label = round(parametro, digits = 1)),
            position = position_stack(vjust = .5)) +
  theme_m() +
  theme(legend.position = "bottom") +
  labs(title = "Parámetros de nivel educativo",
       y = "% de la población mayor a 18 años",
       x = "") +
  scale_fill_brewer(name = "", palette = "RdYlBu")

ggsave("parametros-op/plots/educacion.png", 
       width = 30, height = 20, units = "cm")


##  5. Voto anterior (Boreluy)  =============================================

# 1989
elec89 <- nacional_uy(eleccion = "1989",
                    tipo = "Presidencial",
                    por_departamento = FALSE) %>% 
  mutate(Fecha = as.character(lubridate::year(Fecha)), 
       Porcentaje = round(Porcentaje, 1)) %>%
  agrupar_partidos_uy(umbral = 5)  %>% 
  select(Fecha, Partido, Sigla, Porcentaje) %>%
  mutate(Partido = recode(Partido, "Voto Blanco/Anulado" = "OPBN")) %>% 
  mutate(Partido = recode(Partido, "Otros Partidos" = "OPBN")) %>% 
  group_by(Partido) %>% 
  summarise(parametro = sum(Porcentaje),
            anio = first(Fecha))%>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, parametro)) %>% 
  arrange(desc(parametro)) %>% 
  rename(categoria = Partido) %>% 
  mutate(variable = "voto_anterior")

# 1994
elec94 <- nacional_uy(eleccion = "1994",
                      tipo = "Presidencial",
                      por_departamento = FALSE) %>% 
  mutate(Fecha = as.character(lubridate::year(Fecha)), 
         Porcentaje = round(Porcentaje, 1)) %>%
  agrupar_partidos_uy(umbral = 5)  %>% 
  select(Fecha, Partido, Sigla, Porcentaje) %>%
  mutate(Partido = recode(Partido, "Voto Blanco/Anulado" = "OPBN")) %>% 
  mutate(Partido = recode(Partido, "Otros Partidos" = "OPBN")) %>% 
  group_by(Partido) %>% 
  summarise(parametro = sum(Porcentaje),
            anio = first(Fecha))%>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, parametro)) %>% 
  arrange(desc(parametro)) %>% 
  rename(categoria = Partido) %>% 
  mutate(variable = "voto_anterior")

# 1999
elec99 <- nacional_uy(eleccion = "1999",
                      tipo = "Presidencial",
                      por_departamento = FALSE) %>% 
  mutate(Fecha = as.character(lubridate::year(Fecha)), 
         Porcentaje = round(Porcentaje, 1)) %>%
  agrupar_partidos_uy(umbral = 5)  %>% 
  select(Fecha, Partido, Sigla, Porcentaje) %>%
  mutate(Partido = recode(Partido, "Voto Blanco/Anulado" = "OPBN")) %>% 
  mutate(Partido = recode(Partido, "Otros Partidos" = "OPBN")) %>% 
  group_by(Partido) %>% 
  summarise(parametro = sum(Porcentaje),
            anio = first(Fecha))%>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, parametro)) %>% 
  arrange(desc(parametro)) %>% 
  rename(categoria = Partido) %>% 
  mutate(variable = "voto_anterior")

# 2004
elec04 <- nacional_uy(eleccion = "2004",
                      tipo = "Presidencial",
                      por_departamento = FALSE) %>% 
  mutate(Fecha = as.character(lubridate::year(Fecha)), 
         Porcentaje = round(Porcentaje, 1)) %>%
  agrupar_partidos_uy(umbral = 5)  %>% 
  select(Fecha, Partido, Sigla, Porcentaje) %>%
  mutate(Partido = recode(Partido, "Voto Blanco/Anulado" = "OPBN")) %>% 
  mutate(Partido = recode(Partido, "Otros Partidos" = "OPBN")) %>% 
  group_by(Partido) %>% 
  summarise(parametro = sum(Porcentaje),
            anio = first(Fecha))%>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, parametro)) %>% 
  arrange(desc(parametro)) %>% 
  rename(categoria = Partido) %>% 
  mutate(variable = "voto_anterior")

# 2009
elec09 <- nacional_uy(eleccion = "2009",
                      tipo = "Presidencial",
                      por_departamento = FALSE) %>% 
  mutate(Fecha = as.character(lubridate::year(Fecha)), 
         Porcentaje = round(Porcentaje, 1)) %>%
  agrupar_partidos_uy(umbral = 5)  %>% 
  select(Fecha, Partido, Sigla, Porcentaje) %>%
  mutate(Partido = recode(Partido, "Voto Blanco/Anulado" = "OPBN")) %>% 
  mutate(Partido = recode(Partido, "Otros Partidos" = "OPBN")) %>% 
  group_by(Partido) %>% 
  summarise(parametro = sum(Porcentaje),
            anio = first(Fecha))%>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, parametro)) %>% 
  arrange(desc(parametro)) %>% 
  rename(categoria = Partido) %>% 
  mutate(variable = "voto_anterior")

# 2014
elec14 <- nacional_uy(eleccion = "2014",
                      tipo = "Presidencial",
                      por_departamento = FALSE) %>% 
  mutate(Fecha = as.character(lubridate::year(Fecha)), 
         Porcentaje = round(Porcentaje, 1)) %>%
  agrupar_partidos_uy(umbral = 5)  %>% 
  select(Fecha, Partido, Sigla, Porcentaje) %>%
  mutate(Partido = recode(Partido, "Voto Blanco/Anulado" = "OPBN")) %>% 
  mutate(Partido = recode(Partido, "Otros Partidos" = "OPBN")) %>% 
  group_by(Partido) %>% 
  summarise(parametro = sum(Porcentaje),
            anio = first(Fecha))%>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, parametro)) %>% 
  arrange(desc(parametro)) %>% 
  rename(categoria = Partido) %>% 
  mutate(variable = "voto_anterior")

# 2019
elec19 <- nacional_uy(eleccion = "2019",
                      tipo = "Presidencial",
                      por_departamento = FALSE) %>% 
  mutate(Fecha = as.character(lubridate::year(Fecha)), 
         Porcentaje = round(Porcentaje, 1)) %>%
  agrupar_partidos_uy(umbral = 5)  %>% 
  select(Fecha, Partido, Sigla, Porcentaje) %>%
  mutate(Partido = recode(Partido, "Voto Blanco/Anulado" = "OPBN")) %>% 
  mutate(Partido = recode(Partido, "Otros Partidos" = "OPBN")) %>% 
  group_by(Partido) %>% 
  summarise(parametro = sum(Porcentaje),
            anio = first(Fecha))%>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, parametro)) %>% 
  arrange(desc(parametro)) %>% 
  rename(categoria = Partido) %>% 
  mutate(variable = "voto_anterior")


d_voto <- rbind(elec89, elec94, elec99, elec04, elec09, elec14, elec19)
d_voto <- d_voto %>% 
  mutate(anio = as.numeric(anio)) 

ggplot(d_voto %>% 
         mutate(categoria = factor(categoria, 
                                   levels = c("Frente Amplio",
                                              "Partido Nacional",
                                              "Partido Colorado",
                                              "Nuevo Espacio",
                                              "Cabildo Abierto",
                                              "OPBN"))), 
       aes(x = anio, y = parametro, fill = categoria)) +
  geom_col(alpha = .6) +
  geom_text(aes(label = round(parametro, digits = 1)),
            position = position_stack(vjust = .5)) +
  theme_m() +
  theme(legend.position = "bottom") +
  labs(title = "Parámetros de voto anterior",
       y = "% de votos recibidos",
       x = "") +
  scale_fill_manual(name = "", 
                    values = c("Frente Amplio" = "mediumblue",
                               "Partido Nacional" = "skyblue",
                               "Partido Colorado" = "firebrick2",
                               "Nuevo Espacio" = "forestgreen",
                               "Cabildo Abierto" = "gold3",
                               "OPBN" = "grey40")) +
  scale_x_continuous(breaks = seq(from = 1989, to = 2019, by = 5))

ggsave("parametros-op/plots/voto.png", 
       width = 30, height = 20, units = "cm")


##  6. Merge parámetros  ====================================================
base_parametros <- rbind(d_sexedad, d_poblacion, d_edu, d_voto) %>% 
  mutate(parametro = round(parametro / 100, digits = 3)) 

writexl::write_xlsx(base_parametros, "parametros-op/data/data_parametros.xlsx")
save(base_parametros, file = "parametros-op/data/data_parametros.rda")

