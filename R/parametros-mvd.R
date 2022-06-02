
## **************************************************************************
## Parámetros Montevideo
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

##  2. SexEdad (Censo)  =====================================================


load(file = "parametros-op/data/ech_combined.rda")

# 18 o mas
data <- ech_combined %>% 
  filter(bc_anio == 2019) %>% 
  filter(bc_dpto == "Montevideo") %>% 
  filter(bc_pe3 >= 18)

# Sexo y edad
d_sexedad <- data %>% 
  mutate(categoria = case_when(
    bc_pe3 >= 18 & bc_pe3 <= 39  & bc_pe2 == "Varón" ~ "Hombres_18_39",
    bc_pe3 >= 40 & bc_pe3 <= 59  & bc_pe2 == "Varón" ~ "Hombres_40_59",
    bc_pe3 >= 60 & bc_pe3 <= 110 & bc_pe2 == "Varón" ~ "Hombres_60_mas",
    bc_pe3 >= 18 & bc_pe3 <= 39  & bc_pe2 == "Mujer" ~ "Mujeres_18_39",
    bc_pe3 >= 40 & bc_pe3 <= 59  & bc_pe2 == "Mujer" ~ "Mujeres_40_59",
    bc_pe3 >= 60 & bc_pe3 <= 110 & bc_pe2 == "Mujer" ~ "Mujeres_60_mas",
  )) %>% 
  filter(!is.na(categoria)) %>% 
  select(categoria, bc_pesoan) %>% 
  group_by(categoria) %>% 
  summarise(n = sum(bc_pesoan, na.rm = TRUE)) %>% 
  mutate(parametro = round(n / sum(n), digits = 4) * 100) %>% 
  select(-n) %>% 
  mutate(variable = "sexedad") 

# Educacion 
d_edu <- data %>% 
  filter(bc_edu != -15) %>% 
  mutate(categoria = case_when(
    bc_edu <= 6 ~ "Primaria",
    bc_edu >= 7 & bc_edu <= 12 ~ "Secundaria",
    bc_edu >= 13 ~ "Terciaria"
  )) %>% 
  filter(!is.na(categoria)) %>% 
  select(categoria, bc_pesoan) %>% 
  group_by(categoria) %>% 
  summarise(n = sum(bc_pesoan, na.rm = TRUE)) %>% 
  mutate(parametro = round(n / sum(n), digits = 4) * 100) %>% 
  select(-n) %>% 
  mutate(variable = "educacion") 

# Unir y guardar
par <- rbind(d_sexedad, d_edu)
writexl::write_xlsx(par, "parametros-op/parametros-mvd/sexedad_edu.xlsx")




