
## ***************************************************************************
## Recodificacion ECH
## ***************************************************************************

##  1. Paquetes ==============================================================

library(tidyverse)
library(readstata13)
library(stringr)
library(iterators)
library(foreach)
library(anesrake)
library(Boreluy)

rm(list = ls())

source("parametros-op/R/moda.R") # Cargo la función moda
source("parametros-op/R/theme_m.R") # Cargo la función moda

##  2. Cargar data  ==========================================================

# Leer todas las bases (en dos tandas, por tamaño)
filenames <- list.files("ECH-IECON", 
                        pattern="*.dta",
                        full.names=TRUE) # Todos los archivos .dta
namesfiles <- substr(filenames, 11, 40) # Los modifico por comodidad


filenames_1 <- filenames[1:9]
filenames_2 <- filenames[10:19]
filenames_3 <- filenames[20:27]

namesfiles_1 <- namesfiles[1:9]
namesfiles_2 <- namesfiles[10:19]
namesfiles_3 <- namesfiles[20:27]

# Primeras 15 
for (i in seq_along(filenames_1)) {
  
  assign(namesfiles_1[i], 
         read.dta13(filenames_1[i],
                    nonint.factors=TRUE,
                    generate.factors = TRUE))
}

# Seleccionar variables
lista_encuestas <- ls(pattern = "p")

var_sel <- c("bc_pesoan", "bc_anio", "bc_dpto", "bc_pe2", "bc_pe3", 
             "bc_edu", "bc_edu_1", "bc_nivel", "nivel_aux")

foreach(x = iter(lista_encuestas)) %do% 
  { assign(x, modify_if(get(x) , is.factor, as.character) %>% 
             select_if(names(.) %in% var_sel)) }

# Segunda tanda
for (i in seq_along(filenames_2)) {
  
  assign(namesfiles_2[i], 
         read.dta13(filenames_2[i],
                    nonint.factors=TRUE,
                    generate.factors = TRUE))
}

lista_encuestas <- ls(pattern = "p")

foreach(x = iter(lista_encuestas)) %do% 
  { assign(x, modify_if(get(x) , is.factor, as.character) %>% 
             select_if(names(.) %in% var_sel)) }

# Tercera tanda
for (i in seq_along(filenames_3)) {
  
  assign(namesfiles_3[i], 
         read.dta13(filenames_3[i],
                    nonint.factors=TRUE,
                    generate.factors = TRUE))
}

lista_encuestas <- ls(pattern = "p")

foreach(x = iter(lista_encuestas)) %do% 
  { assign(x, modify_if(get(x) , is.factor, as.character) %>% 
             select_if(names(.) %in% var_sel)) }

# Unir todas
ech_combined <- plyr::rbind.fill(mget(ls(pattern="p")), 
                         fill = TRUE,
                         stringsAsFactors = FALSE)


##  3. Guardar data unida  ==================================================

save(ech_combined, file = "parametros-op/Data/ech_combined.rda")











