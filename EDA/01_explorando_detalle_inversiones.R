# Initial exploration of dataset
library(tidyverse)
library(broom)
library(skimr)
library(lubridate)
library(ggpmisc)

options(scipen = 99999, digits = 9)

# Load data
detalle_inversiones <- data.table::fread(file = "data/01_raw/DETALLE_INVERSIONES.csv")

# skim
skim(detalle_inversiones)

#
with(detalle_inversiones, plot(log(MONTO_VIABLE + 1) ~ log(COSTO_ACTUALIZADO + 1), pch = '.'))

# hay montos tras los cuales se requieren mayores estudios. Hay conglomeración justo antes?
# hasta 1 200 000: Perfil simplificado
# hasta 10 000 000: Perfil
# Mas de 10 000 000: Factibilidad

detalle_inversiones %>% 
    filter(MONTO_VIABLE < 20000000,
           MONTO_VIABLE > 100000) %>% 
    ggplot(aes(x = MONTO_VIABLE)) +
    geom_histogram(bins = 250) +
    geom_vline(xintercept = c(1200000, 10000000), color = "red") +
    scale_x_log10() 

detalle_inversiones %>%  
    filter(MONTO_VALORIZACION < 20000000,
           MONTO_VALORIZACION > 100000) %>% 
    ggplot(aes(x = MONTO_VALORIZACION)) +
    geom_histogram(bins = 250) +
    geom_vline(xintercept = c(1200000, 10000000), color = "red") +
    scale_x_log10() 

detalle_inversiones %>%  
    select(MONTO_VIABLE, MONTO_VALORIZACION) %>% 
    pivot_longer(everything()) %>%
    filter(value < 20000000,
           value > 100000) %>% 
    ggplot(aes(x = value, fill = name)) +
    geom_histogram(aes(y = 0.5*..density..),
                   bins = 350, position = "identity",
                   alpha = 0.6) +
    geom_vline(xintercept = c(1210000, 10010000), color = "red", lwd = 0.5) +
    scale_x_log10() +
    labs(x = "Monto en soles",
         y = "Densidad",
         color = "Monto") +
    theme_minimal()


table(cut(detalle_inversiones$MONTO_VIABLE, c(0, 1150000, 1200000, 1250000, 9900000, 10000000, 10100000, Inf))) %>% as.data.frame()

## DATA CLEANING

detalle_inversiones %>% 
    mutate(across(DEPARTAMENTO:DISTRITO,
                  function(x) str_squish(str_remove(x, '\"')))
           ) %>% 
    group_by(DEPARTAMENTO, PROVINCIA, DISTRITO, anho = year(FECHA_REGISTRO)) %>% 
    summarise(
        n_proyectos = n_distinct(CODIGO_UNICO),
        porc_expediente_tecnico = mean(EXPEDIENTE_TECNICO == "SÍ", na.rm = TRUE)
    ) %>% 
    ggplot(aes(x = anho, y = n_proyectos, group = DISTRITO)) +
    geom_line(alpha = 0.1) +
    scale_y_log10()
