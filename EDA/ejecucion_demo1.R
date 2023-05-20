### EJECUCION DE GASTO ###
library(tidyverse)
library(data.table)
library(skimr)

#install.packages("remotes")
#remotes::install_github("calderonsamuel/perutranspaeconomica", "devel")

library(perutranspaeconomica)

gasto_df <- read.csv("data/01_raw/EP_Estado_Ejecucion_Ingresos_2011_2020.csv")

v0_ejecucion <- gasto_df %>% 
    filter(NIVEL_NOMBRE == "MUNICIPALIDADES") %>% 
    mutate(
        FUENTE_CANON = if_else(
            FUENTE_NOMBRE == "CANON Y SOBRECANON, REGALIAS, RENTA DE ADUANAS Y PARTICIPACIONES",
            "FUENTE CANON",
            "FUENTES REGULARES"
        ),
        GASTO_CORRIENTE = if_else(
            CLASE_NOMBRE == "GASTOS CORRIENTES",
            "GASTOS CORRIENTES",
            "GASTOS DE CAPITAL O SERVICIO DE LA DEUDA")
    ) %>% 
    group_by(
        ANO_EJE, MES_EJE, NIVEL_GOBIERNO_NOMBRE, NIVEL_NOMBRE, DEPARTAMENTO_EJECUTORA_NOMBRE,
        PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE, MUNICIPALIDAD = PLIEGO_NOMBRE
    ) %>% 
    summarise(
        total_monto_autorizado_inicial_pia = sum(MONTO_PIA, na.rm = TRUE),
        total_monto_autorizado_final_pim = sum(MONTO_PIM, na.rm = TRUE),
        total_monto_ejecucion = sum(MONTO_EJECUCION, na.rm = TRUE),
        pct_ejecutado_autorizado_final = total_monto_ejecucion/total_monto_autorizado_final_pim,
        pct_ejecutado_autorizado_inicial = total_monto_ejecucion/total_monto_autorizado_inicial_pia
    ) %>% 
    arrange(NIVEL_GOBIERNO_NOMBRE, NIVEL_NOMBRE, DEPARTAMENTO_EJECUTORA_NOMBRE, 
            PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE, MUNICIPALIDAD, ANO_EJE, MES_EJE) %>% 
    ungroup()

v1_ejecucion <- gasto_df %>% 
    filter(NIVEL_NOMBRE == "MUNICIPALIDADES") %>% 
    mutate(
        FUENTE_CANON = if_else(
            FUENTE_NOMBRE == "CANON Y SOBRECANON, REGALIAS, RENTA DE ADUANAS Y PARTICIPACIONES",
            "FUENTE CANON",
            "FUENTES REGULARES"
        ),
        GASTO_CORRIENTE = if_else(
            CLASE_NOMBRE == "GASTOS CORRIENTES",
            "GASTOS CORRIENTES",
            "GASTOS DE CAPITAL O SERVICIO DE LA DEUDA")
    ) %>% 
    group_by(
        ANO_EJE, MES_EJE, NIVEL_GOBIERNO_NOMBRE, NIVEL_NOMBRE, DEPARTAMENTO_EJECUTORA_NOMBRE,
        PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE, MUNICIPALIDAD = PLIEGO_NOMBRE, FUENTE_CANON, GASTO_CORRIENTE
    ) %>% 
    summarise(
        total_monto_autorizado_inicial_pia = sum(MONTO_PIA, na.rm = TRUE),
        total_monto_autorizado_final_pim = sum(MONTO_PIM, na.rm = TRUE),
        total_monto_ejecucion = sum(MONTO_EJECUCION, na.rm = TRUE),
        pct_ejecutado_autorizado_final = total_monto_ejecucion/total_monto_autorizado_final_pim,
        pct_ejecutado_autorizado_inicial = total_monto_ejecucion/total_monto_autorizado_inicial_pia
    ) %>% 
    ungroup()

gasto_df %>% 
    count(NIVEL_NOMBRE)

gasto_df %>% 
    count(PLIEGO_NOMBRE)

gasto_df %>% 
    count(FUENTE_NOMBRE)

gasto_df %>% 
    count(CLASE_NOMBRE)

gasto_df %>% 
    count(TIPO_TRANSACCION_NOMBRE)

gasto_df %>% 
    count(GENERICA_NOMBRE)


gasto_df %>% 
    skim()

v1_ejecucion %>% 
    skim()

v0_ejecucion %>% 
    skim()

gasto_df %>% 
    count(MES_EJE)


detalle_inversiones <- fread("data/01_raw/DETALLE_INVERSIONES.csv")

detalle_inversiones %>% 
    count(month(FECHA_VIABILIDAD))

detalle_inversiones %>% 
    count(year(FECHA_VIABILIDAD))
