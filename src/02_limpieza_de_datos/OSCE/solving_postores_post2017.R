library(tidyverse)
library(readxl)

# Recuperando el nombre de la entidad de aquellos RUC -> Nombre entidad que podemos trasladar del 2010-2017

df1 <- read_xlsx("data/01_raw/reporte-adjudicaciones/1. Reporte Adjudicaciones 2010-2014.xlsx")

df1 <- df1 %>%
    rename(
        ruc_entidad = RUC,
        nombre_proceso = PROCESO,
        fecha_publicacion = `FECHA PUBLICACION`,
        moneda = MONEDA,
        numero_item = `NUM ITEM`,
        descripcion_item = `DESCRIPCION ITEM`,
        valor_referencial_item = `VALOR REFERENCIAL ITEM`,
        valor_adjudicado_item = `VALOR ADJUDICADO ITEM`,
        cantidad_adjudicada = CANT_ADJUDICADA,
        ruc_ganador = `RUC/CODIGO GANADOR`,
        consorcio = `ES CONSORCIO`,
        nombre_ganador = GANADOR,
        fecha_buenapro = `FECHA BUENAPRO`,
        estado_item = `ESTADO ITEM`
    )

# 2015-2017
df2 <- read_xlsx("data/01_raw/reporte-adjudicaciones/2. Reporte Adjudicaciones 2015-2017.xlsx")

df2 <- df2 %>%
    rename(
        ruc_entidad = RUC,
        nombre_proceso = PROCESO,
        fecha_publicacion = FECHAPUBLICACION,
        moneda = `MONEDA PROCESO`,
        numero_item = NUMITEM,
        descripcion_item = `DESCRIPCION ITEM`,
        valor_referencial_item = `VALOR REFERENCIAL ITEM`,
        valor_adjudicado_item = `VALOR ADJUDICADO ITEM`,
        cantidad_adjudicada = `CANTIDAD ADJUDICADO ITEM`,
        ruc_ganador = `RUC/Código GANADOR`,
        consorcio = CONSORCIO,
        nombre_ganador = GANADOR,
        fecha_buenapro = `FECHA BUENA PRO`,
        estado_item = ESTADO
    )

ruc_entity_mapping1 <- unique(df1[, c("ruc_entidad", "ENTIDAD")])
ruc_entity_mapping2 <- unique(df2[, c("ruc_entidad", "ENTIDAD")])

# Combine the unique pairs into a single mapping table
ruc_entity_mapping <- ruc_entity_mapping1 %>%
    bind_rows(ruc_entity_mapping2) %>%
    unique()

# Create a 1:1 mapping by selecting the first ENTIDAD name for each ENTIDAD_RUC, if not 
# a one to many mapping creates duplication of entries
ruc_entity_mapping_unique <- ruc_entity_mapping %>%
    group_by(ruc_entidad) %>%
    slice(1) %>%
    ungroup()

# Update the read_process_data function to use the ruc_entity_mapping_unique
read_process_data <- function(postor_year, adjudicaciones_year, ruc_entity_mapping) {
    postor_file <- paste0("data/01_raw/CONOSCE/CONOSCE_POSTOR", postor_year, "_0.xlsx")
    adjudicaciones_file <- paste0("data/01_raw/CONOSCE/CONOSCE_ADJUDICACIONES", adjudicaciones_year, "_0.xlsx")
    
    postor_df <- read_xlsx(postor_file)
    adjudicaciones_df <- read_xlsx(adjudicaciones_file)
    
    adjudicaciones_df <- adjudicaciones_df %>% 
        select(
            CODIGOCONVOCATORIA, CODIGOENTIDAD, ENTIDAD_RUC, 
            PROCESO
        ) %>% 
        unique()
    
    all_data <- postor_df %>% 
        left_join(
            adjudicaciones_df,
            by = c("CODIGO_CONVOCATORIA" = "CODIGOCONVOCATORIA")
        ) %>% 
        left_join(
            ruc_entity_mapping,
            by = c("ENTIDAD_RUC" = "ruc_entidad")
        )
    
    return(all_data)
}

# Read data using the updated function and unique mapping
df_2018_all <- read_process_data(2018, 2018, ruc_entity_mapping_unique)
df_2019_all <- read_process_data(2019, 2019, ruc_entity_mapping_unique)
df_2020_all <- read_process_data(2020, 2020, ruc_entity_mapping_unique)
df_2021_all <- read_process_data(2021, 2021, ruc_entity_mapping_unique)
