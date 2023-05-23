library(tidyverse)
library(readxl)
library(skimr)
library(arrow)

# 2010-2014
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


# 2018-2022
df3 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_ADJUDICACIONES2018_0.xlsx") 

df3 <- df3 %>%
    rename(
        ruc_entidad = ENTIDAD_RUC,
        nombre_proceso = PROCESO,
        fecha_publicacion = FECHA_CONVOCATORIA,
        moneda = MONEDA,
        numero_item = N_ITEM,
        descripcion_item = DESCRIPCION_ITEM,
        valor_referencial_item = MONTO_REFERENCIAL_ITEM,
        valor_adjudicado_item = MONTO_ADJUDICADO_ITEM,
        cantidad_adjudicada = CANTIDAD_ADJUDICADO_ITEM,
        ruc_ganador = RUC_PROVEEDOR,
        consorcio = TIPO_PROVEEDOR,
        nombre_ganador = PROVEEDOR,
        fecha_buenapro = FECHA_BUENAPRO,
        estado_item = ESTADO_ITEM
    )


df4 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_ADJUDICACIONES2019_0.xlsx") %>%
    rename(
        ruc_entidad = ENTIDAD_RUC,
        nombre_proceso = PROCESO,
        fecha_publicacion = FECHA_CONVOCATORIA,
        moneda = MONEDA,
        numero_item = N_ITEM,
        descripcion_item = DESCRIPCION_ITEM,
        valor_referencial_item = MONTO_REFERENCIAL_ITEM,
        valor_adjudicado_item = MONTO_ADJUDICADO_ITEM,
        cantidad_adjudicada = CANTIDAD_ADJUDICADO_ITEM,
        ruc_ganador = RUC_PROVEEDOR,
        consorcio = TIPO_PROVEEDOR,
        nombre_ganador = PROVEEDOR,
        fecha_buenapro = FECHA_BUENAPRO,
        estado_item = ESTADO_ITEM
    )

df5 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_ADJUDICACIONES2020_0.xlsx") %>%
    rename(
        ruc_entidad = ENTIDAD_RUC,
        nombre_proceso = PROCESO,
        fecha_publicacion = FECHA_CONVOCATORIA,
        moneda = MONEDA,
        numero_item = N_ITEM,
        descripcion_item = DESCRIPCION_ITEM,
        valor_referencial_item = MONTO_REFERENCIAL_ITEM,
        valor_adjudicado_item = MONTO_ADJUDICADO_ITEM,
        cantidad_adjudicada = CANTIDAD_ADJUDICADO_ITEM,
        ruc_ganador = RUC_PROVEEDOR,
        consorcio = TIPO_PROVEEDOR,
        nombre_ganador = PROVEEDOR,
        fecha_buenapro = FECHA_BUENAPRO,
        estado_item = ESTADO_ITEM
    )

df6 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_ADJUDICACIONES2021_0.xlsx") %>%
    rename(
        ruc_entidad = ENTIDAD_RUC,
        nombre_proceso = PROCESO,
        fecha_publicacion = FECHA_CONVOCATORIA,
        moneda = MONEDA,
        numero_item = N_ITEM,
        descripcion_item = DESCRIPCION_ITEM,
        valor_referencial_item = MONTO_REFERENCIAL_ITEM,
        valor_adjudicado_item = MONTO_ADJUDICADO_ITEM,
        cantidad_adjudicada = CANTIDAD_ADJUDICADO_ITEM,
        ruc_ganador = RUC_PROVEEDOR,
        consorcio = TIPO_PROVEEDOR,
        nombre_ganador = PROVEEDOR,
        fecha_buenapro = FECHA_BUENAPRO,
        estado_item = ESTADO_ITEM
    )

df7 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_ADJUDICACIONES2022_0.xlsx") %>%
    rename(
        ruc_entidad = ENTIDAD_RUC,
        nombre_proceso = PROCESO,
        fecha_publicacion = FECHA_CONVOCATORIA,
        moneda = MONEDA,
        numero_item = N_ITEM,
        descripcion_item = DESCRIPCION_ITEM,
        valor_referencial_item = MONTO_REFERENCIAL_ITEM,
        valor_adjudicado_item = MONTO_ADJUDICADO_ITEM,
        cantidad_adjudicada = CANTIDAD_ADJUDICADO_ITEM,
        ruc_ganador = RUC_PROVEEDOR,
        consorcio = TIPO_PROVEEDOR,
        nombre_ganador = PROVEEDOR,
        fecha_buenapro = FECHA_BUENAPRO,
        estado_item = ESTADO_ITEM
    )


# Helper function to convert columns
convert_columns <- function(df, conversions) {
    for (col_name in names(conversions)) {
        if (col_name %in% colnames(df)) {
            df[[col_name]] <- conversions[[col_name]](df[[col_name]])
        }
    }
    df
}

# Column conversions
conversions <- list(
    ruc_entidad = as.character,
    nombre_proceso = as.character,
    fecha_publicacion = as.Date,
    moneda = as.character,
    numero_item = as.integer,
    descripcion_item = as.character,
    valor_referencial_item = as.numeric,
    valor_adjudicado_item = as.numeric,
    cantidad_adjudicada = as.integer,
    ruc_ganador = as.character,
    consorcio = as.character, # or as.factor if there are more than two categories
    nombre_ganador = as.character,
    fecha_buenapro = as.Date,
    estado_item = as.character
)


# Apply conversions to each dataset
df1_converted <- convert_columns(df1, conversions)
df2_converted <- convert_columns(df2, conversions)
df3_converted <- convert_columns(df3, conversions)
df4_converted <- convert_columns(df4, conversions)
df5_converted <- convert_columns(df5, conversions)
df6_converted <- convert_columns(df6, conversions)
df7_converted <- convert_columns(df7, conversions)


# Identify shared columns
shared_columns <- Reduce(intersect, list(colnames(df1), colnames(df2), colnames(df3)))

# Keep only shared columns in each dataframe
df1_shared <- df1_converted[, shared_columns]
df2_shared <- df2_converted[, shared_columns]
df3_shared <- df3_converted[, shared_columns]
df4_shared <- df4_converted[, shared_columns]
df5_shared <- df5_converted[, shared_columns]
df6_shared <- df6_converted[, shared_columns]
df7_shared <- df7_converted[, shared_columns]

# Combine the dataframes
combined_df_adjudicaciones <- bind_rows(df1_shared, df2_shared, df3_shared,
                         df4_shared, df5_shared, df6_shared, 
                         df7_shared)


# Normalize values:
combined_df_adjudicaciones$consorcio <- ifelse(combined_df_adjudicaciones$consorcio %in% c("S", "N"), 
                                                   combined_df_adjudicaciones$consorcio, 
                                                   ifelse(combined_df_adjudicaciones$consorcio %in% c("Consorcio"), 
                                                          "S", "N"))
combined_df_adjudicaciones$consorcio <- replace_na(combined_df_adjudicaciones$consorcio, "N")


# check
skim_without_charts(combined_df_adjudicaciones)

### RECOVERING ENTITY NAME FROM 2018 ONWARDS

# Extract unique RUC and entity name pairs from the first two tables
ruc_entity_mapping1 <- unique(df1[, c("ruc_entidad", "ENTIDAD")])
ruc_entity_mapping2 <- unique(df2[, c("ruc_entidad", "ENTIDAD")])

# Combine the unique pairs into a single mapping table
ruc_entity_mapping <- ruc_entity_mapping1 %>%
    bind_rows(ruc_entity_mapping2) %>%
    unique()

ruc_entity_mapping_unique <- ruc_entity_mapping %>%
    group_by(ruc_entidad) %>%
    slice(1) %>%
    ungroup()

# Save for later use
write_parquet(ruc_entity_mapping_unique, "data/02_intermediate/ruc_entity_mapping_2010_2017.parquet")

# Add a new ENTIDAD column to the third dataframe using the lookup table
combined_df_adjudicaciones <- combined_df_adjudicaciones %>%
    left_join(ruc_entity_mapping_unique, by = c("ruc_entidad"))

# Write the combined dataset to a PARQUET file
write_parquet(combined_df_adjudicaciones, "data/02_intermediate/OSCE/merged_adjudicaciones_data.parquet")
