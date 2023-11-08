### DATOS 2010-2017

library(dplyr)
library(readxl)
library(arrow)
library(lubridate)

## POSTORES

# Load the data and match columns
df_2010_2014 <- read_xlsx("data/01_raw/reporte-adjudicaciones/8. Reporte Ofertantes 2010-2014.xlsx")
colnames(df_2010_2014)

df_2010_2014 <- df_2010_2014 %>% 
    rename(
        ruc_entidad = RUC,
        ENTIDAD = ENTIDAD,
        nombre_proceso = PROCESO,
        fecha_publicacion = FECHAPUBLICACION,
        numero_item = NUMITEM,
        ruc_postor = RUCPOSTOR,
        nombre_postor = NOMBREPOSTOR,
        fecha_propuesta = FECHAREGISTROPROPUESTA
    )


df_2015_2017 <- read_xlsx("data/01_raw/reporte-adjudicaciones/9. Reporte Ofertantes 2015-2017.xlsx") 
colnames(df_2015_2017)

df_2015_2017 <- df_2015_2017 %>% 
    rename(
        ENTIDAD = ENTIDAD,
        nombre_proceso = PROCESO,
        fecha_publicacion = `FECHA PUBLICACION`,
        ruc_postor = `RUC POSTOR`,
        nombre_postor = `NOMBRE POSTOR`,
        fecha_propuesta = `FECHA REGISTRO PROPUESTA`
    )

# Shared columns
shared_columns <- Reduce(intersect, list(colnames(df_2010_2014),
                                         colnames(df_2015_2017)))

# Keep only shared columns in each dataframe
df_2010_2014_shared <- df_2010_2014[, shared_columns]
df_2015_2017_shared <- df_2015_2017[, shared_columns]

combined_df_postores <- bind_rows(df_2010_2014_shared, df_2015_2017_shared)

# Normalise values
regex_bienes <- "(?i)BIEN"
regex_obras <- "(?i)OBR"
regex_servicios <- "(?i)SERVIC"

combined_df_postores <- combined_df_postores %>%
    mutate(OBJETO = case_when(
        str_detect(OBJETO, regex_bienes) ~ "Bienes",
        str_detect(OBJETO, regex_obras) ~ "Obras",
        str_detect(OBJETO, regex_servicios) ~ "Servicios",
        TRUE ~ "Other"
    )
    )

# Write the combined dataset to a new CSV file
write_parquet(combined_df_postores, "src2/data/02_combined_postores_data_2017.parquet")

rm(df_2010_2014, df_2015_2017, shared_columns, df_2010_2014_shared, df_2015_2017_shared)

## CONTRATOS

library(dplyr)
library(readxl)
library(lubridate)
library(arrow)

# Load the data and match columns
df_2010_2017 <- read_xlsx("data/01_raw/reporte-adjudicaciones/3. Reporte Contratos 201-2017.xlsx") 

df_2010_2017 <- df_2010_2017 %>%
    transmute(
        CODIGO_CONTRATO = CODIGO_CONTRATO,
        NUM_CONTRATO = `Nº CONTRATO`,
        FECHA_VIGENCIA_INICIAL = `FECHA VIGENCIA INICIAL`,
        FECHA_VIGENCIA_FINAL = `FECHA VIGENCIA FINAL`,
        FECHA_SUSCRIPCION_CONTRATO = `FECHA SUSCRIPCION CONTRATO`,
        FECHA_PUBLICACION_CONTRATO = `FECHA_PUBLICACION_CONTRATO`,
        RUC_DESTINATARIO_PAGO = `RUC DESTINO PAGO`,
        MONEDA = MONEDA,
        MONTO_CONTRATADO_TOTAL = MON_CONT_TOTAL,
        MONTO_CONTRATADO_ITEM = `VALOR REFERENCIAL ITEM`,
        DESCRIPCION_PROCESO = `DESCRIPCION PROCESO`,
        CODIGOCONVOCATORIA = PROCESO,
        MODALIDAD = MODALIDAD,
        TIPO_PROCESO = `TIPO PROCESO`
    ) %>% 
    mutate(
        CODIGO_CONTRATO = as.character(CODIGO_CONTRATO),
        NUM_CONTRATO = as.character(NUM_CONTRATO),
        FECHA_VIGENCIA_INICIAL = as.POSIXct(FECHA_VIGENCIA_INICIAL),
        FECHA_VIGENCIA_FINAL = as.POSIXct(FECHA_VIGENCIA_FINAL),
        FECHA_SUSCRIPCION_CONTRATO = as.POSIXct(FECHA_SUSCRIPCION_CONTRATO),
        FECHA_PUBLICACION_CONTRATO = as.POSIXct(FECHA_PUBLICACION_CONTRATO),
        RUC_DESTINATARIO_PAGO = as.character(RUC_DESTINATARIO_PAGO),
        MONEDA = as.character(MONEDA),
        MONTO_CONTRATADO_TOTAL = as.numeric(MONTO_CONTRATADO_TOTAL),
        MONTO_CONTRATADO_ITEM = as.numeric(MONTO_CONTRATADO_ITEM),
        DESCRIPCION_PROCESO = as.character(DESCRIPCION_PROCESO),
        CODIGOCONVOCATORIA = as.character(CODIGOCONVOCATORIA),
        MODALIDAD = as.character(MODALIDAD),
        TIPO_PROCESO = as.character(TIPO_PROCESO)
    )

# Combine datasets
combined_df_contratos <- bind_rows(df_2010_2017)

# Write the combined dataset to a new CSV file
write_parquet(combined_df_contratos, "src2/data/02_combined_contratos_data_2017.parquet")

rm(df_2010_2017)

## Adjudicaciones

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

# Helper function to convert columns
convert_columns <- function(df, conversions, else_type = NULL) {
    for (col_name in names(conversions)) {
        if (col_name %in% colnames(df)) {
            df[[col_name]] <- conversions[[col_name]](df[[col_name]])
        }
    }
    
    if (!is.null(else_type)) {
        cols_to_convert <- setdiff(colnames(df), names(conversions))
        
        for (col_name in cols_to_convert) {
            if (else_type == "character") {
                df[[col_name]] <- as.character(df[[col_name]])
            } else if (else_type == "numerical") {
                df[[col_name]] <- as.numeric(df[[col_name]])
            } else if (else_type == "factor") {
                df[[col_name]] <- as.factor(df[[col_name]])
            }
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
df1_converted <- convert_columns(df1, conversions, "character")
df2_converted <- convert_columns(df2, conversions, "character")

# Identify shared columns
shared_columns <- Reduce(intersect, list(colnames(df1), colnames(df2)))

# Keep only shared columns in each dataframe
df1_shared <- df1_converted[, shared_columns]
df2_shared <- df2_converted[, shared_columns]

# Combine the dataframes
combined_df_adjudicaciones <- bind_rows(df1_shared, df2_shared)

# Normalize values:
combined_df_adjudicaciones$consorcio <- ifelse(combined_df_adjudicaciones$consorcio %in% c("S", "N"), 
                                               combined_df_adjudicaciones$consorcio, 
                                               ifelse(combined_df_adjudicaciones$consorcio %in% c("Consorcio"), 
                                                      "S", "N"))
combined_df_adjudicaciones$consorcio <- replace_na(combined_df_adjudicaciones$consorcio, "N")

regex_bienes <- "(?i)BIEN"
regex_obras <- "(?i)OBR"
regex_servicios <- "(?i)SERVIC"

combined_df_adjudicaciones <- combined_df_adjudicaciones %>%
    mutate(OBJETO = case_when(
        str_detect(OBJETO, regex_bienes) ~ "Bienes",
        str_detect(OBJETO, regex_obras) ~ "Obras",
        str_detect(OBJETO, regex_servicios) ~ "Servicios",
        TRUE ~ "Other"
    )
    )

# Write the combined dataset to a PARQUET file
write_parquet(combined_df_adjudicaciones, "src2/data/02_combined_adjudicaciones_data_2017.parquet")

# clean memory
rm(df1, df2, df1_shared, df2_shared, df1_converted, df2_converted, conversions, shared_columns)


## Invitaciones:

library(tidyverse)
library(readxl)
library(skimr)
library(arrow)

# 2010-2014
df1 <- read_xlsx("data/01_raw/reporte-adjudicaciones/4. Reporte Invidados 2010-2014.xlsx")

df1 <- df1 %>%
    rename(
        AÑO = AÑO,
        ruc_entidad = RUC,
        entidad = ENTIDAD,
        fecha_publicacion = FECHAPUBLICACION,
        nombre_proceso = PROCESO,
        tipo_proceso = TIPO_PROCESO,
        modalidad = MODALIDAD,
        numero_convocatoria = NUMCONVOCA,
        objeto = OBJETO,
        descripcion_proceso = DESCRIPCION_PROCESO,
        tipo_compra = `TIPO COMPRA`,
        descripcion_item = DESCRIPCIONITEM,
        valor_referencial = VALORREFERENCIAL,
        valor_referencial_item = `VALOR REFERENCIAL ITEM`,
        moneda = MONEDA,
        numero_item = NUMITEM,
        moneda_item = MONEDA_ITEM,
        estado_item = `ESTADO ITEM`,
        descripcion_reinicio = `DESCRIPCION REINICIO`
    )


# 2015-2017
df2 <- read_xlsx("data/01_raw/reporte-adjudicaciones/5. Reporte Invitados 2015-2017.xlsx")
df2 <- df2 %>%
    rename(
        AÑO = AÑO,
        entidad = ENTIDAD,
        ruc_entidad = RUC,
        departamento_entidad = DEPARTAMENTO_ENT,
        provincia_entidad = PROVINCIA_ENT,
        distrito_entidad = DISTRITO_ENT,
        normativa = NORMATIVA,
        modalidad = MODALIDAD,
        tipo_compra = `TIPO COMPRA`,
        tipo_proceso = `TIPO PROCESO`,
        proceso = PROCESO,
        numero_convocatoria = `Nº CONVOCA`,
        fecha_publicacion = `FECHA PUBLICACION`,
        objeto = OBJETO,
        descripcion_proceso = `DESCRIPCION PROCESO`,
        valor_referencial = `VALOR REFERENCIAL`,
        moneda = MONEDA,
        numero_item = `Nº ITEM`,
        descripcion_item = `DESCRIPCION ITEM`,
        valor_referencial_item = `VALOR REFERENCIAL ITEM`,
        cantidad = CANTIDAD,
        unidad_medida = `UNIDAD MEDIDA`,
        departamento = DEPARTAMENTO,
        provincia = PROVINCIA,
        distrito = DISTRITO,
        estado = ESTADO,
        descripcion_reinicio = REINICIO
    )


# Helper function to convert columns
convert_columns <- function(df, conversions, else_type = NULL) {
    for (col_name in names(conversions)) {
        if (col_name %in% colnames(df)) {
            df[[col_name]] <- conversions[[col_name]](df[[col_name]])
        }
    }
    
    if (!is.null(else_type)) {
        cols_to_convert <- setdiff(colnames(df), names(conversions))
        
        for (col_name in cols_to_convert) {
            if (else_type == "character") {
                df[[col_name]] <- as.character(df[[col_name]])
            } else if (else_type == "numerical") {
                df[[col_name]] <- as.numeric(df[[col_name]])
            } else if (else_type == "factor") {
                df[[col_name]] <- as.factor(df[[col_name]])
            }
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
    fecha_buenapro = as.Date,
    estado_item = as.character
)


# Apply conversions to each dataset
df1_converted <- convert_columns(df1, conversions, "character")
df2_converted <- convert_columns(df2, conversions, "character")

# Identify shared columns
shared_columns <- Reduce(intersect, list(colnames(df1), colnames(df2)))

# Keep only shared columns in each dataframe
df1_shared <- df1_converted[, shared_columns]
df2_shared <- df2_converted[, shared_columns]

# Combine the dataframes
combined_df_invitados <- bind_rows(df1_shared, df2_shared)

# Normalize values:
regex_bienes <- "(?i)BIEN"
regex_obras <- "(?i)OBR"
regex_servicios <- "(?i)SERVIC"

combined_df_invitados <- combined_df_invitados %>%
    mutate(OBJETO = case_when(
        str_detect(objeto, regex_bienes) ~ "Bienes",
        str_detect(objeto, regex_obras) ~ "Obras",
        str_detect(objeto, regex_servicios) ~ "Servicios",
        TRUE ~ objeto
    )
    )

# Write the combined dataset to a PARQUET file
write_parquet(combined_df_invitados, "src2/data/02_combined_invitados_data_2017.parquet")

## PAC


# 2010-2014
df1 <- read_xlsx("data/01_raw/reporte-adjudicaciones/6. Reporte PAC 2010-2014.xlsx")

# 2015-2017
df2 <- read_xlsx("data/01_raw/reporte-adjudicaciones/7. Reporte PAC 2015-2017.xlsx")

# Identificar las columnas que corresponden entre ambos dataframes
columnas_compartidas <- c("AÑO", "RUC ENTIDAD", "ENTIDAD CONTRATANTE", "SECTOR", "DEP_DESC", 
                          "PRO_DESC", "DIS_DESC", "NUMERO DE REFERENCIA", "TIPO DE PROCESO", 
                          "MODALIDAD", "DESCRIPCION DE PROCESO", "OBJETO DE CONTRATACION", 
                          "MES PREVISTO DE CONVOCATORIA", "MONEDA", "TOTAL VALOR ESTIMADO", 
                          "NUMERO DE ITEM", "DESCRIPCION DE ITEM", "CANTIDAD")

# Eliminar columnas no compartidas en df1
df1_shared <- df1[, colnames(df1) %in% columnas_compartidas]

# Definir las columnas correspondientes en df2
columnas_correspondientes_df2 <- c("AÑO", "RUC ENTIDAD", "ENTIDAD", "SECTOR", "DEPARTAMENTO", 
                                   "PROVINCIA", "DISTRITO", "N° DE REFERENCIA", "TIPO PROCESO", 
                                   "MODALIDAD", "DESCRIPCION DE PROCESO", "OBJETO", 
                                   "MES PERVISTO", "MONEDA", "TOTAL VALOR ESTIMADO", 
                                   "Nº ITEM", "DESCRIPCION ITEM", "CANTIDAD")

# Eliminar columnas no compartidas en df2
df2_shared <- df2[, colnames(df2) %in% columnas_correspondientes_df2]

# Crear un vector con los nombres de las columnas estandarizadas
nombres_estandarizados <- c("ANO", "RUC_ENTIDAD", "ENTIDAD", "SECTOR", "DEPARTAMENTO", "PROVINCIA", 
                            "DISTRITO", "REFERENCIA", "TIPO_PROCESO", "MODALIDAD", 
                            "DESCRIPCION_PROCESO", "OBJETO", "MES_PREVISTO", "MONEDA", 
                            "VALOR_ESTIMADO", "ITEM", "DESCRIPCION_ITEM", "CANTIDAD")

# Renombrar las columnas en df1 y df2
names(df1_shared) <- nombres_estandarizados
names(df2_shared) <- nombres_estandarizados

colnames(df1_shared)
colnames(df2_shared)

# Convertir las columnas de identificación de df1_shared a caracter
df1_shared$ANO <- as.character(df1_shared$ANO)
df1_shared$REFERENCIA <- as.character(df1_shared$REFERENCIA)
df1_shared$MES_PREVISTO <- as.character(df1_shared$MES_PREVISTO)

# Convertir las columnas de identificación de df2_shared a caracter
df2_shared$ANO <- as.character(df2_shared$ANO)
df2_shared$REFERENCIA <- as.character(df2_shared$REFERENCIA)
df2_shared$MES_PREVISTO <- as.character(df2_shared$MES_PREVISTO)

# Convertir columnas numericas a numerico en df1_shared
df1_shared$VALOR_ESTIMADO <- as.numeric(df1_shared$VALOR_ESTIMADO)
df1_shared$ITEM <- as.numeric(df1_shared$ITEM)
df1_shared$CANTIDAD <- as.numeric(df1_shared$CANTIDAD)

# Convertir columnas numericas a numerico en df2_shared
df2_shared$VALOR_ESTIMADO <- as.numeric(df2_shared$VALOR_ESTIMADO)
df2_shared$ITEM <- as.numeric(df2_shared$ITEM)
df2_shared$CANTIDAD <- as.numeric(df2_shared$CANTIDAD)

#

combined_df_pac <- rbind(df1_shared, df2_shared)

# Normalize values:
regex_bienes <- "(?i)BIEN"
regex_obras <- "(?i)OBR"
regex_servicios <- "(?i)SERVIC|CONSULTO"

combined_df_pac <- combined_df_pac %>%
    mutate(OBJETO = case_when(
        str_detect(OBJETO, regex_bienes) ~ "Bienes",
        str_detect(OBJETO, regex_obras) ~ "Obras",
        str_detect(OBJETO, regex_servicios) ~ "Servicios",
        TRUE ~ "Otro objeto"
    )
    )

# Write the combined dataset to a PARQUET file
write_parquet(combined_df_pac, "src2/data/02_combined_pac_data_2017.parquet")
