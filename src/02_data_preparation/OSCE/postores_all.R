library(dplyr)
library(readxl)
library(arrow)
library(lubridate)

# Load the data and match columns
df_2010_2014 <- read_xlsx("data/01_raw/reporte-adjudicaciones/8. Reporte Ofertantes 2010-2014.xlsx")
colnames(df_2010_2014)

df_2010_2014 <- df_2010_2014 %>% 
    mutate(
        year = as.character(AÑO),
        ruc_postor = as.character(`RUCPOSTOR`),
        nombre_postor = as.character(`NOMBREPOSTOR`),
        fecha_registro_propuesta = `FECHAREGISTROPROPUESTA`,
        proceso = as.character(`PROCESO`),
        entidad = as.character(ENTIDAD),
        MODALIDAD = MODALIDAD,
        TIPO_PROCESO = TIPO_PROCESO,
    ) %>% 
    rename(
        ruc_miembro_consorcio = RUCMIEMBROCONSORCIO,
        FECHA_PUBLICACION = FECHAPUBLICACION,
        valor_referencial = VALORREFERENCIAL
    )


df_2015_2017 <- read_xlsx("data/01_raw/reporte-adjudicaciones/9. Reporte Ofertantes 2015-2017.xlsx") 
colnames(df_2015_2017)

df_2015_2017 <- df_2015_2017 %>% 
    mutate(
        year = as.character(`AÑO`),
        ruc_postor = as.character(`RUC POSTOR`),
        nombre_postor = as.character(`NOMBRE POSTOR`),
        fecha_registro_propuesta = `FECHA REGISTRO PROPUESTA`,
        proceso = as.character(`PROCESO`),
        entidad = as.character(ENTIDAD),
        MODALIDAD = MODALIDAD,
        TIPO_PROCESO = `TIPO PROCESO`,
        valor_referencial = as.numeric(`VALOR REFERENCIAL`)
    ) %>% 
    rename(
        ruc_miembro_consorcio = `RUC MIEMBRO CONSORCIO`,
        FECHA_PUBLICACION = `FECHA PUBLICACION`
    )


df_2018 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_POSTOR2018_0.xlsx")
colnames(df_2018)

df_2018 <- df_2018 %>% 
    transmute(
        year = as.character(`FECHACONVOCATORIA`),
        ruc_postor = as.character(`RUC_CODIGO_POSTOR`),
        nombre_postor = as.character(`POSTOR`),
        fecha_registro_propuesta = `FECHA_PRESENTACION_PROPUESTA`,
        proceso = as.character(`CODIGO_CONVOCATORIA`),
        entidad = NA,
        MODALIDAD = NA,
        TIPO_PROCESO = NA,
        FECHA_PUBLICACION = FECHACONVOCATORIA
    )

df_2019 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_POSTOR2019_0.xlsx")
colnames(df_2019)

df_2019 <- df_2019 %>% 
    transmute(
        year = as.character(`FECHACONVOCATORIA`),
        ruc_postor = as.character(`RUC_CODIGO_POSTOR`),
        nombre_postor = as.character(`POSTOR`),
        fecha_registro_propuesta = `FECHA_PRESENTACION_PROPUESTA`,
        proceso = as.character(`CODIGO_CONVOCATORIA`),
        entidad = NA,
        MODALIDAD = NA,
        TIPO_PROCESO = NA,
        FECHA_PUBLICACION = FECHACONVOCATORIA
    )

df_2020 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_POSTOR2020_0.xlsx")
colnames(df_2020)

df_2020 <- df_2020 %>% 
    transmute(
        year = as.character(`FECHACONVOCATORIA`),
        ruc_postor = as.character(`RUC_CODIGO_POSTOR`),
        nombre_postor = as.character(`POSTOR`),
        fecha_registro_propuesta = `FECHA_PRESENTACION_PROPUESTA`,
        proceso = as.character(`CODIGO_CONVOCATORIA`),
        entidad = NA,
        MODALIDAD = NA,
        TIPO_PROCESO = NA,
        FECHA_PUBLICACION = FECHACONVOCATORIA
    ) 

df_2021 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_POSTOR2021_0.xlsx")
colnames(df_2021)

df_2021 <- df_2021 %>% 
    transmute(
        year = as.character(`FECHACONVOCATORIA`),
        ruc_postor = as.character(`RUC_CODIGO_POSTOR`),
        nombre_postor = as.character(`POSTOR`),
        fecha_registro_propuesta = `FECHA_PRESENTACION_PROPUESTA`,
        proceso = as.character(`CODIGO_CONVOCATORIA`),
        entidad = NA,
        MODALIDAD = NA,
        TIPO_PROCESO = NA,
        FECHA_PUBLICACION = FECHACONVOCATORIA
    )

head(df_2010_2014)
head(df_2015_2017)
head(df_2018)

combined_df_postores <- bind_rows(df_2010_2014, df_2015_2017, df_2018, df_2019, df_2020, df_2021)

# Write the combined dataset to a new CSV file
write.csv(combined_df_postores, "data/02_intermediate/OSCE/combined_postores_data.csv", row.names = FALSE)
write_parquet(combined_df_postores, "data/02_intermediate/OSCE/combined_postores_data.parquet")
