library(dplyr)
library(readxl)
library(arrow)
library(lubridate)

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

# recuperar ruc entidad:

ruc_entity_mapping_unique <- read_parquet("data/02_intermediate/ruc_entity_mapping_2010_2017.parquet")

# Add a new ENTIDAD column to the third dataframe using the lookup table
df_2015_2017 <- df_2015_2017 %>% 
    left_join(ruc_entity_mapping_unique,
              by = c("ENTIDAD"))

# use the patched post 2017 data
source(file = "src/02_data_preparation/OSCE/solving_postores_post2017.R")

df_2018_all <- df_2018_all %>% 
    rename(
        ruc_entidad = ENTIDAD_RUC,
        ENTIDAD = ENTIDAD,
        nombre_proceso = PROCESO,
        fecha_publicacion = FECHACONVOCATORIA,
        numero_item = N_ITEM,
        ruc_postor = RUC_CODIGO_POSTOR,
        nombre_postor = POSTOR,
        fecha_propuesta = FECHA_PRESENTACION_PROPUESTA
    )

df_2019_all <- df_2019_all %>% 
    rename(
        ruc_entidad = ENTIDAD_RUC,
        ENTIDAD = ENTIDAD,
        nombre_proceso = PROCESO,
        fecha_publicacion = FECHACONVOCATORIA,
        numero_item = N_ITEM,
        ruc_postor = RUC_CODIGO_POSTOR,
        nombre_postor = POSTOR,
        fecha_propuesta = FECHA_PRESENTACION_PROPUESTA
    )

df_2020_all <- df_2020_all %>% 
    rename(
        ruc_entidad = ENTIDAD_RUC,
        ENTIDAD = ENTIDAD,
        nombre_proceso = PROCESO,
        fecha_publicacion = FECHACONVOCATORIA,
        numero_item = N_ITEM,
        ruc_postor = RUC_CODIGO_POSTOR,
        nombre_postor = POSTOR,
        fecha_propuesta = FECHA_PRESENTACION_PROPUESTA
    )

df_2021_all <- df_2021_all %>% 
    rename(
        ruc_entidad = ENTIDAD_RUC,
        ENTIDAD = ENTIDAD,
        nombre_proceso = PROCESO,
        fecha_publicacion = FECHACONVOCATORIA,
        numero_item = N_ITEM,
        ruc_postor = RUC_CODIGO_POSTOR,
        nombre_postor = POSTOR,
        fecha_propuesta = FECHA_PRESENTACION_PROPUESTA
    )

# Shared columns
shared_columns <- Reduce(intersect, list(colnames(df_2010_2014),
                                         colnames(df_2015_2017),
                                         colnames(df_2018_all),
                                         colnames(df_2019_all),
                                         colnames(df_2020_all),
                                         colnames(df_2021_all)))

# Keep only shared columns in each dataframe
df_2010_2014_shared <- df_2010_2014[, shared_columns]
df_2015_2017_shared <- df_2015_2017[, shared_columns]
df_2018_all_shared <- df_2018_all[, shared_columns]
df_2019_all_shared <- df_2019_all[, shared_columns]
df_2020_all_shared <- df_2020_all[, shared_columns]
df_2021_all_shared <- df_2021_all[, shared_columns]


combined_df_postores <- bind_rows(df_2010_2014_shared, df_2015_2017_shared,
                                  df_2018_all_shared, df_2019_all_shared,
                                  df_2020_all_shared, df_2021_all_shared)


# Write the combined dataset to a new CSV file
write.csv(combined_df_postores, "data/02_intermediate/OSCE/combined_postores_data.csv", row.names = FALSE)
write_parquet(combined_df_postores, "data/02_intermediate/OSCE/combined_postores_data.parquet")
