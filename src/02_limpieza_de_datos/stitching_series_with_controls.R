## JOINING SERIES AND CONTROLS

monthly_indices_df <- read_parquet("data/02_intermediate/OSCE/monthly_indices.parquet")
controles_oci_monthly <- read_parquet("data/02_intermediate/controles_OCI_mensual.parquet")
controles_infogob_monthly <- read_parquet("data/02_intermediate/controles_infogob_mensual.parquet")

semestral_indices_df <- read_parquet("data/02_intermediate/OSCE/semestral_indices.parquet")
controles_oci_semestral <- read_parquet("data/02_intermediate/controles_OCI_semestral.parquet")
controles_infogob_semestral <- read_parquet("data/02_intermediate/controles_infogob_semestral.parquet")

osce_infogob_matching <- read_parquet("data/02_intermediate/osce_infogob_entity_name_matching.parquet")
osce_oci_matching <- read_parquet("data/02_intermediate/osce_oci_entity_name_matching.parquet")


colnames(monthly_indices_df)
colnames(controles_oci_monthly)
colnames(controles_infogob_monthly)
colnames(osce_infogob_matching)
colnames(osce_oci_matching)

osce_infogob2 <- monthly_indices_df %>% 
    mutate(mesanho_publicacion = as.Date(mesanho_publicacion)) %>% 
    left_join(osce_infogob_matching, by = c("gobierno")) %>% 
    left_join(controles_infogob_monthly %>% rename(mesanho_publicacion = date), by = c("region", "provincia", "distrito", "mesanho_publicacion")) %>% 
    left_join(osce_oci_matching %>% select(gobierno, nombre_entidad) %>% unique(), by = c("gobierno")) %>% 
    left_join(controles_oci_monthly %>% rename(mesanho_publicacion = date),
              by = c("gobierno", "nombre_entidad", "mesanho_publicacion"))

osce_infogob %>% 
    mutate(
        cutoff = mesanho_publicacion > "2014-01-01",
        fragmentation_category = case_when(fragmentation_index < 0 ~ "<0",
                                           fragmentation_index < 0.25 ~ "<0.25",
                                           fragmentation_index < 0.5 ~ "<0.5",
                                           fragmentation_index < 0.75 ~ "<0.75",
                                           fragmentation_index > 0.75 ~ ">0.75",
                                           is.na(fragmentation_index) ~ "No disponible"),
        competitividad_category = case_when(competitividad_index < 1 ~ "<1",
                                         competitividad_index < 2 ~ "<2",
                                         competitividad_index < 3 ~ "<3",
                                         competitividad_index < 4 ~ "<4",
                                         competitividad_index > 5 ~ ">5",
                                           is.na(competitividad_index) ~ "No disponible")
    ) %>% 
    lm(data = .,
       formula = tiempo_promedio ~ cutoff + fragmentation_category + competitividad_category) %>% 
    summary()

library(lme4)

osce_infogob %>% 
    mutate(
        cutoff = mesanho_publicacion > "2014-01-01",
        fragmentation_category = case_when(fragmentation_index < 0 ~ "<0",
                                           fragmentation_index < 0.25 ~ "<0.25",
                                           fragmentation_index < 0.5 ~ "<0.5",
                                           fragmentation_index < 0.75 ~ "<0.75",
                                           fragmentation_index > 0.75 ~ ">0.75",
                                           is.na(fragmentation_index) ~ "No disponible"),
        competitividad_category = case_when(competitividad_index < 1 ~ "<1",
                                            competitividad_index < 2 ~ "<2",
                                            competitividad_index < 3 ~ "<3",
                                            competitividad_index < 4 ~ "<4",
                                            competitividad_index > 5 ~ ">5",
                                            is.na(competitividad_index) ~ "No disponible")
    ) %>% 
    lmer(data = .,
       formula = tiempo_promedio ~ (1|gobierno) + (1|mesanho_publicacion) + cutoff + fragmentation_category + competitividad_category) %>% 
    summary()
