## JOINING SERIES AND CONTROLS
library(tidyverse)
library(arrow)

monthly_indices_df <- read_parquet("data/02_intermediate/OSCE/monthly_indices.parquet")
controles_oci_monthly <- read_parquet("data/02_intermediate/controles_OCI_mensual.parquet")
controles_infogob_monthly <- read_parquet("data/02_intermediate/controles_infogob_mensual.parquet")

semestral_indices_df <- read_parquet("data/02_intermediate/OSCE/semestral_indices.parquet")
controles_oci_semestral <- read_parquet("data/02_intermediate/controles_OCI_semestral.parquet")
controles_infogob_semestral <- read_parquet("data/02_intermediate/controles_infogob_semestral.parquet")

osce_infogob_matching <- read_parquet("data/02_intermediate/osce_infogob_entity_name_matching.parquet")
osce_oci_matching <- read_parquet("data/02_intermediate/osce_oci_entity_name_matching.parquet")

## Monthly

osce_infogob2 <- monthly_indices_df %>% 
    mutate(mesanho_publicacion = as.Date(mesanho_publicacion)) %>% 
    left_join(osce_infogob_matching, by = c("gobierno")) %>% 
    left_join(controles_infogob_monthly %>% rename(mesanho_publicacion = date), by = c("region", "provincia", "distrito", "mesanho_publicacion")) %>% 
    left_join(controles_oci_monthly %>% rename(mesanho_publicacion = date),
              by = c("gobierno", "mesanho_publicacion"))

# Fill missing OCI controls with zero (since they are not in the database of OCIs, they don't have one)
osce_infogob2 <- osce_infogob2 %>%
    replace_na(list(
        OCI_exists_any = 0, 
        OCI_exists_proportion = 0,
        OCI_exists_count = 0,
        OCI_incorporated_any = 0,
        OCI_incorporated_proportion = 0,
        OCI_incorporated_count = 0
    )) %>% 
    ungroup()

# save
write_parquet(osce_infogob2, "data/03_model/osce_infogob_oci_monthly.parquet") 
write.csv(osce_infogob2, "data/03_model/osce_infogob_oci_monthly.csv")

## semestral
osce_infogob3 <- semestral_indices_df %>% 
    mutate(semester = as.Date(semester)) %>% 
    left_join(osce_infogob_matching, by = c("gobierno")) %>% 
    left_join(controles_infogob_semestral %>% rename(semester = date), by = c("region", "provincia", "distrito", "semester")) %>% 
    left_join(controles_oci_semestral %>% rename(semester = date),
              by = c("gobierno", "semester"))

osce_infogob3 <- osce_infogob3 %>%
    replace_na(list(
        OCI_exists_any = 0, 
        OCI_exists_proportion = 0,
        OCI_exists_count = 0,
        OCI_incorporated_any = 0,
        OCI_incorporated_proportion = 0,
        OCI_incorporated_count = 0
    )) %>% 
    ungroup()

# save
write_parquet(osce_infogob3, "data/03_model/osce_infogob_oci_semester.parquet") 
write.csv(osce_infogob3, "data/03_model/osce_infogob_oci_semester.csv")
