## JOINING SERIES AND CONTROLS
library(tidyverse)
library(arrow)

# Monthly datasets
monthly_indices_df <- read_parquet("src2/data/03_monthly_indices_2017.parquet")
controles_oci_monthly <- read_parquet("src2/data/04_controles_OCI_mensual.parquet")
controles_infogob_monthly <- read_parquet("src2/data/05_controles_infogob_mensual.parquet")
controles_canon_monthly <- read_parquet("src2/data/06_controles_percentage_canon_mensual.parquet")

# Semestral datasets
semestral_indices_df <- read_parquet("src2/data/03_semestral_indices_2017.parquet")
controles_oci_semestral <- read_parquet("src2/data/04_controles_OCI_semestral.parquet")
controles_infogob_semestral <- read_parquet("src2/data/05_controles_infogob_semestral.parquet")
controles_canon_semestral <- read_parquet("src2/data/06_controles_percentage_canon_semestral.parquet")

# Matching datasets
osce_infogob_matching <- read_parquet("src2/data/07_osce_infogob_entity_name_matching.parquet")
osce_oci_matching <- read_parquet("src2/data/07_osce_oci_entity_name_matching.parquet")
mef_osce_matching <- read_parquet("src2/data/07_all_matches_mef_osce.parquet")

# MEF datasets
mef_indices_df <- read_parquet("src2/data/01_mef_data.parquet")
mef_infogob_matching <- read_parquet("src2/data/07_mef_infogob_matching.parquet")
mef_oci_matching <- read_parquet("src2/data/07_mef_oci_matching.parquet")

# Yearly datasets
controles_oci_yearly <- read_parquet("src2/data/04_controles_OCI_yearly.parquet")
controles_infogob_yearly <- read_parquet("src2/data/05_controles_infogob_anual.parquet")
controles_canon_yearly <- read_parquet("src2/data/06_controles_percentage_canon_anual.parquet")

### OSCE
## Monthly
osce_infogob2 <- monthly_indices_df %>% ungroup() %>% 
    mutate(mesanho_publicacion = as.Date(mesanho_publicacion)) %>% 
    left_join(osce_infogob_matching, by = c("gobierno")) %>% 
    left_join(mef_osce_matching, by = c("gobierno" = "gobierno_mef")) %>% 
    left_join(controles_canon_monthly, by = c("gobierno_osce" = "gobierno", "mesanho_publicacion" = "ANO_EJE")) %>% 
    left_join(controles_infogob_monthly %>% rename(mesanho_publicacion = date), by = c("region", "provincia", "distrito", "mesanho_publicacion")) %>% 
    left_join(controles_oci_monthly %>% rename(mesanho_publicacion = date),
              by = c("gobierno" = "nombre_entidad", "mesanho_publicacion"))

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
write_parquet(osce_infogob2, "src2/data/08_osce_infogob_oci_monthly_2017.parquet") 
write.csv(osce_infogob2, "src2/data/08_osce_infogob_oci_monthly_2017.csv")

## semestral
osce_infogob3 <- semestral_indices_df %>% 
    mutate(semester = as.Date(semester)) %>% 
    left_join(osce_infogob_matching, by = c("gobierno")) %>% 
    left_join(controles_infogob_semestral %>% rename(semester = date), by = c("region", "provincia", "distrito", "semester")) %>% 
    left_join(controles_oci_semestral %>% rename(semester = date),
              by = c("gobierno" = "nombre_entidad", "semester"))

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

controles_infogob_monthly %>% 
    distinct(year, numero_efectivo_partidos) %>% 
    filter(numero_efectivo_partidos < 10) %>% 
    group_by(year) %>% 
    summarise(
        mean = mean(numero_efectivo_partidos, na.rm = TRUE),
        sd = sd(numero_efectivo_partidos, na.rm = TRUE)
    ) %>% clipr::write_clip()

# save
write_parquet(osce_infogob3, "src2/data/08_osce_infogob_oci_semester_2017.parquet") 
write.csv(osce_infogob3, "src2/data/08_osce_infogob_oci_semester_2017.csv")

### MEF
mef_total <- mef_indices_df %>% 
    mutate(
        date = as.Date(paste(ANO_EJE, 1, 1, sep = "-"))
    ) %>% 
    left_join(mef_oci_matching %>% ungroup() %>% distinct(gobierno, nombre_entidad)) %>% 
    left_join(mef_infogob_matching %>% ungroup() %>% distinct(gobierno, full_name, region, provincia, distrito)) %>% 
    left_join(controles_infogob_yearly, by = c("region", "provincia", "distrito", "date")) %>% 
    left_join(controles_oci_yearly,
              by = c("nombre_entidad", "date"))

# Save paths for MEF
write_parquet(mef_total, "src2/data/08_mef_infogob_oci_anual.parquet")
