### MATCHING ENTITIES ACROSS DIVERSE SOURCES
library(arrow)
library(tidyverse)
library(janitor)
library(readxl)
library(fuzzyjoin)

## OSCE - INFOGOB

# infogob
controles_infogob <- read_parquet("data/02_intermediate/controles_infogob.parquet")
combined_df_adjudicaciones <- read_parquet("data/02_intermediate/OSCE/merged_adjudicaciones_data.parquet")

# osce metrics
monthly_osce <- read_parquet("data/02_intermediate/OSCE/monthly_indices.parquet")

# Link UBIGEO to municipalidad, create full government name

controles_infogob <- controles_infogob %>% 
    mutate(
        full_name = case_when(
            tipo_municipalidad == "distrital" ~ str_c("MUNICIPALIDAD DISTRITAL DE", distrito, sep = " "),
            tipo_municipalidad == "provincial" ~ str_c("MUNICIPALIDAD PROVINCIAL DE", provincia, sep = " ")
        )
    )

# Do fuzzy matching on adjudicaciones to create a mapping
library(fuzzyjoin)

distritales_infogob <- controles_infogob %>% 
    filter(tipo_municipalidad == "distrital") %>%
    select(full_name) %>% 
    unique()

provinciales_infogob <- controles_infogob %>% 
    filter(tipo_municipalidad == "provincial") %>%
    select(full_name) %>% 
    unique()

distritales_adjudicaciones <- monthly_osce %>% 
    ungroup() %>% 
    filter(str_detect(gobierno, "^MUNICIPALIDAD DISTRITAL")) %>%
    select(gobierno) %>% 
    unique()

provinciales_adjudicaciones <- monthly_osce %>% 
    ungroup() %>% 
    filter(str_detect(gobierno, "^MUNICIPALIDAD PROVINCIAL")) %>%
    select(gobierno) %>% 
    unique()

distrital_matching <- stringdist_full_join(
    distritales_adjudicaciones, 
    distritales_infogob, 
    by = c("gobierno" = "full_name"), 
    max_dist = 15, method = "lcs",
    distance_col = "string_distance"
)

distrital_matching <- distrital_matching %>% 
    group_by(gobierno) %>% 
    slice_min(string_distance, n = 1, with_ties = FALSE)

provincial_matching <- stringdist_left_join(
    provinciales_adjudicaciones, 
    provinciales_infogob, 
    by = c("gobierno" = "full_name"), 
    max_dist = 15, method = "lcs",
    distance_col = "string_distance"
)

provincial_matching <- provincial_matching %>% 
    group_by(gobierno) %>% 
    slice_min(string_distance, n = 1, with_ties = FALSE)

osce_infogob_matching <- distrital_matching %>% 
    bind_rows(provincial_matching)

# write
write_parquet(osce_infogob_matching, "data/02_intermediate/osce_infogob_entity_name_matching.parquet")

### LINK gobiernos CON OCI with OSCE

OCI_df <- read_excel("data/01_raw/ESTADO DE ENTIDADES CON OCI INCORPORADOS AL_15FEB2023_rev (1).xlsx", sheet = 2)
OCI_df <- clean_names(OCI_df)

# Convert RUC columns to character for correct comparison
OCI_df$ruc_entidad <- as.character(OCI_df$ruc_entidad)
combined_df_adjudicaciones$ruc_entidad <- as.character(combined_df_adjudicaciones$ruc_entidad)

regex_pattern <- "^(GOBIERNO REGIONAL (?:DE )?\\w+|MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DE )?[\\w\\s]+?(?= -|- |$))"

# Remove duplicates
OCI_df <- OCI_df %>%
    distinct(ruc_entidad, nombre_entidad)

combined_df_adjudicaciones <- combined_df_adjudicaciones %>%
    mutate(gobierno = str_extract(ENTIDAD, regex_pattern),
           gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central")) %>% 
    distinct(ruc_entidad, gobierno)

# Exact match using RUC
exact_match <- OCI_df %>%
    select(ruc_entidad, nombre_entidad) %>%
    inner_join(combined_df_adjudicaciones, by = c("ruc_entidad" = "ruc_entidad"))

# Find the names that did not match using RUC
OCI_remaining <- OCI_df %>%
    anti_join(exact_match, by = c("ruc_entidad" = "ruc_entidad"))

adjudicaciones_remaining <- combined_df_adjudicaciones %>%
    anti_join(exact_match, by = c("ruc_entidad" = "ruc_entidad"))

# Perform fuzzy matching for the remaining entities
OCI_remaining_unique <- OCI_remaining %>% 
    # mutate(
    #     nombre_entidad = str_extract(nombre_entidad, regex_pattern)
    # ) %>%  
    select(nombre_entidad) %>% 
    distinct()

adjudicaciones_remaining_unique <- adjudicaciones_remaining %>% 
    select(gobierno) %>% 
    distinct()

fuzzy_match <- stringdist_left_join(
    adjudicaciones_remaining_unique, 
    OCI_remaining_unique, 
    by = c("gobierno" = "nombre_entidad"), 
    max_dist = 2, 
    method = "lcs",
    distance_col = "string_distance"
)

fuzzy_match <- fuzzy_match %>% 
    group_by(gobierno) %>% 
    slice_min(string_distance, n = 1, with_ties = FALSE)

# Combine exact and fuzzy matches
all_matches <- exact_match %>%
    bind_rows(fuzzy_match)

# There are many entities in a GOBIERNO. Let's take the existence of at least 1 as 1.

# save
write_parquet(all_matches, "data/02_intermediate/osce_oci_entity_name_matching.parquet")

# If the entity was not found in the OCI data, it means that that entity had no OCI. So we need
# to add the zeroes to our dataset.

# get OCI data
source("src/02_limpieza_de_datos/Controles/control_entidades_con_OCI.R")

# Merge with the panel data and fill in zeros for the OCI indices if an entity did not have an OCI
monthly_panel <- monthly_panel %>%
    full_join(all_matches, by = "nombre_entidad") %>%
    replace_na(list(OCI_exists = 0, OCI_incorporated = 0))

semestral_panel <- semestral_panel %>%
    full_join(all_matches, by = "nombre_entidad") %>%
    replace_na(list(OCI_exists = 0, OCI_incorporated = 0))

# Summarise monthly panel: Many GOBIERNOS have various "entities", each one with their OCI.
monthly_panel <- monthly_panel %>%
    group_by(gobierno, date) %>%
    summarise(
        OCI_exists_any = max(OCI_exists, na.rm = TRUE),
        OCI_exists_proportion = mean(OCI_exists, na.rm = TRUE),
        OCI_exists_count = sum(OCI_exists, na.rm = TRUE),
        OCI_incorporated_any = max(OCI_incorporated, na.rm = TRUE),
        OCI_incorporated_proportion = mean(OCI_incorporated, na.rm = TRUE),
        OCI_incorporated_count = sum(OCI_incorporated, na.rm = TRUE),
        .groups = "drop"
    )

# Summarise semestral panel
semestral_panel <- semestral_panel %>%
    group_by(gobierno, date) %>%
    summarise(
        OCI_exists_any = max(OCI_exists, na.rm = TRUE),
        OCI_exists_proportion = mean(OCI_exists, na.rm = TRUE),
        OCI_exists_count = sum(OCI_exists, na.rm = TRUE),
        OCI_incorporated_any = max(OCI_incorporated, na.rm = TRUE),
        OCI_incorporated_proportion = mean(OCI_incorporated, na.rm = TRUE),
        OCI_incorporated_count = sum(OCI_incorporated, na.rm = TRUE),
        .groups = "drop"
    )

# Rewrite and update
write_parquet(monthly_panel, "data/02_intermediate/controles_OCI_mensual.parquet")
write_parquet(semestral_panel, "data/02_intermediate/controles_OCI_semestral.parquet")

### MEF - OCI
mef_data <- read_parquet("data/02_intermediate/MEF/mef_data.parquet")

OCI_df <- read_excel("data/01_raw/ESTADO DE ENTIDADES CON OCI INCORPORADOS AL_15FEB2023_rev (1).xlsx", sheet = 2)
OCI_df <- clean_names(OCI_df)

fuzzy_match <- stringdist_left_join(
    mef_data %>% distinct(gobierno), 
    OCI_df %>% distinct(nombre_entidad), 
    by = c("gobierno" = "nombre_entidad"), 
    max_dist = 5, 
    method = "lcs",
    distance_col = "string_distance"
)

fuzzy_match <- fuzzy_match %>% 
    group_by(gobierno) %>% 
    slice_min(string_distance, n = 1, with_ties = FALSE)

# save
write_parquet(fuzzy_match, "data/02_intermediate/mef_oci_matching.parquet")

### MEF - INFOGOB
controles_infogob <- read_parquet("data/02_intermediate/controles_infogob.parquet")

controles_infogob <- controles_infogob %>% 
    mutate(
        full_name = case_when(
            tipo_municipalidad == "distrital" ~ str_c("MUNICIPALIDAD DISTRITAL DE", distrito, sep = " "),
            tipo_municipalidad == "provincial" ~ str_c("MUNICIPALIDAD PROVINCIAL DE", provincia, sep = " ")
        )
    )


fuzzy_match <- stringdist_left_join(
    mef_data %>% ungroup() %>%  distinct(gobierno), 
    controles_infogob %>% distinct(full_name), 
    by = c("gobierno" = "full_name"), 
    max_dist = 12, 
    method = "lcs",
    distance_col = "string_distance"
)

fuzzy_match <- fuzzy_match %>% 
    group_by(gobierno) %>% 
    slice_min(string_distance, n = 1, with_ties = FALSE)

# save
write_parquet(fuzzy_match, "data/02_intermediate/mef_infogob_matching.parquet")

