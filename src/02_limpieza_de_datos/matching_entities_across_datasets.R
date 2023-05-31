### MATCHING ENTITIES ACROSS DIVERSE SOURCES

## OSCE - INFOGOB
controles_infogob <- read_parquet("data/02_intermediate/controles_infogob.parquet")
combined_df_adjudicaciones <- read_parquet("data/02_intermediate/OSCE/combined_adjudicaciones_data.parquet")

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

distritales_adjudicaciones <- combined_df_adjudicaciones %>% 
    filter(str_detect(ENTIDAD, "^MUNICIPALIDAD DISTRITAL")) %>%
    select(ENTIDAD) %>% 
    unique()

provinciales_adjudicaciones <- combined_df_adjudicaciones %>% 
    filter(str_detect(ENTIDAD, "^MUNICIPALIDAD PROVINCIAL")) %>%
    select(ENTIDAD) %>% 
    unique()


distrital_matching <- stringdist_full_join(
    distritales_adjudicaciones, 
    distritales_infogob, 
    by = c("ENTIDAD" = "full_name"), 
    max_dist = 15, method = "lcs",
    distance_col = "string_distance"
)

distrital_matching <- distrital_matching %>% 
    group_by(ENTIDAD) %>% 
    slice_min(string_distance, n = 1, with_ties = FALSE)


provincial_matching <- stringdist_left_join(
    provinciales_adjudicaciones, 
    provinciales_infogob, 
    by = c("ENTIDAD" = "full_name"), 
    max_dist = 15, method = "lcs",
    distance_col = "string_distance"
)

provincial_matching <- provincial_matching %>% 
    group_by(ENTIDAD) %>% 
    slice_min(string_distance, n = 1, with_ties = FALSE)


osce_infogob_matching <- distrital_matching %>% 
    bind_rows(provincial_matching)
