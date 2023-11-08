# Load necessary libraries
library(tidyverse)
library(arrow)
library(lubridate)
library(broom)
library(lavaan)
library(plm)

# Set theme
theme_set(theme_bw())

# Read data
osce_infogob_oci_monthly <- read_parquet("src2/data/08_osce_infogob_oci_monthly_2017.parquet")#("data/03_model/osce_infogob_oci_monthly_2017_overall.parquet") %>% ungroup()
osce_infogob_oci_semestral <- read_parquet("src2/data/08_osce_infogob_oci_semester_2017.parquet")#"data/03_model/osce_infogob_oci_semester_2017_overall.parquet") %>% ungroup()
mef_infogob_oci_yearly <- read_parquet("src2/data/08_mef_infogob_oci_anual.parquet")#"data/03_model/mef_infogob_oci_anual.parquet") %>% ungroup()
mef_osce_matching <- read_parquet("src2/data/07_all_matches_mef_osce.parquet")#"data/02_intermediate/all_matches_mef_osce.parquet")

# Normalize osce_month database
osce_month <- osce_infogob_oci_monthly %>% 
    select(
        gobierno,
        mesanho_publicacion,
        tipo_municipalidad = tipo_municipalidad.x,
        avg_bidders_per_project,
        perc_single_bidder,
        avg_ratio_between_winner_and_publicado,
        perc_valor_adj_del_total,
        percentage_canon,
        percentage_canon_category,
        OCI_exists_any,
        OCI_incorporated_any,
        gender_of_mayor,
        numero_efectivo_partidos,
        competitividad_index
    ) %>% 
    mutate(mesanho_publicacion = ymd(mesanho_publicacion))

# Normalize osce_semester database
osce_semester <- osce_infogob_oci_semestral %>% 
    select(
        gobierno,
        mesanho_publicacion = semester,
        tipo_municipalidad = tipo_municipalidad.x,
        perc_repeated_postores,
        perc_repeated_winners
    ) %>%
    mutate(
        start_date = ymd(mesanho_publicacion),
        end_date = start_date + months(6) - days(1)
    ) %>%
    rowwise() %>%
    mutate(mesanho_publicacion = list(seq(start_date, end_date, by = "month"))) %>%
    ungroup() %>%
    unnest(mesanho_publicacion)

# Normalize mef_yearly database
mef_yearly <- mef_infogob_oci_yearly %>% 
    ungroup() %>% 
    select(
        gobierno,
        mesanho_publicacion = date,
        tipo_municipalidad = tipo_municipalidad.x,
        perc_proyectos_sobrecosto,
        perc_proyectos_debajo_cutoff
    ) %>%
    mutate(
        start_date = ymd(mesanho_publicacion),
        end_date = start_date + years(1) - days(1)
    ) %>%
    rowwise() %>%
    mutate(mesanho_publicacion = list(seq(start_date, end_date, by = "month"))) %>%
    ungroup() %>%
    unnest(mesanho_publicacion)

# Join the three dataframes
combined_data <- osce_month %>%
    left_join(osce_semester, by = c("mesanho_publicacion", "gobierno"), relationship = "many-to-many")
combined_data <- combined_data %>%
    left_join(mef_osce_matching, by = c("gobierno" = "gobierno_osce"))
combined_data <- combined_data %>%
    left_join(mef_yearly, by = c("mesanho_publicacion", "gobierno_mef" = "gobierno"))

combined_data <- combined_data %>%
    group_by(gobierno, mesanho_publicacion) %>%
    slice(1) %>%
    ungroup()

# Coalesce 'tipo_municipalidad'
combined_data <- combined_data %>%
    mutate(tipo_municipalidad = coalesce(tipo_municipalidad.x, tipo_municipalidad.y, tipo_municipalidad)) %>% 
    select(-tipo_municipalidad.x, -tipo_municipalidad.y, 
           -start_date.x, -start_date.y, -end_date.x, -end_date.y,
           -gobierno_mef)

combined_data <- combined_data %>% 
    filter(tipo_municipalidad != "Otra entidad") %>% 
    group_by(gobierno) %>% 
    fill(everything(), .direction = "down") %>% 
    ungroup()

# Dealing with NAs
combined_data_clean <- combined_data %>%
    mutate(numero_efectivo_partidos = if_else(is.infinite(numero_efectivo_partidos), mean(1), numero_efectivo_partidos)) %>% 
    select(gobierno, mesanho_publicacion, tipo_municipalidad, avg_bidders_per_project, avg_ratio_between_winner_and_publicado,
           perc_single_bidder, perc_repeated_winners, perc_proyectos_debajo_cutoff, perc_proyectos_sobrecosto, perc_valor_adj_del_total) %>%
    group_by(gobierno) %>%
    mutate(avg_bidders_per_project = -avg_bidders_per_project) %>%
    ungroup() %>% 
    na.omit()

# Indices
# To calculate the mean index we get scale over the mean and sd of the first year:

# Define custom scale function
custom_scale <- function(x, mean, sd) {
    return((x - mean) / sd)
}

# Calculate the mean and sd for the first year only
first_year <- filter(combined_data_clean, year(mesanho_publicacion) == min(year(mesanho_publicacion), na.rm = TRUE))
means_first_year <- colMeans(select(first_year, avg_bidders_per_project:perc_valor_adj_del_total), na.rm = TRUE)
sds_first_year <- apply(select(first_year, avg_bidders_per_project:perc_valor_adj_del_total), 2, sd, na.rm = TRUE)

# Apply custom scaling
combined_data_clean <- combined_data_clean %>%
    mutate(across(avg_bidders_per_project:perc_valor_adj_del_total, 
                  ~custom_scale(., means_first_year[[cur_column()]], 
                                sds_first_year[[cur_column()]]))) %>%
    mutate(mean_indices = rowMeans(select(., avg_bidders_per_project:perc_valor_adj_del_total)))

# Define the CFA model
cfa_model <- '
  F1 =~ avg_bidders_per_project + perc_single_bidder + avg_ratio_between_winner_and_publicado + perc_valor_adj_del_total
  F2 =~ perc_proyectos_debajo_cutoff  + perc_proyectos_sobrecosto + perc_repeated_winners
'

# Fit the model
fit_2_factors <- cfa(cfa_model, data = combined_data_clean)

# Summary of the model
summary(fit_2_factors, fit.measures = TRUE)

# Calculate factor scores
cfa_scores <- predict(fit_2_factors)

# Add to the data frame
combined_data_clean <- cbind(combined_data_clean, cfa_scores)

model_df <- combined_data_clean %>% 
    left_join(combined_data %>% select(gobierno, mesanho_publicacion,
                                       tipo_municipalidad,
                                       competitividad_index, numero_efectivo_partidos, percentage_canon_category,
                                       OCI_exists_any, gender_of_mayor, 
                                       percentage_canon,
                                       avg_bidders_per_project,
                                       avg_ratio_between_winner_and_publicado, perc_single_bidder,                   
                                       perc_repeated_winners, perc_proyectos_debajo_cutoff,         
                                       perc_proyectos_sobrecosto, perc_valor_adj_del_total),
              suffix = c("", "_original"),
              by = c("gobierno", "mesanho_publicacion", "tipo_municipalidad")) %>%
    group_by(gobierno, mesanho_publicacion, tipo_municipalidad) %>%
    slice(1) %>%
    ungroup() %>% 
    mutate(
        cutoff = mesanho_publicacion > "2015-03-01",
        time_var = as.numeric(mesanho_publicacion)/365,
        competitividad_category = case_when(competitividad_index < 1 ~ "<1",
                                            competitividad_index < 2 ~ "<2",
                                            competitividad_index < 3 ~ "<3",
                                            competitividad_index < 4 ~ "<4",
                                            competitividad_index > 5 ~ ">5",
                                            is.na(competitividad_index) ~ "No disponible"),
        numero_efectivo_partidos_category = case_when(numero_efectivo_partidos < 1 ~ "]0, 1]",
                                                      numero_efectivo_partidos < 2 ~ "]1, 2]",
                                                      numero_efectivo_partidos < 3 ~ "]2, 3]",
                                                      numero_efectivo_partidos < 4 ~ "]3, 4]",
                                                      numero_efectivo_partidos > 5 ~ "]5, Inf[",
                                                      is.na(numero_efectivo_partidos) ~ "No disponible"),
        month_publicacion = factor(month(mesanho_publicacion, label = TRUE), ordered = FALSE),
        perc_repeated_winners_original = perc_repeated_winners_original/100
    ) %>% 
    select(
        gobierno, mesanho_publicacion, time_var, tipo_municipalidad, cutoff,
        competitividad_category, numero_efectivo_partidos_category, percentage_canon_category, month_publicacion,
        percentage_canon, numero_efectivo_partidos, competitividad_index,
        OCI_exists_any, gender_of_mayor, mean_indices, F1, F2, 
        avg_bidders_per_project:perc_valor_adj_del_total, avg_bidders_per_project_original:perc_valor_adj_del_total_original
    ) %>% 
    group_by(gobierno, mesanho_publicacion, tipo_municipalidad) %>%
    slice(1) %>%
    ungroup() %>% 
    filter(mean_indices < quantile(mean_indices, probs = 0.99), mean_indices > quantile(mean_indices, probs = 0.01))

plm_model_df <- pdata.frame(model_df, index = c("gobierno", "mesanho_publicacion"))

# save
write_parquet(combined_data_clean, "data/02_intermediate/cleaned_data_overall.parquet")
write_parquet(model_df, "data/02_intermediate/model_data_overall.parquet")
write_rds(fit_2_factors, "data/03_model/2_factor_model.rds")
