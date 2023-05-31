library(lubridate)
library(tidyverse)
library(skimr)
library(arrow)

### ADJUDICACIONES

combined_df_adjudicaciones <- read_parquet("data/02_intermediate/OSCE/merged_adjudicaciones_data.parquet")
combined_df_postores <- read_parquet("data/02_intermediate/OSCE/combined_postores_data.parquet")

# Desestimando entidades ejecutoras y colapsando al gobierno

regex_pattern <- "^(GOBIERNO REGIONAL (?:DE )?\\w+|MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DE )?[\\w\\s]+?(?= -|- |$))"

df_adjudicaciones <- combined_df_adjudicaciones %>% 
    filter(fecha_publicacion > "2010-01-01") %>% 
    mutate(gobierno = str_extract(ENTIDAD, regex_pattern),
           gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
           mesanho_publicacion = floor_date(fecha_publicacion, "month"))

combined_df_postores <- combined_df_postores %>% 
    mutate(
        gobierno = str_extract(ENTIDAD, regex_pattern),
        gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
        mesanho_publicacion = floor_date(fecha_publicacion, "month")
    )

## NUMERO DE GANADORES DE PROYECTOS EN MUNICIPALIDADES:

numero_ganadores <- df_adjudicaciones %>% 
    group_by(mesanho_publicacion, gobierno) %>% 
    summarise(
        n_ganadores = n_unique(ruc_ganador)
    )
# Valor medio de los proyectos adjudicados (TODOS)

valor_proyectos_adjudicados <- df_adjudicaciones %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        valor_adjudicado_mean = mean(valor_adjudicado_item, na.rm = TRUE)
    )

# Valor promedio de los proyectos adjudicados (valor del item menor a S/. 1,000,000)
valor_proyectos_adjudicados <- df_adjudicaciones %>%
    filter(valor_adjudicado_item < 1000000) %>% 
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        valor_adjudicado_mean = mean(valor_adjudicado_item, na.rm = TRUE)
    )

# Tiempo promedio entre fecha de publicacion y fecha de buena pro
tiempo_promedio_publicacion_buenapro <- df_adjudicaciones %>%
    mutate(time_diff_days = as.numeric(difftime(fecha_buenapro, fecha_publicacion, units = "days"))) %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        tiempo_promedio = mean(time_diff_days, na.rm = TRUE)
    )

# Calculate the number of bidders per project
bidder_count <- combined_df_postores %>%
    group_by(ruc_entidad, nombre_proceso, mesanho_publicacion, gobierno) %>%
    summarise(n_bidders = n()) 

# Create a column to indicate if a project has only one bidder
bidder_count <- bidder_count %>%
    mutate(single_bidder = n_bidders == 1)

# Calculate the percentage of projects with a single bidder
single_bidder_percentage <- bidder_count %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        total_projects = n(),
        single_bidder_projects = sum(single_bidder),
        perc_single_bidder = (single_bidder_projects / total_projects) * 100
    )

# Calculate the average number of bidders per project
average_bidder_count <- combined_df_postores %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        total_bidders = n(),
        total_projects = n_distinct(nombre_proceso),
        avg_bidders_per_project = total_bidders / total_projects
    )


# Identify single bidder projects
single_bidder_projects <- combined_df_postores %>%
    group_by(gobierno, nombre_proceso) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    select(gobierno, nombre_proceso)

# Join with the adjudicaciones dataset
single_bidder_adjudicaciones <- df_adjudicaciones %>%
    inner_join(single_bidder_projects,
               by = c("gobierno", "nombre_proceso"))

# Calculate the difference and ratio
single_bidder_metrics <- single_bidder_adjudicaciones %>%
    mutate(diff_valor = valor_adjudicado_item - valor_referencial_item,
           ratio_valor = valor_adjudicado_item / valor_referencial_item - 1) %>%
    filter(ratio_valor < 2, # there is one anomalous month. Only this month gets filtered by this
           diff_valor < 100000 # there is one anomalous month. Only this month gets filtered by this
    ) %>% 
    group_by(mesanho_publicacion = floor_date(fecha_publicacion, "month"), gobierno) %>%
    summarise(
        avg_diff = mean(diff_valor, na.rm = TRUE),
        avg_ratio = mean(ratio_valor, na.rm = TRUE)
    )

# Calculate Postores universe and postores intersection 6-month interval by 6 month-interval

# Break down the data into 6-month intervals
combined_df_postores$semestre_publicacion <- floor_date(combined_df_postores$fecha_publicacion, "6 months")

# Group by entity and 6-month time interval and get the unique postores
postores_per_entity <- combined_df_postores %>% 
    group_by(gobierno, semester = semestre_publicacion) %>%
    summarise(postores = list(unique(ruc_postor))) %>%
    arrange(gobierno, semester)

# Define a function to get the overlap (repetition) of postores
get_overlap <- function(current, previous) {
    return(length(intersect(current, previous)))
}

# Calculate the repeated postores for each entity and 6-month interval
postores_overlap <- postores_per_entity %>%
    group_by(gobierno) %>%
    mutate(
        postores_previous = lag(postores),
        repeated_postores = map2_int(postores, postores_previous, get_overlap),
        total_postores = map_int(postores, length),
        perc_repeated_postores = repeated_postores / total_postores * 100
    ) %>%
    select(-postores_previous, -postores)

## Concentration indices

# Concentration Index
combined_df_adjudicaciones <- df_adjudicaciones %>% 
    mutate(
        semester = floor_date(fecha_publicacion, unit = "6 month"),
        monto_total = valor_adjudicado_item * cantidad_adjudicada
    ) 

total_value <- combined_df_adjudicaciones %>% 
    group_by(gobierno, semester) %>% 
    arrange(gobierno, semester) %>% 
    summarise(total_value = sum(monto_total, na.rm = TRUE)) 

combined_df_adjudicaciones <- combined_df_adjudicaciones %>% 
    left_join(total_value, by = c("gobierno", "semester"))

concentration_index_at_5 <- combined_df_adjudicaciones %>% 
    group_by(gobierno, semester) %>% 
    slice_max(monto_total, n = 5) %>% 
    group_by(gobierno, semester) %>% 
    summarise(proportion = sum(monto_total, na.rm = TRUE)/first(total_value))

concentration_index_at_3 <- combined_df_adjudicaciones %>% 
    group_by(gobierno, semester) %>% 
    slice_max(monto_total, n = 3) %>% 
    group_by(gobierno, semester) %>% 
    summarise(proportion = sum(monto_total, na.rm = TRUE)/first(total_value))

concentration_index_at_10 <- combined_df_adjudicaciones %>% 
    group_by(gobierno, semester) %>% 
    slice_max(monto_total, n = 10) %>% 
    group_by(gobierno, semester) %>% 
    summarise(proportion = sum(monto_total, na.rm = TRUE)/first(total_value))

# Don't forget to update the coverage function as well
coberture_at_80_index <- combined_df_adjudicaciones %>%
    group_by(gobierno, semester) %>%
    arrange(gobierno, semester, desc(monto_total)) %>% 
    mutate(cum_monto = cumsum(monto_total)) %>%
    summarise(num_projects = sum(cum_monto <= total_value * 0.8) + 1) %>%
    ungroup()

percent_coberture_at_80_index <- combined_df_adjudicaciones %>%
    group_by(gobierno, semester) %>%
    arrange(gobierno, semester, desc(monto_total)) %>% 
    mutate(cum_monto = cumsum(monto_total)) %>%
    summarise(perc_projects = (sum(cum_monto <= total_value * 0.8) + 1) / n(),
              n = n()) %>%
    ungroup()

# Centralise the new metrics and save

## MONTHLY

# Join all indices in one dataframe
indices_df <- numero_ganadores %>%
    full_join(valor_proyectos_adjudicados, by = c("mesanho_publicacion", "gobierno")) %>%
    full_join(tiempo_promedio_publicacion_buenapro, by = c("mesanho_publicacion", "gobierno")) %>%
    full_join(single_bidder_percentage, by = c("mesanho_publicacion", "gobierno")) %>%
    full_join(average_bidder_count, by = c("mesanho_publicacion", "gobierno")) %>%
    full_join(single_bidder_metrics, by = c("mesanho_publicacion", "gobierno"))

# save as parquet
write_parquet(indices_df, "data/02_intermediate/OSCE/monthly_indices.parquet")

## SEMESTRAL

# Rename 'proportion' column for each concentration index
concentration_index_at_5 <- concentration_index_at_5 %>% 
    rename(concentration_at_5 = proportion)

concentration_index_at_3 <- concentration_index_at_3 %>% 
    rename(concentration_at_3 = proportion)

concentration_index_at_10 <- concentration_index_at_10 %>% 
    rename(concentration_at_10 = proportion)

# Join concentration indices with semestral_df
semestral_df <- postores_overlap %>% 
    left_join(concentration_index_at_3, by = c("gobierno", "semester")) %>%
    left_join(concentration_index_at_5, by = c("gobierno", "semester")) %>%
    left_join(concentration_index_at_10, by = c("gobierno", "semester"))

# Rename columns in coverage index dataframes
coberture_at_80_index <- coberture_at_80_index %>% 
    rename(coberture_at_80 = num_projects)

percent_coberture_at_80_index <- percent_coberture_at_80_index %>% 
    rename(perc_coberture_at_80 = perc_projects)

# Join coverage indices with semestral_df
semestral_df <- semestral_df %>%
    left_join(coberture_at_80_index, by = c("gobierno", "semester")) %>%
    left_join(percent_coberture_at_80_index, by = c("gobierno", "semester"))

# save semestral data
write_parquet(semestral_df, "data/02_intermediate/OSCE/semestral_indices.parquet")
