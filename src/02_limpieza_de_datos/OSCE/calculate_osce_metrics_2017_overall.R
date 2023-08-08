### Metricas mensuales 2010-2017
library(tidyverse)
library(lubridate)
library(arrow)

# load data
combined_df_adjudicaciones <- read_parquet("data/02_intermediate/OSCE/combined_adjudicaciones_data_2017.parquet")
combined_df_postores <- read_parquet("data/02_intermediate/OSCE/combined_postores_data_2017.parquet")
combined_df_contratos <- read_parquet("data/02_intermediate/OSCE/combined_contratos_data_2017.parquet")
combined_df_invitados <- read_parquet("data/02_intermediate/OSCE/combined_invitados_data_2017.parquet")
combined_df_pac <- read_parquet("data/02_intermediate/OSCE/combined_pac_data_2017.parquet")

# get IDs and time
regex_pattern <- "^(GOBIERNO REGIONAL (?:DE )?\\w+|MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DE )?[\\w\\s]+?(?= -|- |$))"
combined_df_adjudicaciones <- combined_df_adjudicaciones %>% 
    filter(fecha_publicacion > "2010-01-01") %>% 
    mutate(
        gobierno = str_extract(ENTIDAD, regex_pattern),
        gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
        tipo_municipalidad = case_when(str_detect(gobierno, "PROVINCIAL") ~ "provincial",
                                       str_detect(gobierno, "REGI") ~ "regional",
                                       str_detect(gobierno, "DIS") ~ "distrital",
                                       TRUE ~ "Otra entidad"),
        mesanho_publicacion = floor_date(fecha_publicacion, "month"),
        semester = floor_date(fecha_publicacion, unit = "6 month"),
        monto_total = valor_adjudicado_item * cantidad_adjudicada
    ) 

combined_df_postores <- combined_df_postores %>% 
    mutate(
        semester = floor_date(combined_df_postores$fecha_publicacion, "6 months"),
        gobierno = str_extract(ENTIDAD, regex_pattern),
        gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
        tipo_municipalidad = case_when(str_detect(gobierno, "PROVINCIAL") ~ "provincial",
                                       str_detect(gobierno, "REGI") ~ "regional",
                                       str_detect(gobierno, "DIS") ~ "distrital",
                                       TRUE ~ "Otra entidad"),
        mesanho_publicacion = floor_date(fecha_publicacion, "month")
    )

combined_df_invitados <- combined_df_invitados %>% 
    mutate(
        semester = floor_date(fecha_publicacion, "6 months"),
        gobierno = str_extract(entidad, regex_pattern),
        gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
        tipo_municipalidad = case_when(str_detect(gobierno, "PROVINCIAL") ~ "provincial",
                                       str_detect(gobierno, "REGI") ~ "regional",
                                       str_detect(gobierno, "DIS") ~ "distrital",
                                       TRUE ~ "Otra entidad"),
        mesanho_publicacion = floor_date(fecha_publicacion, "month")
    )

combined_df_pac <- combined_df_pac %>% 
    mutate(
        mesanho_publicacion = as.Date(str_c(ANO, "-", MES_PREVISTO, "-", "01"), format = "%Y-%m-%d"),
        gobierno = str_extract(ENTIDAD, regex_pattern),
        gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
        tipo_municipalidad = case_when(str_detect(gobierno, "PROVINCIAL") ~ "provincial",
                                       str_detect(gobierno, "REGI") ~ "regional",
                                       str_detect(gobierno, "DIS") ~ "distrital",
                                       TRUE ~ "Otra entidad"),
    )


#### MENSUALES

## Numero de ganadores de proyectos
numero_ganadores <- combined_df_adjudicaciones %>% 
    group_by(mesanho_publicacion, gobierno) %>% 
    summarise(
        n_ganadores = n_distinct(ruc_ganador),
        n_projects = n_distinct(nombre_proceso)
    )

## Valor adjudicado promedio
valor_proyectos_adjudicados <- combined_df_adjudicaciones %>%
    group_by(gobierno, mesanho_publicacion) %>%
    summarise(
        monthly_valor_adjudicado_mean = mean(monto_total, na.rm = TRUE)
    )

# desviacion_valor_proyecto_adjudicado <- combined_df_adjudicaciones %>%
#     group_by(gobierno, mesanho_publicacion) %>%
#     arrange(gobierno, mesanho_publicacion) %>%
#     transmute(last_12_months_start = mesanho_publicacion %m-% months(11)) %>%
#     ungroup() %>%
#     left_join(combined_df_adjudicaciones %>% 
#                   select(gobierno, fecha_publicacion, monto_total),
#               by = c("gobierno"), relationship = "many-to-many") %>%
#     filter(fecha_publicacion >= last_12_months_start,
#            fecha_publicacion <= mesanho_publicacion) %>%
#     group_by(gobierno, mesanho_publicacion) %>%
#     summarise(mean_pool_12_months = mean(monto_total, na.rm = TRUE))
# 
# # fill forward
# desviacion_valor_proyecto_adjudicado <- desviacion_valor_proyecto_adjudicado %>%
#     ungroup() %>% 
#     complete(gobierno, 
#              mesanho_publicacion = seq.Date(min(mesanho_publicacion), 
#                                             max(mesanho_publicacion), 
#                                             by = "month")) %>%
#     fill(mean_pool_12_months)
# 
# valor_proyectos_adjudicados_respecto_al_anual <- valor_proyectos_adjudicados %>% 
#     left_join(desviacion_valor_proyecto_adjudicado) %>% 
#     mutate(
#         desvicacion_respecto_al_promedio_anual = monthly_valor_adjudicado_mean - mean_pool_12_months
#     )

## Numero de procesos con un solo postor:
# total de postores
bidder_count <- combined_df_postores %>%
    group_by(gobierno, nombre_proceso, mesanho_publicacion) %>%
    summarise(n_bidders = n()) %>%
    ungroup()

# Create a column to indicate if a project has only one bidder
bidder_count <- bidder_count %>%
    mutate(single_bidder = n_bidders == 1)

# Calculate the percentage of projects with a single bidder
single_bidder_percentage <- bidder_count %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        total_projects = n_distinct(nombre_proceso),
        total_bidders = sum(n_bidders, na.rm = TRUE),
        single_bidder_projects = sum(single_bidder, na.rm = TRUE),
        perc_single_bidder = (single_bidder_projects / total_projects)
    ) %>% ungroup()

# Calculate the average number of bidders per project
# average_bidder_count <- combined_df_postores %>%
#     group_by(mesanho_publicacion, gobierno) %>%
#     summarise(
#         total_bidders_postores = n(),
#         total_projects_postores = n_distinct(nombre_proceso),
#         avg_bidders_per_project = total_bidders_postores / total_projects_postores,
#     ) %>% ungroup()

average_bidder_count <- combined_df_postores %>%
    group_by(mesanho_publicacion, gobierno, nombre_proceso) %>%
    summarise(total_bidders = n()) %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(avg_bidders_per_project = mean(total_bidders, na.rm = TRUE)) %>%
    ungroup()


# Identify single bidder projects
single_bidder_projects <- combined_df_postores %>%
    group_by(gobierno, nombre_proceso) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    select(gobierno, nombre_proceso)

# Join with the adjudicaciones dataset
single_bidder_adjudicaciones <- combined_df_adjudicaciones %>%
    left_join(single_bidder_projects,
              by = c("gobierno", "nombre_proceso"))

# Calculate the difference and ratio
single_bidder_metrics <- combined_df_adjudicaciones %>%
    mutate(diff_valor = valor_adjudicado_item - valor_referencial_item,
           ratio_valor = valor_adjudicado_item / valor_referencial_item - 1) %>%
    filter(ratio_valor < 2, # there is one anomalous month. Only this month gets filtered by this
           diff_valor < 100000 # there is one anomalous month. Only this month gets filtered by this
    ) %>% 
    group_by(mesanho_publicacion = floor_date(fecha_publicacion, "month"), gobierno) %>%
    summarise(
        avg_diff_between_winner_and_publicado = mean(diff_valor, na.rm = TRUE),
        avg_ratio_between_winner_and_publicado = mean(ratio_valor, na.rm = TRUE)
    )

# single_bidder_percentage %>% 
#     mutate(
#         tipo_municipalidad = case_when(str_detect(gobierno, "PROVINCIAL") ~ "provincial",
#                                        str_detect(gobierno, "REGI") ~ "regional",
#                                        str_detect(gobierno, "DIS") ~ "distrital",
#                                        TRUE ~ "Otras entidades")
#     ) %>% 
#     group_by(mesanho_publicacion, tipo_municipalidad) %>% 
#     summarise(
#         perc_single_bidder = mean(perc_single_bidder, na.rm = TRUE),
#     ) %>% 
#     ggplot(aes(x = mesanho_publicacion, y = perc_single_bidder, color = tipo_municipalidad)) +
#     geom_line(alpha = 1) +
#     geom_vline(xintercept = as.POSIXct("2014-10-05"))
# 
# single_bidder_metrics %>% 
#     mutate(
#         tipo_municipalidad = case_when(str_detect(gobierno, "PROVINCIAL") ~ "provincial",
#                                        str_detect(gobierno, "REGI") ~ "regional",
#                                        str_detect(gobierno, "DIS") ~ "distrital",
#                                        TRUE ~ "Otras entidades")
#     ) %>% 
#     group_by(mesanho_publicacion, tipo_municipalidad) %>% 
#     summarise(
#         avg_ratio_between_winner_and_publicado = mean(avg_ratio_between_winner_and_publicado, na.rm = TRUE),
#     ) %>% 
#     ggplot(aes(x = mesanho_publicacion, y = avg_ratio_between_winner_and_publicado, color = tipo_municipalidad)) +
#     geom_line(alpha = 1) +
#     geom_vline(xintercept = as.POSIXct("2014-10-05"))

# bidder_count %>% 
#     mutate(
#         tipo_municipalidad = case_when(str_detect(gobierno, "PROVINCIAL") ~ "provincial",
#                                        str_detect(gobierno, "REGI") ~ "regional",
#                                        str_detect(gobierno, "DIS") ~ "distrital",
#                                        TRUE ~ "Otras entidades")
#     ) %>% 
#     group_by(mesanho_publicacion, tipo_municipalidad) %>% 
#     summarise(
#        n_bidders  = mean(n_bidders, na.rm = TRUE),
#     ) %>% 
#     filter(n_bidders < 4) %>% 
#     ggplot(aes(x = mesanho_publicacion, y = n_bidders, color = tipo_municipalidad)) +
#     geom_line(alpha = 1) +
#     geom_vline(xintercept = as.POSIXct("2014-10-05"))

## Tiempo promedio publicación buena pro:
tiempo_promedio_publicacion_buenapro <- combined_df_adjudicaciones %>%
    ungroup() %>% 
    mutate(time_diff_days = as.numeric(difftime(fecha_buenapro, fecha_publicacion, units = "days"))) %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        tiempo_promedio = mean(time_diff_days, na.rm = TRUE)
    )

## Valor total de proyectos a adjudicacion directa selectiva
n_proyectos_pac <- combined_df_pac %>% 
    filter(OBJETO != "Otro objeto") %>% 
    group_by(mesanho_publicacion, gobierno) %>% 
    summarise(
        valor_total_adjudicaciones_all = sum(VALOR_ESTIMADO, na.rm = TRUE)/1000,
        numero_total_adjudicaciones_all = n_distinct(DESCRIPCION_PROCESO),
    ) %>% 
    ungroup()

valor_proyectos_adjudicacion_directa <- combined_df_pac %>% 
    filter(OBJETO != "Otro objeto") %>% 
    group_by(mesanho_publicacion, gobierno) %>% 
    filter(TIPO_PROCESO %in% c("ADJUDICACION DIRECTA SELECTIVA", "ADJUDICACION DIRECTA PUBLICA", "Adjudicación Simplificada")) %>% 
    summarise(
        valor_total_adjudicaciones_adj = sum(VALOR_ESTIMADO, na.rm = TRUE)/1000,
        numero_total_adjudicaciones_adj = n_distinct(DESCRIPCION_PROCESO),
    ) %>% 
    ungroup()

indices_proyectos_adjudicacion <- n_proyectos_pac %>% 
    left_join(valor_proyectos_adjudicacion_directa, by = c("mesanho_publicacion", "gobierno"))

indices_proyectos_adjudicacion <-  indices_proyectos_adjudicacion %>% 
    mutate(
        valor_total_adjudicaciones_adj = coalesce(valor_total_adjudicaciones_adj, 0),
        numero_total_adjudicaciones_adj = coalesce(numero_total_adjudicaciones_adj, 0),
        perc_valor_adj_del_total = valor_total_adjudicaciones_adj / valor_total_adjudicaciones_all,
        perc_numero_adj_del_total = numero_total_adjudicaciones_adj / numero_total_adjudicaciones_all
    )

## MERGE MENSUAL
indices_df <- numero_ganadores %>%
    full_join(tiempo_promedio_publicacion_buenapro, by = c("mesanho_publicacion", "gobierno")) %>%
    full_join(single_bidder_percentage, by = c("mesanho_publicacion", "gobierno")) %>%
    full_join(average_bidder_count, by = c("mesanho_publicacion", "gobierno")) %>%
    full_join(single_bidder_metrics, by = c("mesanho_publicacion", "gobierno")) %>% 
    #    full_join(valor_proyectos_adjudicados_respecto_al_anual, by = c("mesanho_publicacion", "gobierno"))%>% 
    left_join(indices_proyectos_adjudicacion, by = c("mesanho_publicacion", "gobierno")) %>% 
    ungroup()

## fix columns for postores. If at least 1 adjudicado then at least 1 postor
indices_df <- indices_df %>%
    # Coalesce .x and .y columns
    mutate( 
        total_projects = max(total_projects, n_projects, na.rm = TRUE),
        total_bidders = coalesce(total_bidders, n_ganadores)
    )


# save as parquet
write_parquet(indices_df, "data/02_intermediate/OSCE/monthly_indices_2017_overall.parquet")
write.csv(indices_df, "data/02_intermediate/OSCE/monthly_indices_2017_overall.csv")

#### SEMESTRALES
## Concentration indices

# pre-steps
total_value <- combined_df_adjudicaciones %>%
    group_by(gobierno, semester) %>% 
    arrange(gobierno, semester) %>% 
    summarise(total_value = sum(monto_total, na.rm = TRUE)) 

combined_df_adjudicaciones <- combined_df_adjudicaciones %>% 
    left_join(total_value, by = c("gobierno", "semester"))

# calculation
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
    summarise(
        num_projects = sum(cum_monto <= total_value * 0.8) + 1,
        perc_projects = (sum(cum_monto <= total_value * 0.8) + 1) / n(),
        n = n(),
        n_ganadores = n_distinct(ruc_ganador),
        n_proyectos = n_distinct(nombre_proceso)) %>%
    ungroup()

## REPEATED BIDDERS
# Group by entity and 6-month time interval and get the unique postores
postores_per_entity <- combined_df_postores %>% 
    group_by(gobierno, semester) %>%
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
        postores_previous = dplyr::lag(postores),
        repeated_postores = map2_int(postores, postores_previous, get_overlap),
        total_postores = map_int(postores, length),
        perc_repeated_postores = repeated_postores / total_postores * 100
    ) %>%
    select(-postores_previous, -postores)

## REPEATED WINNERS
winners_per_entity <- combined_df_adjudicaciones %>% 
    group_by(gobierno, semester) %>%
    summarise(winners = list(unique(ruc_ganador))) %>%
    arrange(gobierno, semester)

# Define a function to get the overlap (repetition) of winners
get_overlap <- function(current, previous) {
    return(length(intersect(current, previous)))
}

# Calculate the repeated winners for each entity and 6-month interval
winners_overlap <- winners_per_entity %>%
    group_by(gobierno) %>%
    mutate(
        winners_previous = dplyr::lag(winners),
        repeated_winners = map2_int(winners, winners_previous, get_overlap),
        total_winners = map_int(winners, length),
        perc_repeated_winners = repeated_winners / total_winners * 100
    ) %>%
    select(-winners_previous, -winners)

#

# Merge
semestral_df <- total_value %>% 
    left_join(percent_coberture_at_80_index, by = c("gobierno", "semester")) %>%
    left_join(postores_overlap, by = c("gobierno", "semester")) %>%
    left_join(winners_overlap, by = c("gobierno", "semester"))

# fill
semestral_df <- semestral_df %>% 
    mutate(
        num_projects = coalesce(num_projects,  1),
        repeated_postores = replace_na(repeated_postores, 0),
        repeated_winners = replace_na(repeated_winners, 0),
        total_postores = coalesce(total_postores, total_winners, n_ganadores, 1)
    )

semestral_df %>% 
    mutate(
        tipo_municipalidad = case_when(str_detect(gobierno, "PROVINCIAL") ~ "provincial",
                                       str_detect(gobierno, "REGI") ~ "regional",
                                       str_detect(gobierno, "DIS") ~ "distrital",
                                       TRUE ~ "Otras entidades")
    ) %>% 
    group_by(semester, tipo_municipalidad) %>% 
    summarise(
        perc_repeated_winners  = mean(perc_repeated_winners, na.rm = TRUE),
    ) %>% 
    ggplot(aes(x = semester, y = perc_repeated_winners , color = tipo_municipalidad)) +
    geom_line(alpha = 1) +
    geom_vline(xintercept = as.POSIXct("2014-10-05"))


# write 
write_parquet(semestral_df, "data/02_intermediate/OSCE/semestral_indices_2017_overall.parquet")