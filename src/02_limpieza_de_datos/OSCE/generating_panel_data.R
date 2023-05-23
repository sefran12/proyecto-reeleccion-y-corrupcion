library(tidyverse)
library(lubridate)
library(skimr)

regex_pattern <- "^(GOBIERNO REGIONAL (?:DE )?\\w+|MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DE )?[\\w\\s]+?(?= -|- |$))"

# Pre-processing combined_df_postores and df_adjudicaciones
combined_df_postores_filtered <- combined_df_postores %>%
    filter(str_detect(ENTIDAD, "MUNICIPALIDAD|GOBIERNO REGIONAL"),
           fecha_publicacion > "2010-01-01") %>%
    mutate(gobierno = str_extract(ENTIDAD, regex_pattern),
           gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
           mesanho_publicacion = floor_date(fecha_publicacion, "month"))

df_adjudicaciones_processed <- df_adjudicaciones %>%
    filter(str_detect(ENTIDAD, "MUNICIPALIDAD|GOBIERNO REGIONAL"),
           fecha_publicacion > "2010-01-01") %>%
    mutate(gobierno = str_extract(ENTIDAD, regex_pattern),
           gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
           mesanho_publicacion = floor_date(fecha_publicacion, "month"))

# Calculate indices
numero_ganadores <- df_adjudicaciones_processed %>% 
    group_by(mesanho_publicacion, gobierno) %>% 
    summarise(
        n_ganadores = n_unique(ruc_ganador)
    )

valor_proyectos_adjudicados <- df_adjudicaciones_processed %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        valor_adjudicado_mean = mean(valor_adjudicado_item, na.rm = TRUE)
    )

tiempo_promedio_publicacion_buenapro <- df_adjudicaciones_processed %>%
    mutate(time_diff_days = as.numeric(difftime(fecha_buenapro, fecha_publicacion, units = "days"))) %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        tiempo_promedio = mean(time_diff_days, na.rm = TRUE)
    )

bidder_count <- combined_df_postores_filtered %>%
    group_by(ruc_entidad, nombre_proceso, mesanho_publicacion, gobierno) %>%
    summarise(n_bidders = n()) %>%
    mutate(single_bidder = n_bidders == 1)

single_bidder_percentage <- bidder_count %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        n_total_projects = n(),
        single_bidder_projects = sum(single_bidder),
        perc_single_bidder = (single_bidder_projects / n_total_projects) * 100
    )


average_bidder_count <- combined_df_postores_filtered %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        total_bidders = n(),
        n_total_projects = n_distinct(nombre_proceso),
        avg_bidders_per_project = total_bidders / n_total_projects
    )

single_bidder_projects <- combined_df_postores_filtered %>%
    group_by(ruc_entidad, nombre_proceso) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    select(ruc_entidad, nombre_proceso)

single_bidder_adjudicaciones <- df_adjudicaciones_processed %>%
    inner_join(single_bidder_projects,
               by = c("ruc_entidad", "nombre_proceso"))

single_bidder_metrics <- single_bidder_adjudicaciones %>%
    mutate(diff_valor = valor_adjudicado_item - valor_referencial_item,
           ratio_valor = valor_adjudicado_item / valor_referencial_item - 1) %>%
    filter(ratio_valor < 2, diff_valor < 100000) %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        avg_diff = mean(diff_valor, na.rm = TRUE),
        avg_ratio = mean(ratio_valor, na.rm = TRUE)
    )

# Consolidate to a big table
panel_time_series <- single_bidder_percentage %>%
    full_join(average_bidder_count, by = c("mesanho_publicacion", "gobierno")) %>%
    full_join(single_bidder_metrics, by = c("mesanho_publicacion", "gobierno")) %>%
    full_join(numero_ganadores, by = c("mesanho_publicacion", "gobierno")) %>%
    full_join(valor_proyectos_adjudicados, by = c("mesanho_publicacion", "gobierno")) %>%
    full_join(tiempo_promedio_publicacion_buenapro, by = c("mesanho_publicacion", "gobierno")) %>%
    mutate(
        n_total_projects = coalesce(n_total_projects.x, n_total_projects.y)
    ) %>%
    select(
        mesanho_publicacion,
        gobierno,
        n_total_projects,
        single_bidder_projects,
        perc_single_bidder,
        total_bidders,
        avg_bidders_per_project,
        avg_diff,
        avg_ratio,
        n_ganadores,
        valor_adjudicado_mean,
        tiempo_promedio
    )

# Balancing the panel data

# Create all possible combinations of mesanho_publicacion and gobierno
all_dates <- seq(min(panel_time_series$mesanho_publicacion),
                 max(panel_time_series$mesanho_publicacion),
                 by = "month")

all_gobiernos <- unique(panel_time_series$gobierno)

all_combinations <- expand_grid(mesanho_publicacion = all_dates,
                                gobierno = all_gobiernos)

# Merge panel_time_series with all combinations to create a balanced dataset
balanced_panel_time_series <- all_combinations %>%
    full_join(panel_time_series, by = c("mesanho_publicacion", "gobierno"))


#Save the panel_time_series table to a CSV file

write.csv(panel_time_series, "panel_time_series.csv", row.names = FALSE)