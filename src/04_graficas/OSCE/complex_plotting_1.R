library(lubridate)
library(tidyverse)
library(skimr)

### ADJUDICACIONES

# Desestimando entidades ejecutoras y colapsando al gobierno

regex_pattern <- "^(GOBIERNO REGIONAL (?:DE )?\\w+|MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DE )?[\\w\\s]+?(?= -|- |$))"

df_adjudicaciones <- combined_df_adjudicaciones %>% 
    filter(str_detect(ENTIDAD, "MUNICIPALIDAD|GOBIERNO REGIONAL"),
           fecha_publicacion > "2010-01-01") %>% 
    mutate(gobierno = str_extract(ENTIDAD, regex_pattern),
           gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
           mesanho_publicacion = floor_date(fecha_publicacion, "month"))


## NUMERO DE GANADORES DE PROYECTOS EN MUNICIPALIDADES:

numero_ganadores <- df_adjudicaciones %>% 
    group_by(mesanho_publicacion, gobierno) %>% 
    summarise(
        n_ganadores = n_unique(ruc_ganador)
    )

## Plotting function

create_mean_plot <- function(df, group_col, value_col, title, x_label, y_label, break_year = "2014-01-01") {
    # Usage:
    #plot1 <- create_mean_plot(muni_df, "month_yearfecha_publicacion", "porc_procesos_unico_postor",
    #                          "% de procesos en municipalidades y gobiernos regionales con único postor",
    #                          "Año", "Porcentaje")
    
    summary_df <- df %>%
        group_by(!!sym(group_col)) %>%
        summarise(
            Mean_Value = mean(!!sym(value_col), na.rm = TRUE),
            Std_Error = sd(!!sym(value_col), na.rm = TRUE) / sqrt(n()),
            Lower_Bound = Mean_Value - (1.96 * Std_Error),
            Upper_Bound = Mean_Value + (1.96 * Std_Error)
        )
    
    plot <- 
        summary_df %>% 
        ggplot(aes(x = !!sym(group_col), y = Mean_Value)) +
        geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
        geom_point() +
        geom_line(size = 1) +
        geom_vline(xintercept = as.Date(break_year), color = "red") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        labs(title = title,
             x = x_label,
             y = y_label)
    
    return(plot)
}

create_mean_plot(numero_ganadores, "mesanho_publicacion", "n_ganadores",
                 "Número promedio de ganadores de adjudicaciones de proyectos",
                 "Año", "Promedio")

# Valor medio de los proyectos adjudicados (TODOS)

valor_proyectos_adjudicados <- df_adjudicaciones %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        valor_adjudicado_mean = mean(valor_adjudicado_item, na.rm = TRUE)
    )

# Plot the "Valor promedio de los proyectos adjudicados"
plot_valor_adjudicado <- create_mean_plot(valor_proyectos_adjudicados, "mesanho_publicacion", "valor_adjudicado_mean",
                                          "Valor promedio del item adjudicado",
                                          "Año", "Promedio")

plot_valor_adjudicado

# Valor promedio de los proyectos adjudicados (valor del item menor a S/. 1,000,000)
valor_proyectos_adjudicados <- df_adjudicaciones %>%
    filter(valor_adjudicado_item < 1000000) %>% 
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        valor_adjudicado_mean = mean(valor_adjudicado_item, na.rm = TRUE)
    )

# Plot the "Valor promedio de los proyectos adjudicados"
plot_valor_adjudicado <- create_mean_plot(valor_proyectos_adjudicados, "mesanho_publicacion", "valor_adjudicado_mean",
                                          "Valor promedio del item adjudicado (items menores a S/.1,000,000",
                                          "Año", "Promedio")

plot_valor_adjudicado

# Tiempo promedio entre fecha de publicacion y fecha de buena pro
tiempo_promedio_publicacion_buenapro <- df_adjudicaciones %>%
    mutate(time_diff_days = as.numeric(difftime(fecha_buenapro, fecha_publicacion, units = "days"))) %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        tiempo_promedio = mean(time_diff_days, na.rm = TRUE)
    )

# Plot the "Tiempo promedio entre fecha de publicacion y fecha de buena pro"
plot_tiempo_promedio <- create_mean_plot(tiempo_promedio_publicacion_buenapro, "mesanho_publicacion", "tiempo_promedio",
                                         "Tiempo promedio entre fecha de publicación y fecha de buena pro",
                                         "Año", "Días promedio")

plot_tiempo_promedio

### POSTORES

create_mean_plot <- function(df, group_col, value_col, title, x_label, y_label, break_year = "2015-01-01") {
    # Usage:
    #plot1 <- create_mean_plot(muni_df, "month_yearfecha_publicacion", "porc_procesos_unico_postor",
    #                          "% de procesos en municipalidades y gobiernos regionales con único postor",
    #                          "Año", "Porcentaje")
    
    summary_df <- df %>%
        group_by(!!sym(group_col)) %>%
        summarise(
            Mean_Value = mean(!!sym(value_col), na.rm = TRUE),
            Std_Error = sd(!!sym(value_col), na.rm = TRUE) / sqrt(n()),
            Lower_Bound = Mean_Value - (1.96 * Std_Error),
            Upper_Bound = Mean_Value + (1.96 * Std_Error)
        )
    
    plot <- 
        summary_df %>% 
        ggplot(aes(x = !!sym(group_col), y = Mean_Value)) +
        geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
        geom_point() +
        geom_line(size = 1) +
        geom_vline(xintercept = as.Date(break_year), color = "red") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        labs(title = title,
             x = x_label,
             y = y_label)
    
    return(plot)
}

regex_pattern <- "^(GOBIERNO REGIONAL (?:DE )?\\w+|MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DE )?[\\w\\s]+?(?= -|- |$))"

# Filtering relevant rows and creating a new column 'gobierno'
combined_df_postores_filtered <- combined_df_postores %>%
    filter(str_detect(ENTIDAD, "MUNICIPALIDAD|GOBIERNO REGIONAL"),
           fecha_publicacion > "2010-01-01") %>%
    mutate(gobierno = str_extract(ENTIDAD, regex_pattern),
           gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
           mesanho_publicacion = floor_date(fecha_publicacion, "month"))

# Calculate the number of bidders per project
bidder_count <- combined_df_postores_filtered %>%
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

# Create the plot for "% of projects with a single bidder"
single_bidder_plot <- create_mean_plot(single_bidder_percentage, "mesanho_publicacion", "perc_single_bidder",
                                       "% de proyectos con un solo postor",
                                       "Año", "Porcentaje")
single_bidder_plot

# Average bidder plot

# Calculate the average number of bidders per project
average_bidder_count <- combined_df_postores_filtered %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        total_bidders = n(),
        total_projects = n_distinct(nombre_proceso),
        avg_bidders_per_project = total_bidders / total_projects
    )

# Create the plot for "Numero promedio de postores por proceso"
average_bidder_plot <- create_mean_plot(average_bidder_count, "mesanho_publicacion", "avg_bidders_per_project",
                                        "Número promedio de postores por proceso",
                                        "Año", "Promedio")
average_bidder_plot

## Using cross references for last metrics

# Identify single bidder projects
single_bidder_projects <- combined_df_postores_filtered %>%
    group_by(ruc_entidad, nombre_proceso) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    select(ruc_entidad, nombre_proceso)

# Join with the adjudicaciones dataset
single_bidder_adjudicaciones <- df_adjudicaciones %>%
    inner_join(single_bidder_projects,
               by = c("ruc_entidad", "nombre_proceso"))

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

# Create the plots
avg_diff_plot <- create_mean_plot(single_bidder_metrics, "mesanho_publicacion", "avg_diff",
                                  "Diferencia promedio entre el valor ofertado y el valor referencial de los proyectos con un solo postor",
                                  "Año", "Diferencia promedio")

avg_ratio_plot <- create_mean_plot(single_bidder_metrics, "mesanho_publicacion", "avg_ratio",
                                   "Diferencia porcentual promedio entre el valor ofertado y el valor referencial de los proyectos con un solo postor",
                                   "Año", "Ratio promedio")
avg_diff_plot
avg_ratio_plot

# Calculate Postores universe and postores intersection 6-month interval by 6 month-interval

# Break down the data into 6-month intervals
combined_df_postores_filtered$semestre_publicacion <- floor_date(combined_df_postores_filtered$fecha_publicacion, "6 months")

# Group by entity and 6-month time interval and get the unique postores
postores_per_entity <- combined_df_postores_filtered %>% 
    group_by(ENTIDAD, semester = semestre_publicacion) %>%
    summarise(postores = list(unique(ruc_postor))) %>%
    arrange(ENTIDAD, semester)

# Define a function to get the overlap (repetition) of postores
get_overlap <- function(current, previous) {
    return(length(intersect(current, previous)))
}

# Calculate the repeated postores for each entity and 6-month interval
postores_overlap <- postores_per_entity %>%
    group_by(ENTIDAD) %>%
    mutate(
        postores_previous = lag(postores),
        repeated_postores = map2_int(postores, postores_previous, get_overlap),
        total_postores = map_int(postores, length),
        perc_repeated_postores = repeated_postores / total_postores * 100
    ) %>%
    select(-postores_previous, -postores)

# Preview the postores_overlap dataframe
head(postores_overlap)

avg_postor_overlap_plot <- create_mean_plot(postores_overlap, "semester", "perc_repeated_postores",
                                   "% promedio de postores únicos repetidos del semestre anterior",
                                   "Semestre", "% de repetición")
avg_postor_overlap_plot + 
    geom_vline(xintercept = as.POSIXct("2015-01-01"), color = "red")

## Concentration indices

# Concentration Index
combined_df_adjudicaciones <- combined_df_adjudicaciones %>% 
    mutate(
        semester = floor_date(fecha_publicacion, unit = "6 month"),
        monto_total = valor_adjudicado_item * cantidad_adjudicada
    ) 

total_value <- combined_df_adjudicaciones %>% 
    group_by(ENTIDAD, semester) %>% 
    arrange(ENTIDAD, semester) %>% 
    summarise(total_value = sum(valor_adjudicado_item * cantidad_adjudicada)) 

combined_df_adjudicaciones <- combined_df_adjudicaciones %>% 
    left_join(total_value, by = c("ENTIDAD", "semester"))

concentration_index_at_5 <- combined_df_adjudicaciones %>% 
    group_by(ENTIDAD, semester) %>% 
    slice_max(monto_total, n = 5) %>% 
    group_by(ENTIDAD, semester) %>% 
    summarise(proportion = sum(monto_total, na.rm = TRUE)/first(total_value))


concentration_index_at_3 <- combined_df_adjudicaciones %>% 
    group_by(ENTIDAD, semester) %>% 
    slice_max(monto_total, n = 3) %>% 
    group_by(ENTIDAD, semester) %>% 
    summarise(proportion = sum(monto_total, na.rm = TRUE)/first(total_value))


concentration_index_at_10 <- combined_df_adjudicaciones %>% 
    group_by(ENTIDAD, semester) %>% 
    slice_max(monto_total, n = 10) %>% 
    group_by(ENTIDAD, semester) %>% 
    summarise(proportion = sum(monto_total, na.rm = TRUE)/first(total_value))

# Don't forget to update the coverage function as well
coberture_at_80_index <- combined_df_adjudicaciones %>%
    group_by(ENTIDAD, semester) %>%
    arrange(ENTIDAD, semester, desc(monto_total)) %>% 
    mutate(cum_monto = cumsum(monto_total)) %>%
    summarise(num_projects = sum(cum_monto <= total_value * 0.8) + 1) %>%
    ungroup()

percent_coberture_at_80_index <- combined_df_adjudicaciones %>%
    group_by(ENTIDAD, semester) %>%
    arrange(ENTIDAD, semester, desc(monto_total)) %>% 
    mutate(cum_monto = cumsum(monto_total)) %>%
    summarise(perc_projects = (sum(cum_monto <= total_value * 0.8) + 1) / n(),
              n = n()) %>%
    ungroup()

create_mean_plot(percent_coberture_at_80_index, "semester", "perc_projects",
                 "% de proyectos necesarios para cubrir 80% del valor adjudicado total",
                 "Año", "Porcentaje")

create_mean_plot(concentration_index_at_3, "semester", "proportion",
                 "% del valor adjudicado total correspondiente a los 3 proyectos de mayor valor",
                 "Año", "Porcentaje")

create_mean_plot(concentration_index_at_5, "semester", "proportion",
                 "% del valor adjudicado total correspondiente a los 5 proyectos de mayor valor",
                 "Año", "Porcentaje")

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
    left_join(concentration_index_at_3, by = c("ENTIDAD", "semester")) %>%
    left_join(concentration_index_at_5, by = c("ENTIDAD", "semester")) %>%
    left_join(concentration_index_at_10, by = c("ENTIDAD", "semester"))

# Rename columns in coverage index dataframes
coberture_at_80_index <- coberture_at_80_index %>% 
    rename(coberture_at_80 = num_projects)

percent_coberture_at_80_index <- percent_coberture_at_80_index %>% 
    rename(perc_coberture_at_80 = perc_projects)

# Join coverage indices with semestral_df
semestral_df <- semestral_df %>%
    left_join(coberture_at_80_index, by = c("ENTIDAD", "semester")) %>%
    left_join(percent_coberture_at_80_index, by = c("ENTIDAD", "semester"))

# save semestral data
write_parquet(semestral_df, "data/02_intermediate/OSCE/semestral_indices.parquet")
