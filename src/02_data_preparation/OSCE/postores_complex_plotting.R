library(lubridate)
library(tidyverse)
library(skimr)

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
    group_by(ruc_entidad, semestre_publicacion) %>%
    summarise(postores = list(unique(ruc_postor))) %>%
    arrange(ruc_entidad, semestre_publicacion)

# Define a function to get the overlap (repetition) of postores
get_overlap <- function(current, previous) {
    return(length(intersect(current, previous)))
}

# Calculate the repeated postores for each entity and 6-month interval
postores_overlap <- postores_per_entity %>%
    group_by(ruc_entidad) %>%
    mutate(
        postores_previous = lag(postores),
        repeated_postores = map2_int(postores, postores_previous, get_overlap),
        total_postores = map_int(postores, length),
        perc_repeated_postores = repeated_postores / total_postores * 100
    ) %>%
    select(-postores_previous, -postores)

# Preview the postores_overlap dataframe
head(postores_overlap)

avg_postor_overlap_plot <- create_mean_plot(postores_overlap, "semestre_publicacion", "perc_repeated_postores",
                                   "% promedio de postores únicos repetidos del semestre anterior",
                                   "Semestre", "% de repetición")
avg_postor_overlap_plot + 
    geom_vline(xintercept = as.POSIXct("2015-01-01"), color = "red")
