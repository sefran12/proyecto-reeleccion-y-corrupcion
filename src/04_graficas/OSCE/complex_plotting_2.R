library(patchwork)

# Ununfiltered
regex_pattern <- "^(GOBIERNO REGIONAL (?:DE )?\\w+|MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DE )?[\\w\\s]+?(?= -|- |$))"

combined_df_postores_unfiltered <- combined_df_postores %>%
    mutate(gobierno = str_extract(ENTIDAD, regex_pattern),
           gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
           mesanho_publicacion = floor_date(fecha_publicacion, "month"))
# Calculate the number of bidders per project
bidder_count_unfiltered <- combined_df_postores_unfiltered %>%
    group_by(ruc_entidad, nombre_proceso, mesanho_publicacion, gobierno) %>%
    summarise(n_bidders = n()) 

# Create a column to indicate if a project has only one bidder
bidder_count_unfiltered <- bidder_count_unfiltered %>%
    mutate(single_bidder = n_bidders == 1)

# Calculate the percentage of projects with a single bidder
single_bidder_percentage_unfiltered <- bidder_count_unfiltered %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        total_projects = n(),
        single_bidder_projects = sum(single_bidder),
        perc_single_bidder = (single_bidder_projects / total_projects) * 100
    )

# Create the plot for "% of projects with a single bidder"
single_bidder_plot <- create_mean_plot(single_bidder_percentage_unfiltered, "mesanho_publicacion", "perc_single_bidder",
                                       "% de proyectos con un solo postor",
                                       "Año", "Porcentaje")
single_bidder_plot

# Average bidder plot

# Calculate the average number of bidders per project
average_bidder_count_unfiltered <- combined_df_postores_unfiltered %>%
    group_by(mesanho_publicacion, gobierno) %>%
    summarise(
        total_bidders = n(),
        total_projects = n_distinct(nombre_proceso),
        avg_bidders_per_project = total_bidders / total_projects
    )

# Create the plot for "Numero promedio de postores por proceso"
average_bidder_plot <- create_mean_plot(average_bidder_count_unfiltered, "mesanho_publicacion", "avg_bidders_per_project",
                                        "Número promedio de postores por proceso",
                                        "Año", "Promedio")
average_bidder_plot

## Using cross references for last metrics

# Identify single bidder projects
single_bidder_projects <- combined_df_postores_unfiltered %>%
    group_by(ruc_entidad, nombre_proceso) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    select(ruc_entidad, nombre_proceso)

# Join with the adjudicaciones dataset
single_bidder_adjudicaciones <- df_adjudicaciones %>%
    inner_join(single_bidder_projects,
               by = c("ruc_entidad", "nombre_proceso"))

# Calculate the difference and ratio
single_bidder_metrics_unfiltered <- single_bidder_adjudicaciones %>%
    mutate(diff_valor = valor_adjudicado_item - valor_referencial_item,
           ratio_valor = valor_adjudicado_item / valor_referencial_item - 1) %>%
    filter(ratio_valor < 2, # there is one anomalous month. Only this month gets unfiltered by this
           diff_valor < 100000 # there is one anomalous month. Only this month gets unfiltered by this
    ) %>% 
    group_by(mesanho_publicacion = floor_date(fecha_publicacion, "month"), gobierno) %>%
    summarise(
        avg_diff = mean(diff_valor, na.rm = TRUE),
        avg_ratio = mean(ratio_valor, na.rm = TRUE)
    )

# Create the plots
avg_diff_plot <- create_mean_plot(single_bidder_metrics_unfiltered, "mesanho_publicacion", "avg_diff",
                                  "Diferencia promedio entre el valor ofertado y el valor referencial de los proyectos con un solo postor",
                                  "Año", "Diferencia promedio")

avg_ratio_plot <- create_mean_plot(single_bidder_metrics_unfiltered, "mesanho_publicacion", "avg_ratio",
                                   "Diferencia porcentual promedio entre el valor ofertado y el valor referencial de los proyectos con un solo postor",
                                   "Año", "Ratio promedio")
avg_diff_plot
avg_ratio_plot

# Calculate Postores universe and postores intersection 6-month interval by 6 month-interval

# Break down the data into 6-month intervals
combined_df_postores_unfiltered$semestre_publicacion <- floor_date(combined_df_postores_unfiltered$fecha_publicacion, "6 months")

# Group by entity and 6-month time interval and get the unique postores
postores_per_entity_unfiltered <- combined_df_postores_unfiltered %>% 
    group_by(ruc_entidad, semestre_publicacion) %>%
    summarise(postores = list(unique(ruc_postor))) %>%
    arrange(ruc_entidad, semestre_publicacion)

# Define a function to get the overlap (repetition) of postores
get_overlap <- function(current, previous) {
    return(length(intersect(current, previous)))
}

# Calculate the repeated postores for each entity and 6-month interval
postores_overlap_unfiltered <- postores_per_entity_unfiltered %>%
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

####

# Create a function to summarize the dataframe
create_summary_df <- function(df, group_col, value_col) {
    summary_df <- df %>%
        group_by(!!sym(group_col)) %>%
        summarise(
            Mean_Value = mean(!!sym(value_col), na.rm = TRUE),
            Std_Error = sd(!!sym(value_col), na.rm = TRUE) / sqrt(n()),
            Lower_Bound = Mean_Value - (1.96 * Std_Error),
            Upper_Bound = Mean_Value + (1.96 * Std_Error)
        )
    return(summary_df)
}

# Apply the function to both filtered and unfiltered datasets
summary_df_filtered <- create_summary_df(single_bidder_percentage, "mesanho_publicacion", "perc_single_bidder")
summary_df_unfiltered <- create_summary_df(single_bidder_percentage_unfiltered, "mesanho_publicacion", "perc_single_bidder")

# Join the filtered and unfiltered datasets based on the group_col
summary_df_joined <- summary_df_filtered %>%
    left_join(summary_df_unfiltered, by = "mesanho_publicacion", suffix = c("_filtered", "_unfiltered"))

# Now subtract the mean trends
summary_df_joined <- summary_df_joined %>%
    mutate(
        diff_in_mean = Mean_Value_filtered - Mean_Value_unfiltered
    )

# Now you can plot the differences
diff_mean_plot <- summary_df_joined %>%
    ggplot(aes(x = !!sym("mesanho_publicacion"), y = diff_in_mean)) +
    geom_line(size = 1) +
    geom_vline(xintercept = as.Date("2015-01-01"), color = "red") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    labs(title = "Difference in Mean Trends",
         x = "Año",
         y = "Difference in Mean Value")

# Display the plot
print(diff_mean_plot)

# Calculate the cumulative difference
summary_df_joined <- summary_df_joined %>%
    mutate(
        cum_diff_in_mean = cumsum(diff_in_mean)
    )

# Now plot the cumulative differences
cum_diff_mean_plot <- summary_df_joined %>%
    ggplot(aes(x = !!sym("mesanho_publicacion"), y = cum_diff_in_mean)) +
    geom_line(size = 1) +
    geom_vline(xintercept = as.Date("2015-01-01"), color = "red") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    labs(title = "Cumulative Difference in Mean Trends",
         x = "Año",
         y = "Cumulative Difference in Mean Value")

# Display the plot
print(cum_diff_mean_plot)



###

library(patchwork)

create_summary_plot <- function(df, group_col, value_col, unfiltered_df, title, x_label, y_label) {
    
    # Generate summary statistics
    summary_df <- df %>%
        group_by(!!sym(group_col)) %>%
        summarise(
            Mean_Value = mean(!!sym(value_col), na.rm = TRUE),
            Std_Error = sd(!!sym(value_col), na.rm = TRUE) / sqrt(n()),
            Lower_Bound = Mean_Value - (1.96 * Std_Error),
            Upper_Bound = Mean_Value + (1.96 * Std_Error)
        )
    
    # Generate summary statistics for the unfiltered dataset
    summary_df_unfiltered <- unfiltered_df %>%
        group_by(!!sym(group_col)) %>%
        summarise(
            Mean_Value_unfiltered = mean(!!sym(value_col), na.rm = TRUE),
            Std_Error_unfiltered = sd(!!sym(value_col), na.rm = TRUE) / sqrt(n()),
            Lower_Bound_unfiltered = Mean_Value_unfiltered - (1.96 * Std_Error_unfiltered),
            Upper_Bound_unfiltered = Mean_Value_unfiltered + (1.96 * Std_Error_unfiltered)
        )
    
    # Join the two dataframes
    summary_df <- left_join(summary_df, summary_df_unfiltered, by = group_col)
    
    # Calculate the difference in means
    summary_df <- summary_df %>%
        mutate(
            diff_in_mean = Mean_Value - Mean_Value_unfiltered,
            cum_diff_in_mean = cumsum(diff_in_mean)
        )
    
    # Generate the "level" series plot for filtered and unfiltered data
    level_plot <- summary_df %>%
        ggplot() +
        geom_line(aes(x = !!sym(group_col), y = Mean_Value), color = "blue") +
        geom_line(aes(x = !!sym(group_col), y = Mean_Value_unfiltered), color = "red") +
        geom_vline(xintercept = as.POSIXct("2015-01-01"), color = "black") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        labs(title = paste0(title, " (Gobiernos locales y regionales: Blue, Todas las entidades: Red)"),
             x = x_label,
             y = "Mean Value")
    
    # Generate the difference in mean plot
    diff_plot <- summary_df %>%
        ggplot(aes(x = !!sym(group_col), y = diff_in_mean)) +
        geom_line(size = 1) +
        geom_vline(xintercept = as.POSIXct("2015-01-01"), color = "black") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        labs(title = "Difference in Mean Trends",
             x = x_label,
             y = "Difference in Mean Value")
    
    # Generate the cumulative difference in mean plot
    cum_diff_plot <- summary_df %>%
        ggplot(aes(x = !!sym(group_col), y = cum_diff_in_mean)) +
        geom_line(size = 1) +
        geom_vline(xintercept = as.POSIXct("2015-01-01"), color = "black") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        labs(title = "Cumulative Difference in Mean Trends",
             x = x_label,
             y = "Cumulative Difference in Mean Value")
    
    # Combine the plots using patchwork
    combined_plot <- level_plot / diff_plot / cum_diff_plot
    
    # Return the combined plot
    return(combined_plot)
}


# Example usage of the function
combined_plot <- create_summary_plot(single_bidder_percentage, "mesanho_publicacion", "perc_single_bidder", single_bidder_percentage_unfiltered,
                                     "% de proyectos con un solo postor",
                                     "Año", "Porcentaje")

# Display the combined plot
print(combined_plot)

# Example usage of the function
plots <- create_summary_plot(average_bidder_count, "mesanho_publicacion", "avg_bidders_per_project", average_bidder_count_unfiltered,
                             "Número promedio de postores por proceso",
                             "Año", "Diferencia acumulada entre gobiernos locales y el resto del Perú")

# Display the plots
print(plots)

# Example usage of the function
plots <- create_summary_plot(postores_overlap, "semestre_publicacion", "perc_repeated_postores", postores_overlap_unfiltered,
                             "% de postores repetidos semestre a semestre",
                             "Año", "Diferencia % acumulada entre gobiernos locales y el resto del Perú")

# Display the plots
print(plots)

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

#

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
"% del valor adjudicado total correspondiente a los 3 proyectos de mayor valor",
"Año", "Porcentaje")

