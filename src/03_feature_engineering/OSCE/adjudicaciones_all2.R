## ADJUDICACIONES ANALISIS
library(lubridate)

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

