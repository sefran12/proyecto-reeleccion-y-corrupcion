library(tidyverse)
library(stringi)
library(lubridate)

regex_pattern <- "^(GOBIERNO REGIONAL (?:DE )?\\w+|MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DE )?[\\w\\s]+?(?= -|- |$))"

# Function to summarize data
summarize_data <- function(df, column_name) {
    df %>%
        group_by(month_yearfecha_publicacion) %>%
        summarise(
            Mean_Value = mean(!!sym(column_name), na.rm = TRUE),
            Std_Error = sd(!!sym(column_name), na.rm = TRUE) / sqrt(n()),
            Lower_Bound = pmax(Mean_Value - (1.96 * Std_Error), 0),
            Upper_Bound = Mean_Value + (1.96 * Std_Error)
        )
}

# Preprocessing
preprocessed_data <- combined_df_postores %>%
    filter(str_detect(entidad, "MUNICIPALIDAD|GOBIERNO REGIONAL")) %>%
    mutate(
        gobierno = str_extract(entidad, regex_pattern),
        gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
        month_yearfecha_publicacion = floor_date(FECHA_PUBLICACION, "month"),
        TIPO_PROCESO = str_to_lower(TIPO_PROCESO) %>%
            stri_trans_general("Latin-ASCII") %>%
            str_squish()
    ) %>%
    distinct(month_yearfecha_publicacion, gobierno, proceso, TIPO_PROCESO, ruc_postor, valor_referencial)

# Analysis steps
postores_por_proceso <- preprocessed_data %>%
    group_by(month_yearfecha_publicacion, gobierno, proceso) %>%
    summarise(
        tipo_proceso = first(TIPO_PROCESO),
        n_postores = n_distinct(ruc_postor),
        valor_proyecto = sum(unique(valor_referencial))
    )

postores_por_entidad <- postores_por_proceso %>%
    group_by(month_yearfecha_publicacion, gobierno) %>%
    summarise(
        porc_procesos_unico_postor = sum(n_postores == 1) / n_distinct(proceso),
        numero_procesos_unico_postor = sum(n_postores == 1),
        total_procesos = n_distinct(proceso),
        media_postores_proceso = mean(n_postores, na.rm = TRUE),
        total_adjudicacion_licitacion_publica = sum(str_detect(tipo_proceso, "licitacion publica")),
        total_adjudicacion_directa = sum(str_detect(tipo_proceso, "adjudicacion directa|contratacion directa")),
        perc_adjudicaciones_licitacion_publica = total_adjudicacion_licitacion_publica / total_procesos,
        perc_adjudicaciones_directas = total_adjudicacion_directa / total_procesos,
        valor_total_procesos_un_postor = sum(valor_proyecto * (n_postores == 1))
    )

# Summarizing data
muni_summary_unico_postor <- summarize_data(postores_por_entidad, "porc_procesos_unico_postor")
muni_summary_adjudicacion_licitacion_publica <- summarize_data(postores_por_entidad, "perc_adjudicaciones_licitacion_publica")
muni_summary_adjudicacion_directa <- summarize_data(postores_por_entidad, "perc_adjudicaciones_directas")
muni_summary_valor_total_procesos_un_postor <- summarize_data(postores_por_entidad, "valor_total_procesos_un_postor")


#Plotting functions

plot_summary <- function(summary_data, y_var, title, y_lab) {
    ggplot(summary_data, aes(x = month_yearfecha_publicacion, y = !!sym(y_var))) +
        geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
        geom_point() +
        geom_line(size = 1) +
        geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
        theme_minimal() +
        labs(title = title,
             x = "Year",
             y = y_lab)
}

#Plotting

plot1 <- plot_summary(muni_summary_unico_postor, "Mean_Value", "% de procesos con único postor en Municipalidades y Gobiernos Regionales", "Mean day difference")
plot2 <- plot_summary(muni_summary_adjudicacion_licitacion_publica, "Mean_Value", "% de procesos de tipo concurso o licitación pública", "% del total de procesos")
plot3 <- plot_summary(muni_summary_adjudicacion_directa, "Mean_Value", "% de procesos de tipo adjudicación o contratación directa", "% del total de procesos")
plot4 <- plot_summary(muni_summary_valor_total_procesos_un_postor, "Mean_Value", "Valor total de procesos con único postor en Municipalidades y Gobiernos Regionales", "Soles (S/.)")

#Show plots

plot1
plot2
plot3
plot4

#Regression analysis

regression_analysis <- function(df, response_var, predictor_var) {
    df %>%
        mutate(predictor = !!sym(predictor_var)) %>%
        lm(data = .,
           !!sym(response_var) ~ predictor) %>%
        summary()
}

#Regressions

regression_1 <- regression_analysis(muni_df, "perc_adjudicaciones_directas", "cutoff")
regression_2 <- regression_analysis(muni_df, "perc_adjudicaciones_simplificadas", "cutoff")
regression_3 <- regression_analysis(muni_df, "valor_total_procesos_un_postor", "cutoff")

#Show regression summaries

regression_1
regression_2
regression_3




colnames(combined_df_adjudicaciones)
colnames(combined_df_contratos)
colnames(combined_df_postores)