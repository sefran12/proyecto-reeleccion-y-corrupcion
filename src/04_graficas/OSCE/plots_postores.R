### POSTORES
library(tidyverse)
library(stringi)
library(lubridate)

regex_pattern <- "^(GOBIERNO REGIONAL (?:DE )?\\w+|MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DE )?[\\w\\s]+?(?= -|- |$))"

postores_por_proceso <- combined_df_postores %>% 
    filter(str_detect(entidad, "MUNICIPALIDAD|GOBIERNO REGIONAL")) %>% 
    mutate(gobierno = str_extract(entidad, regex_pattern),
           gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
           month_yearfecha_publicacion = floor_date(FECHA_PUBLICACION, "month")) %>% 
    group_by(month_yearfecha_publicacion, gobierno, proceso) %>% 
    summarise(
        n_postores = n_distinct(ruc_postor)
    )

# Postores por entidad
postores_por_entidad <- postores_por_proceso %>% 
    group_by(month_yearfecha_publicacion, gobierno) %>% 
    summarise(
        porc_procesos_unico_postor = sum(n_postores == 1)/n_distinct(proceso),
        numero_procesos_unico_postor = sum(n_postores == 1),
        total_procesos = n_distinct(proceso),
        media_postores_proceso = mean(n_postores, na.rm = TRUE),
    )

muni_df <- postores_por_entidad

muni_summary <- muni_df %>%
    group_by(month_yearfecha_publicacion) %>%
    summarise(
        Mean_Single_Bidder_Percentage = mean(porc_procesos_unico_postor, na.rm = TRUE),
        Std_Error = sd(porc_procesos_unico_postor, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Single_Bidder_Percentage - (1.96 * Std_Error), 0),
        Upper_Bound = pmin(Mean_Single_Bidder_Percentage + (1.96 * Std_Error), 1)
    )


plot1 <- 
    muni_summary %>% 
    ggplot(aes(x = month_yearfecha_publicacion, y = Mean_Single_Bidder_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    labs(title = "% de procesos en municipalidades y gobiernos regionales con único postor",
         x = "Año",
         y = "Porcentaje")

plot1

## Numero de procesos un solo postor

muni_df <- postores_por_entidad

muni_summary <- muni_df %>%
    group_by(month_yearfecha_publicacion) %>%
    summarise(
        Mean_Single_Bidder_Percentage = mean(numero_procesos_unico_postor, na.rm = TRUE),
        Std_Error = sd(numero_procesos_unico_postor, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Single_Bidder_Percentage - (1.96 * Std_Error), 0),
        Upper_Bound = Mean_Single_Bidder_Percentage + (1.96 * Std_Error)
    )


plot1 <- 
    muni_summary %>% 
    ggplot(aes(x = month_yearfecha_publicacion, y = Mean_Single_Bidder_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
    theme_minimal() +
    labs(title = "Número promedio de procesos en municipalidades y gobiernos regionales con único postor",
         x = "Año",
         y = "Procesos")

plot1

## Total de procesos


muni_df <- postores_por_entidad

muni_summary <- muni_df %>%
    group_by(month_yearfecha_publicacion) %>%
    summarise(
        Mean_Single_Bidder_Percentage = mean(total_procesos, na.rm = TRUE),
        Std_Error = sd(total_procesos, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Single_Bidder_Percentage - (1.96 * Std_Error), 0),
        Upper_Bound = Mean_Single_Bidder_Percentage + (1.96 * Std_Error)
    )


plot1 <- 
    muni_summary %>% 
    ggplot(aes(x = month_yearfecha_publicacion, y = Mean_Single_Bidder_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
    theme_minimal() +
    labs(title = "Total de procesos en municipalidades y gobiernos regionales",
         x = "Año",
         y = "Procesos")

plot1

### % Concursos públicos

adjudicaciones_directas <- combined_df_postores %>% 
    filter(str_detect(entidad, "MUNICIPALIDAD|GOBIERNO REGIONAL")) %>% 
    mutate(gobierno = str_extract(entidad, regex_pattern),
           gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
           TIPO_PROCESO = str_to_lower(TIPO_PROCESO),
           TIPO_PROCESO = stri_trans_general(TIPO_PROCESO, "Latin-ASCII"),
           TIPO_PROCESO = str_squish(TIPO_PROCESO),
           month_yearfecha_publicacion = floor_date(FECHA_PUBLICACION, "month")
           ) %>% 
    distinct(month_yearfecha_publicacion, gobierno, proceso, TIPO_PROCESO) 

muni_df <- adjudicaciones_directas %>% 
    group_by(month_yearfecha_publicacion, gobierno) %>% 
    summarise(
        total_adjudicacion_directa = sum(str_detect(TIPO_PROCESO, "licitacion publica")), #"adjudicacion|directa|menor cuantia")),
        total_procesos = n_distinct(proceso),
        perc_adjudicaciones_directas = total_adjudicacion_directa/total_procesos
    )


muni_summary <- muni_df %>%
    group_by(month_yearfecha_publicacion) %>%
    summarise(
        Mean_Adjudicacion_Directa_Percentage = mean(perc_adjudicaciones_directas, na.rm = TRUE),
        Std_Error = sd(perc_adjudicaciones_directas, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Adjudicacion_Directa_Percentage - (1.96 * Std_Error), 0),
        Upper_Bound = Mean_Adjudicacion_Directa_Percentage + (1.96 * Std_Error)
    )


plot1 <- 
    muni_summary %>% 
    ggplot(aes(x = month_yearfecha_publicacion, y = Mean_Adjudicacion_Directa_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    labs(title = "% de procesos de la entidad que son de tipo licitación pública",
         x = "Año",
         y = "% del total de procesos")

plot1

muni_df %>% 
    mutate(cutoff = month_yearfecha_publicacion >= as_datetime("2015-01-01")) %>% 
    lm(data = .,
        perc_adjudicaciones_directas ~ cutoff
        ) %>% 
    summary()

### % adjudicacion simplificada

adjudicaciones_directas <- combined_df_postores %>% 
    filter(str_detect(entidad, "MUNICIPALIDAD|GOBIERNO REGIONAL")) %>% 
    mutate(gobierno = str_extract(entidad, regex_pattern),
           gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
           TIPO_PROCESO = str_to_lower(TIPO_PROCESO),
           TIPO_PROCESO = stri_trans_general(TIPO_PROCESO, "Latin-ASCII"),
           TIPO_PROCESO = str_squish(TIPO_PROCESO),
           month_yearfecha_publicacion = floor_date(FECHA_PUBLICACION, "month")
    ) %>% 
    distinct(month_yearfecha_publicacion, gobierno, proceso, TIPO_PROCESO) 

muni_df <- adjudicaciones_directas %>% 
    group_by(month_yearfecha_publicacion, gobierno) %>% 
    summarise(
        total_adjudicacion_directa = sum(str_detect(TIPO_PROCESO, "adjudicacion directa|contratacion directa")),
        total_procesos = n_distinct(proceso),
        perc_adjudicaciones_directas = total_adjudicacion_directa/total_procesos
    )


muni_summary <- muni_df %>%
    group_by(month_yearfecha_publicacion) %>%
    summarise(
        Mean_Adjudicacion_Directa_Percentage = mean(perc_adjudicaciones_directas, na.rm = TRUE),
        Std_Error = sd(perc_adjudicaciones_directas, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Adjudicacion_Directa_Percentage - (1.96 * Std_Error), 0),
        Upper_Bound = Mean_Adjudicacion_Directa_Percentage + (1.96 * Std_Error)
    )


plot1 <- 
    muni_summary %>% 
    ggplot(aes(x = month_yearfecha_publicacion, y = Mean_Adjudicacion_Directa_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = 2015, color = "red") +
    theme_minimal() +
    labs(title = "% de procesos de tipo adjudicacion directa",
         x = "Year",
         y = "% del total de procesos")

plot1

muni_df %>% 
    mutate(cutoff = year >= 2015) %>% 
    lm(data = .,
       perc_adjudicaciones_directas ~ cutoff
    ) %>% 
    summary()

### Valor de los proyectos de un solo postor

postores_por_proceso <- combined_df_postores %>% 
    filter(str_detect(entidad, "MUNICIPALIDAD|GOBIERNO REGIONAL")) %>% 
    mutate(gobierno = str_extract(entidad, regex_pattern),
           gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
           month_yearfecha_publicacion = floor_date(FECHA_PUBLICACION, "month")) %>% 
    group_by(month_yearfecha_publicacion, gobierno, proceso) %>% 
    summarise(
        n_postores = n_distinct(ruc_postor),
        valor_proyecto = sum(unique(valor_referencial))
    )

# Postores por entidad
postores_por_entidad <- postores_por_proceso %>% 
    group_by(month_yearfecha_publicacion, gobierno) %>% 
    summarise(
        porc_procesos_unico_postor = sum(n_postores == 1)/n_distinct(proceso),
        numero_procesos_unico_postor = sum(n_postores == 1),
        total_procesos = n_distinct(proceso),
        media_postores_proceso = mean(n_postores, na.rm = TRUE),
        valor_total_procesos_un_postor = sum(valor_proyecto * (n_postores == 1)),
        perc_valot_total_procesos_un_postor = valor_total_procesos_un_postor/sum(valor_proyecto)
    )

muni_df <- postores_por_entidad

muni_summary <- muni_df %>%
    group_by(month_yearfecha_publicacion) %>%
    summarise(
        Mean_Single_Bidder_Percentage = mean(valor_total_procesos_un_postor, na.rm = TRUE),
        Std_Error = sd(valor_total_procesos_un_postor, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Single_Bidder_Percentage - (1.96 * Std_Error), 0),
        Upper_Bound = Mean_Single_Bidder_Percentage + (1.96 * Std_Error)
    )


plot1 <- 
    muni_summary %>% 
    ggplot(aes(x = month_yearfecha_publicacion, y = Mean_Single_Bidder_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
    scale_y_continuous(labels = scales::label_dollar(prefix = "S/."), limits = c(0, 5000000)) +
    theme_minimal() +
    labs(title = "Valor total promedio en Municipalidades y Gobiernos Regionales de procesos con único postor",
         x = "Year",
         y = "Soles (S/.)")

plot1

muni_df %>% 
    mutate(cutoff = month_yearfecha_publicacion > as_datetime("2015-01-01")) %>% 
    lm(data = .,
       valor_total_procesos_un_postor ~ cutoff) %>% 
    summary()
