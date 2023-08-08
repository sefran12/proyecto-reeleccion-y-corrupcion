# ANALISIS DESCRIPTIVO OSCE:
library(tidyverse)
library(arrow)
library(skimr)
library(clipr)
library(zoo)
library(fuzzyjoin)
library(lubridate)
library(broom)

theme_set(theme_bw())

osce_infogob_oci_monthly <- read_parquet("data/03_model/osce_infogob_oci_monthly_2017_overall.parquet") %>% ungroup()
osce_infogob_oci_semestral <- read_parquet("data/03_model/osce_infogob_oci_semester_2017_overall.parquet") %>% ungroup()
mef_infogob_oci_yearly <- read_parquet("data/03_model/mef_infogob_oci_anual.parquet") %>% ungroup()
mef_osce_matching <- read_parquet("data/02_intermediate/all_matches_mef_osce.parquet")

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
    select(
        gobierno,
        mesanho_publicacion = date,
        tipo_municipalidad = tipo_municipalidad.x,
        perc_proyectos_debajo_cutoff,
        perc_proyectos_sobrecosto
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
    left_join(osce_semester, by = c("mesanho_publicacion", "gobierno"))
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
    mutate(numero_efectivo_partidos = if_else(is.infinite(numero_efectivo_partidos), 1, numero_efectivo_partidos)) %>% 
    select(gobierno, mesanho_publicacion, tipo_municipalidad, avg_bidders_per_project, avg_ratio_between_winner_and_publicado,
           perc_single_bidder, perc_repeated_winners, perc_proyectos_debajo_cutoff, perc_proyectos_sobrecosto, perc_valor_adj_del_total) %>%
    group_by(gobierno) %>%
#    mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
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

library(dplyr)
library(ggplot2)

mean_index_plot <- combined_data_clean %>%
    group_by(mesanho_publicacion) %>%
    summarise(
        mean_index = mean(mean_indices, na.rm = TRUE),
        range_indices = max(mean_indices) - min(mean_indices),
        sd_indices = SD(mean_indices, na.rm = TRUE),
        n_indices = n(),
        se_mean = sd_indices / sqrt(n_indices),
        df = n_indices - 1
    ) %>%
    mutate(
        t_critical = qt(0.975, df), 
        lower_CI = mean_index - t_critical*se_mean,
        upper_CI = mean_index + t_critical*se_mean
    ) %>%
    ggplot(aes(x = mesanho_publicacion, y = mean_index)) +
    geom_line(alpha = 1) +
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.4, fill = "orange") +
    geom_smooth(method = "loess", se = FALSE, color = "black") +
    geom_smooth(data = . %>% filter(mesanho_publicacion <= as.Date("2015-03-01")),
                method = "loess", se = FALSE, color = "blue") +
    geom_smooth(data = . %>% filter(mesanho_publicacion > as.Date("2015-03-01")),
                method = "loess", se = FALSE, color = "red") +
    geom_vline(xintercept = as.Date("2015-03-01")) +
    labs(x = NULL,
         y = "Promedio de Indicadores Estandarizados",
         color = "Tipo de proyecto")

mean_index_plot

mean_index_plot2 <- combined_data_clean %>% 
    group_by(mesanho_publicacion, tipo_municipalidad) %>% 
    mutate(
        lower_bound = quantile(mean_indices, 0.01, na.rm = TRUE),
        upper_bound = quantile(mean_indices, 0.99, na.rm = TRUE)
    ) %>%
    filter(mean_indices >= lower_bound & mean_indices <= upper_bound) %>% 
    summarise(
        mean_index = mean(mean_indices, na.rm = TRUE),
        range_indices = max(mean_indices) - min(mean_indices),
        sd_indices = sd(mean_indices, na.rm = TRUE),
        n_indices = n(),
        se_mean = sd_indices / sqrt(n_indices),
        df = n_indices - 1
    ) %>%
    mutate(
        t_critical = qt(0.975, df), 
        lower_CI = mean_index - t_critical*se_mean,
        upper_CI = mean_index + t_critical*se_mean
    ) %>% 
    ggplot(aes(x = mesanho_publicacion, y = mean_index)) +
    geom_line(alpha = 1) +
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.4, fill = "orange") +
    geom_smooth(method = "loess", se = FALSE, color = "black") +
    geom_smooth(data = . %>% filter(mesanho_publicacion <= as.Date("2015-03-01")),
                method = "loess", se = FALSE, color = "blue") +
    geom_smooth(data = . %>% filter(mesanho_publicacion > as.Date("2015-03-01")),
                method = "loess", se = FALSE, color = "red") +
    geom_vline(xintercept = as.Date("2015-03-01")) +
    labs(x = NULL,
         y = "Promedio de Indicadores Estandarizados") +
    facet_wrap(~tipo_municipalidad, ncol = 1, scales = "free_y")


mean_index_plot2

combined_data_clean %>% 
    group_by(año = year(mesanho_publicacion), tipo_municipalidad) %>% 
    summarise(
        mean_index = mean(mean_indices, na.rm = TRUE),
        sd_index = sd(mean_indices, na.rm = TRUE)
    ) %>% 
    pivot_wider(names_from = OBJETO, values_from = c(mean_index, sd_index)) %>% 
    arrange(tipo_municipalidad, año) %>% 
    clipr::write_clip()

### Confirmatory FA
library(lavaan)
library(semTable)
library(clipr)
library(psych)
library(broom)

# Define the CFA model
# This syntax specifies that each of the indices is loaded on a single factor (F1)
cfa_model <- 'F1 =~ avg_bidders_per_project + perc_single_bidder + avg_ratio_between_winner_and_publicado + perc_valor_adj_del_total +
              perc_repeated_winners + perc_proyectos_debajo_cutoff + perc_proyectos_sobrecosto '

# Fit the model
fit_1_factor <- cfa(cfa_model, data = combined_data_clean)

# Summary of the model
summary(fit_1_factor, fit.measures = TRUE) 
fit_1_factor %>% semTable(type = "csv") %>% write_clip()

tidy(fit_1_factor) %>% write_clip()

# Psychometric tests
cronbach_alpha_1f = alpha(combined_data_clean[,c("avg_bidders_per_project", "perc_single_bidder", "avg_ratio_between_winner_and_publicado", "perc_valor_adj_del_total", "perc_repeated_winners", "perc_proyectos_debajo_cutoff", "perc_proyectos_sobrecosto")])
cronbach_alpha_1f

alpha_tibble <- tibble(
    total_items = cronbach_alpha_1f$total,
    alpha = cronbach_alpha_1f$alpha,
    standardised_alpha = cronbach_alpha_1f$std.alpha,
    average_interitem_correlation = cronbach_alpha_1f$av.r
) %>% 
    unnest(cols = c(total_items, alpha), names_sep = "_")

alpha_tibble %>% write_clip()
reliability(fit_1_factor)

## Segun origen de los datos
cfa_model <- '
  F1 =~ avg_bidders_per_project + perc_single_bidder + avg_ratio_between_winner_and_publicado + perc_valor_adj_del_total + perc_repeated_winners
  F2 =~ perc_proyectos_debajo_cutoff  + perc_proyectos_sobrecosto
'

# Fit the model
#fit_2_factors <- cfa(cfa_model, data = combined_data_clean)

# Summary of the model
summary(fit_2_factors, fit.measures = TRUE)
fit_2_factors %>% semTable(type = "csv") %>% write_clip()

## Segun agrupamiento del modelo unico
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

# Hierarchical FA
# and an overall factor (F) is indicated by F1 and F2
cfa_model <- '
  # Definicion de los factores de primer orden
  F1 =~ avg_bidders_per_project + perc_single_bidder + avg_ratio_between_winner_and_publicado + perc_valor_adj_del_total
  F2 =~ perc_proyectos_debajo_cutoff  + perc_proyectos_sobrecosto + perc_repeated_winners 
  # Definicion del factor de segundo orden
  L1 =~ F1 + F2
'

# Fit the model
fit_hierar_factors <- cfa(cfa_model, data = combined_data_clean)

# Summary of the model
summary(fit_hierar_factors, fit.measures = TRUE)

# DOES NOT WORK

## GRAFICOS

combined_data_clean %>% colnames()
combined_data_clean %>% 
    mutate(
        cutoff = mesanho_publicacion > "2015-03-01"
    ) %>% 
    lm(data = .,
       formula = mean_indices ~ cutoff*OBJETO*tipo_municipalidad*mesanho_publicacion) %>% 
    summary(vcov = sandwich::vcovHC)

combined_data_clean %>% 
    mutate(
        cutoff = mesanho_publicacion > "2015-03-01",
        mean_indices = (F1 + F2) / 2
    ) %>% 
    filter(tipo_municipalidad == "distrital") %>% 
    lm(data = . ,
       formula = mean_indices ~ cutoff*mesanho_publicacion + gobierno,
    ) %>% 
    broom::tidy() %>% View()


combined_data_clean %>% 
    group_by(mesanho_publicacion) %>% 
    summarise(
        mean_indices = mean((F1 + F2)/2, na.rm = TRUE)
    ) %>% 
    ggplot(aes(x = mesanho_publicacion, y = mean_indices)) +
    geom_line(alpha = 1) +
    geom_vline(xintercept = as.Date("2015-03-01")) +
    labs(x = NULL,
         y = "Índice de Riesgo de Corrupción",
         color = "Tipo de proyecto")

# Demeaned graph

# Crear la versión 'demeaned' de los índices
# Cargar el paquete tidyverse
library(tidyverse)

# Crear la versión 'demeaned' de los índices
combined_data_clean <- combined_data_clean %>%
    group_by(gobierno) %>%
    mutate(demeaned_indices = (F1 + F2)/2 - mean((F1 + F2)/2, na.rm = TRUE))

# Crear el gráfico
combined_data_clean %>% 
    filter(demeaned_indices < 3, demeaned_indices > -3) %>% 
    ggplot(aes(x = mesanho_publicacion, y = demeaned_indices, group = gobierno)) +
    geom_line(alpha = 0.01, size = 1) +
    geom_vline(xintercept = as.Date("2015-03-01")) +
    labs(x = NULL,
         y = "Índice de Riesgo de Corrupción (demeaned)",
         color = "Gobierno") +
    geom_smooth(aes(x = mesanho_publicacion, y = demeaned_indices), inherit.aes = FALSE)


combined_data_clean %>% 
    group_by(mesanho_publicacion, tipo_municipalidad) %>% 
    summarise(
        mean_indices = mean((F1 + F2)/2, na.rm = TRUE)
    ) %>% 
    ggplot(aes(x = mesanho_publicacion, y = mean_indices)) +
    geom_line(alpha = 1) +
    geom_smooth(method = "loess", se = FALSE, color = "black") +
    geom_smooth(data = . %>% filter(mesanho_publicacion <= as.Date("2015-03-01")),
                method = "loess", se = FALSE, color = "blue") +
    geom_smooth(data = . %>% filter(mesanho_publicacion > as.Date("2015-03-01")),
                method = "loess", se = FALSE, color = "red") +
    geom_vline(xintercept = as.Date("2015-03-01")) +
    labs(x = NULL,
         y = "Índice de Riesgo de Corrupción") +
    facet_wrap(~tipo_municipalidad, ncol = 3, scales = "free_y") 

combined_data_clean %>% 
    pivot_longer(cols = F1:F2)%>% 
    group_by(mesanho_publicacion, tipo_municipalidad, name) %>% 
    mutate(
        value = mean(value, na.rm = TRUE)
    ) %>% 
    ggplot(aes(x = mesanho_publicacion, y = value)) +
    geom_line(alpha = 1) +
    geom_smooth(method = "loess", se = FALSE, color = "black") +
    geom_smooth(data = . %>% filter(mesanho_publicacion <= as.Date("2015-03-01")),
                method = "loess", se = FALSE, color = "blue") +
    geom_smooth(data = . %>% filter(mesanho_publicacion > as.Date("2015-03-01")),
                method = "loess", se = FALSE, color = "red") +
    geom_vline(xintercept = as.Date("2015-03-01")) +
    labs(x = NULL,
         y = "Índice de Riesgo de Corrupción") +
    facet_wrap(~tipo_municipalidad + name, ncol = 2, scales = "free_y") 

### MODELS:

library(plm)

model_df <- combined_data_clean %>% 
    left_join(combined_data %>% select(gobierno, mesanho_publicacion,
                                       tipo_municipalidad,
                                       competitividad_index, numero_efectivo_partidos, percentage_canon_category,
                                       OCI_exists_any, gender_of_mayor, 
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
        numero_efectivo_partidos = case_when(numero_efectivo_partidos < 1 ~ "]0, 1]",
                                             numero_efectivo_partidos < 2 ~ "]1, 2]",
                                             numero_efectivo_partidos < 3 ~ "]2, 3]",
                                             numero_efectivo_partidos < 4 ~ "]3, 4]",
                                             numero_efectivo_partidos > 5 ~ "]5, Inf[",
                                             is.na(numero_efectivo_partidos) ~ "No disponible"),
        month_publicacion = factor(month(mesanho_publicacion, label = TRUE), ordered = FALSE)
    ) %>% 
    select(
        gobierno, mesanho_publicacion, time_var, tipo_municipalidad, cutoff,
        competitividad_category, numero_efectivo_partidos, percentage_canon_category, month_publicacion,
        OCI_exists_any, gender_of_mayor, mean_indices, F1, F2, 
        avg_bidders_per_project:perc_valor_adj_del_total, avg_bidders_per_project_original:perc_valor_adj_del_total_original
    ) %>% 
    group_by(gobierno, mesanho_publicacion, tipo_municipalidad) %>%
    slice(1) %>%
    ungroup() %>% 
    filter(mean_indices < quantile(mean_indices, probs = 0.99), mean_indices > quantile(mean_indices, probs = 0.01))

plm_model_df <- pdata.frame(model_df, index = c("gobierno", "mesanho_publicacion"))

library(lme4)
library(broom)
library(broom.mixed)
library(lmtest)

indices <- c("mean_indices", "F1", "F2", 
             "avg_bidders_per_project","avg_ratio_between_winner_and_publicado",
             "perc_single_bidder", "perc_repeated_winners", "perc_proyectos_debajo_cutoff",
             "perc_proyectos_sobrecosto", "perc_valor_adj_del_total")
explanation <- c("Índice compuesto de riesgo de corrupción", "Primer Factor", "Segundo Factor")
tipo_municipalidad <- unique(model_df$tipo_municipalidad)


result_df_overall <- data.frame()
for (i in seq_along(indices)) {
        index <- indices[i]
        print(paste0("Processing ", index, " for ", tipo_municipalidad, " municipalidad", " ..."))
        
        # Subset the data for the current objeto and tipo_municipalidad
        subset_model_df <- model_df
        subset_plm_model_df <- plm_model_df
        
        # Models with controls
        linear_model <- lm(data = subset_model_df, formula = reformulate("cutoff + cutoff:time_var + time_var + numero_efectivo_partidos + competitividad_category + 
                  gender_of_mayor + OCI_exists_any + percentage_canon_category + month_publicacion", 
                  response = index))
        linear_model_hc <- coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC1"))
        
        individual_within_model <- plm(data = subset_plm_model_df, formula = reformulate("cutoff + cutoff:time_var + time_var + numero_efectivo_partidos + competitividad_category +
                  gender_of_mayor + OCI_exists_any + percentage_canon_category + month_publicacion", 
                  response = index),
                  model = "within", effect = "individual")
        individual_within_model_hc <- coeftest(individual_within_model, vcov = vcovHC(individual_within_model, type = "HC1"))
        
        # Models without controls
        linear_model_nc <- lm(data = subset_model_df, formula = reformulate("cutoff + cutoff:time_var + time_var", response = index))
        linear_model_nc_hc <- coeftest(linear_model_nc, vcov = vcovHC(linear_model_nc, type = "HC1"))
        
        individual_within_model_nc <- plm(data = subset_plm_model_df, formula = reformulate("cutoff + cutoff:time_var + time_var", response = index),
                                          model = "within", effect = "individual")
        individual_within_model_nc_hc <- coeftest(individual_within_model_nc, vcov = vcovHC(individual_within_model_nc, type = "HC1"))
        
        
        # Extract coefficient and SE for "cutoff" and "cutoff:mesanho_publicacion"
        models <- list(linear_model, individual_within_model, linear_model_nc, individual_within_model_nc)
        model_names <- c("Linear model with controls", "Within model with controls",
                         "Linear model without controls", "Within model without controls")
        
        for (j in seq_along(models)) {
            coef_df <- tidy(models[[j]]) %>% 
                select(term, estimate, std.error, statistic, p.value)
            coef_df$model <- model_names[j]
            coef_df$index <- index
            coef_df$tipo_bien <- "overall"
            coef_df$tipo_municipalidad <- "overall"
            
            # Append to the result data frame
            result_df_overall <- rbind(result_df_overall, coef_df)
        }
}

result_df_overall %>% write_clip()

result_df <- data.frame()
for (i in seq_along(indices)) {
        for (municipalidad in tipo_municipalidad) {
            index <- indices[i]
            print(paste0("Processing ", index, " for overall..."))
            
            # Subset the data for the current objeto and tipo_municipalidad
            subset_model_df <- model_df %>% filter(tipo_municipalidad == municipalidad)
            subset_plm_model_df <- plm_model_df %>% filter(tipo_municipalidad == municipalidad)
            
            # Models with controls
            linear_model <- lm(data = subset_model_df, formula = reformulate("cutoff + cutoff:time_var + time_var + numero_efectivo_partidos + competitividad_category + 
                  gender_of_mayor + OCI_exists_any + percentage_canon_category + month_publicacion", 
                  response = index))
            linear_model_hc <- coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC1"))
            
            individual_within_model <- plm(data = subset_plm_model_df, formula = reformulate("cutoff + cutoff:time_var + time_var + numero_efectivo_partidos + competitividad_category +
                  gender_of_mayor + OCI_exists_any + percentage_canon_category + month_publicacion", 
                  response = index),
                  model = "within", effect = "individual")
            individual_within_model_hc <- coeftest(individual_within_model, vcov = vcovHC(individual_within_model, type = "HC1"))
            
            # Models without controls
            linear_model_nc <- lm(data = subset_model_df, formula = reformulate("cutoff + cutoff:time_var + time_var", response = index))
            linear_model_nc_hc <- coeftest(linear_model_nc, vcov = vcovHC(linear_model_nc, type = "HC1"))
            
            individual_within_model_nc <- plm(data = subset_plm_model_df, formula = reformulate("cutoff + cutoff:time_var + time_var", response = index),
                                              model = "within", effect = "individual")
            individual_within_model_nc_hc <- coeftest(individual_within_model_nc, vcov = vcovHC(individual_within_model_nc, type = "HC1"))
            
            
            # Extract coefficient and SE for "cutoff" and "cutoff:mesanho_publicacion"
            models <- list(linear_model, individual_within_model, linear_model_nc, individual_within_model_nc)
            model_names <- c("Linear model with controls", "Within model with controls",
                             "Linear model without controls", "Within model without controls")
            
            for (j in seq_along(models)) {
                coef_df <- tidy(models[[j]]) %>%
                    select(term, estimate, std.error, statistic, p.value)
                coef_df$model <- model_names[j]
                coef_df$index <- index
                coef_df$tipo_bien <- "overall"
                coef_df$tipo_municipalidad <- municipalidad
                
                # Append to the result data frame
                result_df <- rbind(result_df, coef_df)
            }
        }
}

result_df %>% write_clip()

# Crea dos dataframes: uno para "cutoffTRUE" y otro para "cutoffTRUE:time_var"
df_cutoff <- result_df %>% filter(term == "cutoffTRUE")
df_time_var <- result_df %>% filter(term == "cutoffTRUE:time_var")
df_time_var_alone <- result_df %>% filter(term == "time_var")

# Ensanchar cada dataframe
df_cutoff_wide <- df_cutoff %>% spread(term, estimate)
df_time_var_wide <- df_time_var %>% spread(term, estimate)
df_time_var_alone_wide <- df_time_var_alone %>% spread(term, estimate)

# Une los dos dataframes ensanchados
df_wide <- df_cutoff_wide %>% 
    left_join(df_time_var_wide, by = c("model", "tipo_bien", "tipo_municipalidad"),
              suffix = c("_cutoff", "_time_var")) %>% 
    left_join(df_time_var_alone_wide, by = c("model", "tipo_bien", "tipo_municipalidad"))

df_wide %>% write_clip()

df_wide %>%
    filter(str_detect(model, "inear")) %>% 
    arrange(tipo_municipalidad, tipo_bien, model) %>%
    select(model, tipo_municipalidad, tipo_bien, cutoffTRUE,
           std.error_cutoff, p.value_cutoff, `cutoffTRUE:time_var`, std.error_time_var, p.value_time_var,
           time_var, std.error, p.value) %>%
    write_clip()

df_wide %>%
    filter(str_detect(model, "ithin")) %>% 
    arrange(tipo_municipalidad, tipo_bien, model) %>%
    select(model, tipo_municipalidad, tipo_bien, cutoffTRUE,
           std.error_cutoff, p.value_cutoff, `cutoffTRUE:time_var`, std.error_time_var, p.value_time_var,
           time_var, std.error, p.value) %>%
    write_clip()

### CONTROL REGRESSIONS

# plots

control_vars <- c("numero_efectivo_partidos", "competitividad_category", "gender_of_mayor", "percentage_canon_category", "month_publicacion")
control_plots <- list()

for (i in seq_along(control_vars)) {
    current_control = control_vars[i]
    
    # Determine if the control variable is numeric or factor
    if (is.numeric(model_df[[current_control]]) || is.integer(model_df[[current_control]])) {
        plot_effect <- model_df %>%
            ggplot(aes_string(x = current_control, y = "mean_indices")) +
            geom_point(alpha = 0.05) +
            geom_smooth(method = "loess", se = FALSE, color = "blue") +
            labs(x = current_control, y = "mean_indices", 
                 title = paste("Relationship between", current_control, "and mean_indices")) +
            theme_minimal()
    } else {
        # Assume it's a factor or character variable
        plot_effect <- model_df %>%
            filter(!is.na(get(current_control))) %>% 
            ggplot(aes_string(x = current_control, y = "mean_indices")) +
            geom_violin(trim = TRUE) +
            labs(x = current_control, y = "mean_indices", 
                 title = paste("Boxplot of mean_indices by", current_control)) +
            theme_minimal()
    }
    
    control_plots[[i]] <- plot_effect
}

control_plots[[4]]

control_vars <- c("numero_efectivo_partidos", "competitividad_category", "gender_of_mayor", "percentage_canon_category", "month_publicacion")

result_df_control <- data.frame()
for (i in seq_along(control_vars)) {
        for (municipalidad in tipo_municipalidad) {
            for (control in control_vars) {
                index <- indices[i]
                print(paste0("Processing ", index, " for ", " in ", municipalidad, " with control ", control, " ..."))
                
                # Subset the data for the current objeto and tipo_municipalidad
                subset_model_df <- model_df %>% filter(tipo_municipalidad == municipalidad)
                subset_plm_model_df <- plm_model_df %>% filter(tipo_municipalidad == municipalidad)
                
                # Bivariate models - use only one control variable here
                linear_model <- lm(data = subset_model_df, formula = reformulate(control, response = index))
                linear_model_hc <- coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC1"))
                
                individual_within_model <- plm(data = subset_plm_model_df, formula = reformulate(control, response = index),
                                               model = "within", effect = "individual")
                individual_within_model_hc <- coeftest(individual_within_model, vcov = vcovHC(individual_within_model, type = "HC1"))
                
                
                # Extract coefficient and SE for control
                models <- list(linear_model, individual_within_model)
                model_names <- c("Linear model", "Within model")
                
                for (j in seq_along(models)) {
                    coef_df <- tidy(models[[j]]) %>% filter(str_detect(term, control)) %>% 
                        select(term, estimate, std.error, statistic, p.value)
                    coef_df$model <- model_names[j]
                    coef_df$index <- index
                    coef_df$tipo_bien <- objeto
                    coef_df$tipo_municipalidad <- municipalidad
                    
                    # Append to the result data frame
                    result_df_control <- rbind(result_df_control, coef_df)
                }
            }
        }
    }

result_df_control %>% 
    write_clip()

result_df_control %>% 
    filter(str_detect(model, "ithin")) %>% 
    group_by(str_sub(term, 1, 4)) %>% 
    summarise(
        times_it_was_significant = sum(p.value < 0.05),
        perc_of_models_it_was_significant =  mean(p.value < 0.05),
        n_models = n()
    ) %>% write_clip()

### PLOTS FOR CONCRETE INDEX

indices <- c("avg_bidders_per_project", "perc_single_bidder", "avg_ratio_between_winner_and_publicado", 
             "perc_repeated_winners", "perc_proyectos_debajo_cutoff", "perc_proyectos_sobrecosto", "perc_valor_adj_del_total")
for (index in indices) {
    plot_direct <- combined_data %>% 
        group_by(mesanho_publicacion, tipo_municipalidad) %>% 
        summarise(
            mean_indices = mean(get(index), na.rm = TRUE)
        ) %>% 
        ggplot(aes(x = mesanho_publicacion, y = mean_indices)) +
        geom_line(alpha = 1) +
        geom_smooth(method = "loess", se = FALSE, color = "black") +
        geom_smooth(data = . %>% filter(mesanho_publicacion <= as.Date("2015-03-01")),
                    method = "loess", se = FALSE, color = "blue") +
        geom_smooth(data = . %>% filter(mesanho_publicacion > as.Date("2015-03-01")),
                    method = "loess", se = FALSE, color = "red") +
        geom_vline(xintercept = as.Date("2015-03-01")) +
        labs(x = NULL,
             y = paste("Índice de Riesgo de Corrupción -", index)) +
        facet_wrap(~tipo_municipalidad, ncol = 1, scales = "free_y")
    
    # Save the plot with a unique name based on the index
    ggsave(plot = plot_direct, filename = paste0("data/05_plots/mean_index_plot_", index, "_tipo_gobierno.png"),
           width = 500*2.5, height = 300*2.5, scale = 3, units = "px")
}

