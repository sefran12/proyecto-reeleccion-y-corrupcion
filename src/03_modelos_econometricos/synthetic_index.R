# Index (semester)
library(arrow)
library(tidyverse)
library(fuzzyjoin)
library(lubridate)

theme_set(theme_bw())

osce_infogob_oci_monthly <- read_parquet("data/03_model/osce_infogob_oci_monthly_2017.parquet") %>% ungroup()
osce_infogob_oci_semestral <- read_parquet("data/03_model/osce_infogob_oci_semester_2017.parquet") %>% ungroup()
mef_infogob_oci_yearly <- read_parquet("data/03_model/mef_infogob_oci_anual.parquet") %>% ungroup()
mef_osce_matching <- read_parquet("data/02_intermediate/all_matches_mef_osce.parquet")

colnames(osce_infogob_oci_monthly)
colnames(osce_infogob_oci_semestral)
colnames(mef_infogob_oci_yearly)

# Normalize osce_month database
osce_month <- osce_infogob_oci_monthly %>% 
    select(
        gobierno,
        mesanho_publicacion,
        OBJETO,
        tipo_municipalidad = tipo_municipalidad.x,
        avg_bidders_per_project,
        perc_single_bidder,
        avg_ratio_between_winner_and_publicado,
        perc_valor_adj_del_total,
        percentage_canon,
        percentage_canon_category,
        OCI_exists_any,
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
        OBJETO,
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
    left_join(osce_semester, by = c("mesanho_publicacion", "gobierno", "OBJETO"))
combined_data <- combined_data %>%
    left_join(mef_osce_matching, by = c("gobierno" = "gobierno_osce"))
combined_data <- combined_data %>%
    left_join(mef_yearly, by = c("mesanho_publicacion", "gobierno_mef" = "gobierno"))

combined_data <- combined_data %>%
    group_by(gobierno, mesanho_publicacion, OBJETO) %>%
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
    group_by(gobierno, OBJETO) %>% 
    fill(everything(), .direction = "down") %>% 
    ungroup()

# Dealing with NAs

library(zoo)

combined_data_clean <- combined_data %>%
    select(gobierno, mesanho_publicacion, OBJETO, tipo_municipalidad, avg_bidders_per_project, avg_ratio_between_winner_and_publicado,
           perc_valor_adj_del_total,
           perc_single_bidder, perc_repeated_winners, perc_proyectos_debajo_cutoff, perc_proyectos_sobrecosto) %>%
    mutate(avg_bidders_per_project = -avg_bidders_per_project) %>%
    group_by(gobierno, OBJETO) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
    group_by(gobierno) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
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
means_first_year <- colMeans(select(first_year, avg_bidders_per_project:perc_proyectos_sobrecosto), na.rm = TRUE)
sds_first_year <- apply(select(first_year, avg_bidders_per_project:perc_proyectos_sobrecosto), 2, sd, na.rm = TRUE)

# Apply custom scaling
combined_data_clean <- combined_data_clean %>%
    filter(OBJETO != "Other") %>% 
    mutate(across(avg_bidders_per_project:perc_proyectos_sobrecosto, 
                  ~custom_scale(., means_first_year[[cur_column()]], 
                                sds_first_year[[cur_column()]]))) %>%
    mutate(mean_indices = rowMeans(select(., avg_bidders_per_project:perc_proyectos_sobrecosto)))

mean_index_plot <- combined_data_clean %>% 
    group_by(mesanho_publicacion, OBJETO) %>% 
    summarise(
        mean_indices = mean(mean_indices, na.rm = TRUE)
    ) %>% 
    ggplot(aes(x = mesanho_publicacion, y = mean_indices, color = OBJETO)) +
    geom_line(alpha = 1) +
    geom_vline(xintercept = as.Date("2015-03-01")) +
    labs(x = NULL,
         y = "Índice de Riesgo de Corrupción",
         color = "Tipo de proyecto")

ggsave(plot = mean_index_plot, filename = "data/05_plots/mean_index_plot.png",
       width = 500, height = 300, scale = 3, units = "px")

mean_index_plot2 <- combined_data_clean %>% 
    group_by(mesanho_publicacion, OBJETO, tipo_municipalidad) %>% 
    summarise(
        mean_indices = mean(mean_indices, na.rm = TRUE)
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
    facet_wrap(~tipo_municipalidad + OBJETO, ncol = 3, scales = "free_y") 


ggsave(plot = mean_index_plot2, filename = "data/05_plots/mean_index_plot_tipo_gob.png",
       width = 500*2.5, height = 300*2.5, scale = 3, units = "px")


combined_data_clean %>% 
    group_by(año = year(mesanho_publicacion), tipo_municipalidad, OBJETO) %>% 
    summarise(
        mean_index = mean(mean_indices, na.rm = TRUE),
        sd_index = sd(mean_indices, na.rm = TRUE)
    ) %>% 
    pivot_wider(names_from = OBJETO, values_from = c(mean_index, sd_index)) %>% 
    arrange(tipo_municipalidad, año) %>% 
    clipr::write_clip()

## PCA
pca_result <- prcomp(combined_data_clean[, 5:10], scale. = TRUE)
summary(pca_result)
pca_result
pca_scores <- pca_result$x
combined_data_clean <- cbind(combined_data_clean, pca_scores)


## Exploratory FA
fa_result <- factanal(combined_data_clean[, 5:10], factors = 3, scores = "regression")
print(fa_result)

# Factor Analysis scores
library(psych)

fa_scores <- factor.scores(combined_data_clean[, 5:10], fa_result$loadings)$scores
fa_scores_df <- as.data.frame(fa_scores, stringsAsFactors = FALSE)

# Adding Factor Analysis scores to the dataset
combined_data_clean <- cbind(combined_data_clean, fa_scores_df)

### Confirmatory FA
library(lavaan)
library(clipr)

# Define the CFA model
# This syntax specifies that each of the indices is loaded on a single factor (F1)
cfa_model <- 'F1 =~ avg_bidders_per_project + perc_single_bidder + avg_ratio_between_winner_and_publicado + perc_valor_adj_del_total +
              perc_repeated_winners + perc_proyectos_debajo_cutoff + perc_proyectos_sobrecosto '

# Fit the model
fit_1_factor <- cfa(cfa_model, data = combined_data_clean)

# Summary of the model
summary(fit_1_factor, fit.measures = TRUE)

tidy(fit_1_factor) %>% write_clip()

library(lavaan)

# Define the CFA model
# This syntax specifies that perc_proyectos_debajo_cutoff and perc_proyectos_sobrecosto load on F1
# avg_bidders_per_project, perc_single_bidder, perc_repeated_winners, and perc_proyectos_debajo_cutoff load on F2
# and an overall factor (F) is indicated by F1 and F2
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
  F =~ F1 + F2
'

# Fit the model
fit_hierar_factors <- cfa(cfa_model, data = combined_data_clean %>% select(-F1, -F2))

# Summary of the model
summary(fit_hierar_factors, fit.measures = TRUE)

# DOES NOT WORK

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
    filter(tipo_municipalidad == "distrital", OBJETO == "Bienes") %>% 
    lm(data = . ,
       formula = mean_indices ~ cutoff*mesanho_publicacion + gobierno,
       ) %>% 
    broom::tidy() %>% View()


combined_data_clean %>% 
    group_by(mesanho_publicacion, OBJETO) %>% 
    summarise(
        mean_indices = mean((F1 + F2)/2, na.rm = TRUE)
    ) %>% 
    ggplot(aes(x = mesanho_publicacion, y = mean_indices, color = OBJETO)) +
    geom_line(alpha = 1) +
    geom_vline(xintercept = as.Date("2015-03-01")) +
    labs(x = NULL,
         y = "Índice de Riesgo de Corrupción",
         color = "Tipo de proyecto")

combined_data_clean %>% 
    group_by(mesanho_publicacion, OBJETO, tipo_municipalidad) %>% 
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
    facet_wrap(~tipo_municipalidad + OBJETO, ncol = 3, scales = "free_y") 

### MODELS:

library(plm)

model_df <- combined_data_clean %>% 
    left_join(combined_data %>% select(gobierno, mesanho_publicacion,
                                       OBJETO, tipo_municipalidad,
                                       competitividad_index, numero_efectivo_partidos, percentage_canon_category,
                                        OCI_exists_any, gender_of_mayor)) %>% 
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
                                             numero_efectivo_partidos > 5 ~ "]5, 6]",
                                             is.na(numero_efectivo_partidos) ~ "No disponible"),
        month_publicacion = factor(month(mesanho_publicacion)),
        mean_indices = (F1 + F2) / 2 
) %>% 
    select(
        gobierno, mesanho_publicacion, time_var, OBJETO, tipo_municipalidad, cutoff,
        competitividad_category, numero_efectivo_partidos, percentage_canon_category, month_publicacion,
        OCI_exists_any, gender_of_mayor, mean_indices
    )


model_df$gobierno_objeto = str_c(model_df$gobierno, "-", model_df$OBJETO)
plm_model_df <- pdata.frame(model_df, index = c("gobierno_objeto", "mesanho_publicacion"))

library(lme4)
library(broom)
library(broom.mixed)
library(lmtest)

indices <- c("mean_indices")
explanation <- c("Índice compuesto de riesgo de corrupción")
tipo_bien <- unique(model_df$OBJETO)
tipo_municipalidad <- unique(model_df$tipo_municipalidad)

result_df <- data.frame()
for (i in seq_along(indices)) {
    for (objeto in tipo_bien) {
        for (municipalidad in tipo_municipalidad) {
            index <- indices[i]
            print(paste0("Processing ", index, " for ", objeto, " in ", municipalidad, " ..."))
            
            # Subset the data for the current objeto and tipo_municipalidad
            subset_model_df <- model_df %>% filter(OBJETO == objeto & tipo_municipalidad == municipalidad)
            subset_plm_model_df <- plm_model_df %>% filter(OBJETO == objeto & tipo_municipalidad == municipalidad)
            
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
                coef_df <- tidy(models[[j]]) %>% filter(term %in% c("cutoffTRUE", "cutoffTRUE:time_var", "time_var")) %>% 
                    select(term, estimate, std.error, statistic, p.value)
                coef_df$model <- model_names[j]
                coef_df$index <- index
                coef_df$tipo_bien <- objeto
                coef_df$tipo_municipalidad <- municipalidad
                
                # Append to the result data frame
                result_df <- rbind(result_df, coef_df)
            }
        }
    }
}

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

control_vars <- c("numero_efectivo_partidos", "competitividad_category", "gender_of_mayor", "percentage_canon_category", "month_publicacion")

result_df_control <- data.frame()
for (i in seq_along(indices)) {
    for (objeto in tipo_bien) {
        for (municipalidad in tipo_municipalidad) {
            for (control in control_vars) {
                index <- indices[i]
                print(paste0("Processing ", index, " for ", objeto, " in ", municipalidad, " with control ", control, " ..."))
                
                # Subset the data for the current objeto and tipo_municipalidad
                subset_model_df <- model_df %>% filter(OBJETO == objeto & tipo_municipalidad == municipalidad)
                subset_plm_model_df <- plm_model_df %>% filter(OBJETO == objeto & tipo_municipalidad == municipalidad)
                
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
             "perc_repeated_winners", "perc_proyectos_debajo_cutoff", "perc_proyectos_sobrecosto")
for (index in indices) {
    plot <- combined_data_clean %>% 
        group_by(mesanho_publicacion, OBJETO, tipo_municipalidad) %>% 
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
        facet_wrap(~tipo_municipalidad + OBJETO, ncol = 3, scales = "free_y")
    
    # Save the plot with a unique name based on the index
    ggsave(plot = plot, filename = paste0("data/05_plots/mean_index_plot_", index, ".png"),
           width = 500*2.5, height = 300*2.5, scale = 3, units = "px")
}
