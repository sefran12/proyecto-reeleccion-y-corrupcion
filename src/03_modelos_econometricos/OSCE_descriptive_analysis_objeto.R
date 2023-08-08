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

osce_infogob_oci_monthly <- read_parquet("data/03_model/osce_infogob_oci_monthly_2017_objeto.parquet") %>% ungroup()
osce_infogob_oci_semestral <- read_parquet("data/03_model/osce_infogob_oci_semester_2017_objeto.parquet") %>% ungroup()
mef_infogob_oci_yearly <- read_parquet("data/03_model/mef_infogob_oci_anual.parquet") %>% ungroup()
mef_osce_matching <- read_parquet("data/02_intermediate/all_matches_mef_osce.parquet")

# Normalize osce_month database
osce_month <- osce_infogob_oci_monthly %>% 
    select(
        gobierno,
        OBJETO,
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
        OBJETO,
        mesanho_publicacion = semester,
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
    mutate(tipo_municipalidad = coalesce(tipo_municipalidad.x, tipo_municipalidad.y)) %>% 
    select(-tipo_municipalidad.x, -tipo_municipalidad.y, 
           -start_date.x, -start_date.y, -end_date.x, -end_date.y,
           -gobierno_mef)

combined_data <- combined_data %>% 
    filter(tipo_municipalidad != "Otra entidad") %>% 
    group_by(gobierno, OBJETO) %>% 
    fill(everything(), .direction = "down") %>% 
    ungroup()

# Dealing with NAs

combined_data_clean <- combined_data %>%
    mutate(numero_efectivo_partidos = if_else(is.infinite(numero_efectivo_partidos), 1, numero_efectivo_partidos)) %>% 
    select(gobierno, OBJETO, mesanho_publicacion, tipo_municipalidad, avg_bidders_per_project, avg_ratio_between_winner_and_publicado,
           perc_single_bidder, perc_repeated_winners, perc_proyectos_debajo_cutoff, perc_proyectos_sobrecosto, perc_valor_adj_del_total) %>%
    group_by(gobierno, OBJETO) %>%
    ungroup() %>% 
    na.omit()

### Análisis Descriptivo
# Resumen de estadísticas
summary_OSCE <- combined_data_clean %>% 
    select(OBJETO, avg_bidders_per_project, avg_ratio_between_winner_and_publicado, perc_single_bidder, 
           perc_repeated_winners, perc_valor_adj_del_total) %>% 
    group_by(OBJETO) %>% 
    skim()

summary_OSCE %>% 
    select(-skim_type, -n_missing, -complete_rate, -numeric.hist) %>% 
    write_clip()

summary_OSCE_tipomuni <- combined_data_clean %>% 
    select(tipo_municipalidad, OBJETO, avg_bidders_per_project, avg_ratio_between_winner_and_publicado, perc_single_bidder, 
           perc_repeated_winners, perc_valor_adj_del_total) %>% 
    group_by(tipo_municipalidad, OBJETO) %>% 
    skim()

summary_OSCE_tipomuni %>% 
    select(-skim_type, -n_missing, -complete_rate, -numeric.hist) %>% 
    write_clip()

## MEF

summary_MEF <- combined_data_clean %>% 
    select(OBJETO, perc_proyectos_debajo_cutoff, perc_proyectos_sobrecosto) %>% 
    group_by(OBJETO) %>% 
    skim()

summary_MEF %>% 
    select(-skim_type, -n_missing, -complete_rate, -numeric.hist) %>% 
    write_clip()

summary_MEF_tipomuni <- combined_data_clean %>% 
    select(tipo_municipalidad, OBJETO, perc_proyectos_debajo_cutoff, perc_proyectos_sobrecosto) %>% 
    group_by(tipo_municipalidad, OBJETO) %>% 
    skim()

summary_MEF_tipomuni %>% 
    select(-skim_type, -n_missing, -complete_rate, -numeric.hist) %>% 
    write_clip()

## Tendencias temporales

combined_data_clean %>% 
    pivot_longer(cols = -c(gobierno, mesanho_publicacion, tipo_municipalidad, OBJETO)) %>% 
    group_by(mesanho_publicacion, name, OBJETO) %>% 
    summarise(
        mean_indices = mean(value, na.rm = TRUE)
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
         y = "Índicadores de Riesgo de Corrupción") +
    facet_wrap(~name + OBJETO, ncol = 6, scales = "free_y") 

# Análisis de correlación entre indicadores
correlation_analysis <- combined_data_clean %>% 
    select(-c(gobierno:tipo_municipalidad, OBJETO)) %>% 
    select_if(is.numeric) %>% 
    as.matrix.data.frame() %>% 
    cor(., use = "pairwise.complete.obs") %>% 
    as.data.frame() %>% 
    write_clip()

# Ensure necessary packages are installed
library(Hmisc)
library(xtable)

addSignificanceStars <- function(table, pvalues) {
    for (i in 1:nrow(table)) {
        for (j in 1:ncol(table)) {
            if (!is.na(pvalues[i, j])) {
                if (pvalues[i, j] < .001) table[i, j] <- paste0(table[i, j], "***")
                else if (pvalues[i, j] < .01) table[i, j] <- paste0(table[i, j], "**")
                else if (pvalues[i, j] < .05) table[i, j] <- paste0(table[i, j], "*")
            }
        }
    }
    return(table)
}


# Create correlation matrix with significance level
correlation_analysis <- combined_data_clean %>% 
    select(-c(gobierno:tipo_municipalidad, OBJETO)) %>% 
    select_if(is.numeric)

correlation_result <- rcorr(as.matrix(correlation_analysis), type = "spearman")

# Create pretty-printed table
correlation_table <- xtable(round(correlation_result$r, 4))

# Add significance stars
correlation_table <- addSignificanceStars(correlation_table, correlation_result$P)

# Print the table
print.xtable(correlation_table, type = "html", html.table.attributes = "style='width:100%'")

# If you want to write the table to clipboard for pasting into Excel
write_clip(correlation_table)
write_clip(correlation_result$r)

combined_data_clean %>% 
    ggplot(aes(y = perc_single_bidder, x = avg_bidders_per_project)) +
    geom_point() +
    geom_smooth(method = "lm")

combined_data_clean %>% 
    filter(perc_single_bidder < 0.02, avg_bidders_per_project < 0.2) %>% 
    ggplot(aes(y = perc_single_bidder, x = avg_bidders_per_project)) +
    geom_point() +
    geom_smooth(method = "lm")

osce_infogob_oci_monthly %>% 
    ggplot(aes(y = perc_single_bidder, x = avg_bidders_per_project)) +
    geom_point() +
    geom_smooth(method = "lm")
