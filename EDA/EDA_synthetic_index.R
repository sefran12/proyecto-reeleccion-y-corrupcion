# Index (semester)

osce_infogob_oci_monthly <- read_parquet("data/03_model/osce_infogob_oci_monthly_2017.parquet") %>% ungroup()
osce_infogob_oci_semestral <- read_parquet("data/03_model/osce_infogob_oci_semester_2017.parquet") %>% ungroup()
mef_infogob_oci_yearly <- read_parquet("data/03_model/mef_infogob_oci_anual.parquet") %>% ungroup()

colnames(osce_infogob_oci_monthly)
colnames(osce_infogob_oci_semestral)
colnames(mef_infogob_oci_yearly)

# Handle semester conversion to date
osce_infogob_oci_semestral <- osce_infogob_oci_semestral %>%
    mutate(date = as.Date(semester)) %>%
    select(-semester)

# Create a sequence of monthly dates for each gobierno/OBJETO combination
date_sequence <- osce_infogob_oci_semestral %>%
    group_by(gobierno, OBJETO) %>%
    summarise(min_date = min(date), max_date = max(date)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(date = list(seq(from = min_date, to = max_date, by = "month"))) %>%
    unnest(date)

# Join this sequence back to the data and fill in the missing values
osce_infogob_oci_semestral <- date_sequence %>%
    left_join(osce_infogob_oci_semestral, by = c("gobierno", "OBJETO", "date")) %>%
    group_by(gobierno, OBJETO) %>%
    fill(everything(), .direction = "down")

# Repeat the same steps for the yearly data
mef_infogob_oci_yearly <- mef_infogob_oci_yearly %>%
    mutate(date = as.Date(paste0(ANO_EJE, "-01-01"), "%Y-%m-%d")) %>%
    select(-ANO_EJE)

date_sequence_yearly <- mef_infogob_oci_yearly %>%
    group_by(gobierno, tipo_municipalidad.x) %>%
    summarise(min_date = min(date), max_date = max(date)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(date = list(seq(from = min_date, to = max_date, by = "month"))) %>%
    unnest(date)

mef_infogob_oci_yearly <- date_sequence_yearly %>%
    left_join(mef_infogob_oci_yearly, by = c("gobierno", "tipo_municipalidad.x", "date")) %>%
    group_by(gobierno, tipo_municipalidad.x) %>%
    fill(everything(), .direction = "down")

df3 <- osce_infogob_oci_monthly %>% 
    left_join(osce_infogob_oci_semestral)

# Calculate z-scores
df4 <- df3 %>%
    select(mesanho_publicacion,
           OBJETO,
           perc_repeated_winners, 
           avg_bidders_per_project,
           avg_ratio_between_winner_and_publicado,
           desvicacion_respecto_al_promedio_anual) %>%
    mutate(across(where(is.numeric), scale)) 

# Calculate index
df4 <- df4 %>%
    mutate(
        index = (perc_repeated_winners - avg_bidders_per_project + avg_ratio_between_winner_and_publicado + desvicacion_respecto_al_promedio_anual)/4
    )


library(dplyr)

df4_summary <- df4 %>%
    filter(index < 4) %>% 
    group_by(mesanho_publicacion) %>%
    summarise(
        index_mean = mean(index, na.rm = TRUE),
        index_se = sd(index, na.rm = TRUE) / sqrt(n()),
        ci_lower = index_mean - 1.96 * index_se,
        ci_upper = index_mean + 1.96 * index_se
    )

library(ggplot2)

ggplot(df4_summary, aes(x = mesanho_publicacion, y = index_mean)) +
    geom_line(color = "blue") +
    geom_point() +
    geom_vline(xintercept = as.Date("2015-03-08")) +
    geom_hline(yintercept = 0) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
    labs(x = "Mes-Anho Publicacion", y = "Mean Index with Confidence Interval") +
    theme_minimal()

##

# Separate the summary data into treated and control groups

df4_summary <- df4 %>%
    filter(index < 4,
           index > -4) %>% 
    group_by(mesanho_publicacion, OBJETO) %>%
    summarise(
        index_mean = mean(index, na.rm = TRUE),
        index_se = sd(index, na.rm = TRUE) / sqrt(n()),
        ci_lower = index_mean - 1.96 * index_se,
        ci_upper = index_mean + 1.96 * index_se
    )

# Separate the summary data into different groups

otra_entidad_plot <- ggplot(df4_summary, aes(x = mesanho_publicacion, y = index_mean, color = OBJETO)) +
    geom_point() +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
    geom_vline(xintercept = as.Date("2015-03-08")) +
    labs(x = "Mes-Anho Publicacion", y = "Mean Index with Confidence Interval - Otra Entidad") +
    theme_minimal() +
    facet_wrap(~OBJETO, ncol = 1, scales = "free")

otra_entidad_plot
    

## FA

# Install and load the necessary package
if (!require(factoextra)) install.packages("factoextra")
library(factoextra)

# Prepare the data for FA
fa_data <- df3 %>%
    select(perc_repeated_winners, 
           avg_bidders_per_project, 
           avg_ratio_between_winner_and_publicado, 
           desvicacion_respecto_al_promedio_anual) %>% 
    mutate_if(is.numeric, )

# Conduct the FA
fa_result <- psych::fa(fa_data, nfactors = 1, scores = "tenBerge")

# Check the loadings of the FA
fa_result$loadings

df4 <- df3 %>%
    mutate(
        index = fa_result$loadings[1]*perc_repeated_winners + 
            fa_result$loadings[2]*avg_bidders_per_project + 
            fa_result$loadings[3]*avg_ratio_between_winner_and_publicado + 
            fa_result$loadings[4]*desvicacion_respecto_al_promedio_anual
    )

df4 %>% 
    ggplot(aes(x = mesanho_publicacion, y = index)) 


df4_summary <- df4 %>%
    group_by(mesanho_publicacion, OBJETO) %>%
    summarise(
        index_mean = mean(index, na.rm = TRUE),
        index_se = sd(index, na.rm = TRUE) / sqrt(n()),
        ci_lower = index_mean - 1.96 * index_se,
        ci_upper = index_mean + 1.96 * index_se
    )

# Separate the summary data into different groups

otra_entidad_plot <- ggplot(df4_summary, aes(x = mesanho_publicacion, y = index_mean, color = OBJETO)) +
    geom_point() +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
    geom_vline(xintercept = as.Date("2015-03-08")) +
    labs(x = "Mes-Anho Publicacion", y = "Mean Index with Confidence Interval - Otra Entidad") +
    theme_minimal() +
    facet_wrap(~OBJETO, ncol = 1, scales = "free")

otra_entidad_plot


