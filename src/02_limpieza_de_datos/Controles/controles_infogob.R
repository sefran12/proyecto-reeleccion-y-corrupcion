# Cargamos las bibliotecas necesarias
library(readxl)
library(stringr)
library(janitor)
library(tidyverse)
library(arrow)

# Creamos un data frame vacío para almacenar los datos
controles_infogob <- data.frame()

file_list <- dir(path = "data/01_raw/Base_de_datos_INFOgob", full.names = TRUE, recursive = TRUE)

# Iteramos sobre cada archivo en la lista
for (curr_file in file_list) {
    cat("Processing", curr_file, "\n")
    
    if (!str_detect(curr_file, "Resultados|RESU|resu") | !str_detect(curr_file, "Provincial|Distrital|PROV|DIST|prov|dist|REGIO")) {
        cat("Not a results file, skipping...", "\n")
        next
    }
    
    # Leemos los datos
    curr_data <- read_excel(curr_file)
    curr_data <- clean_names(curr_data)
    
    # Transformamos las características
    curr_data$year <- str_extract(curr_file, "[1-2][0-9]{3}")
    curr_data$tipo_municipalidad <- if_else(str_detect(curr_file, "PROVINCIAL"), "provincial",
                                            if_else(str_detect(curr_file, "REGI"), "regional", "distrital"))
    if (!("distrito" %in% colnames(curr_data))) {
        curr_data$distrito <- "general"
    }
    if (!("provincia" %in% colnames(curr_data))) {
        curr_data$provincia <- "general"
    }
    
    # Calculamos el índice de fragmentación por entidad y ciclo
    curr_data <- curr_data %>%
        filter(!str_detect(organizacion_politica, "Votos|VOTOS")) %>% 
        group_by(region, provincia, distrito, year, tipo_municipalidad) %>%
        mutate(percent_votos_sq = percent_votos^2) %>%
        mutate(winning_percent_votos_sq = max(percent_votos, na.rm = TRUE)^2) %>%
        ungroup() %>% 
        group_by(region, provincia, distrito, year, tipo_municipalidad) %>%
        summarise(numero_real_de_partidos = n(),
                  max_porc_partido = max(percent_votos), 
                  fragmentation_index = 1 - sum(percent_votos_sq, na.rm = TRUE),
                  numero_efectivo_partidos = 1 / sum(percent_votos_sq, na.rm = TRUE),
                  numero_partidos_molinar = ( 1 + numero_real_de_partidos^2 * sum( percent_votos_sq , na.rm = TRUE) ),
                  hiperfraccionamiento = exp( -sum( percent_votos * log(percent_votos) , na.rm = TRUE) ),
                  competitividad_index = 1 + (
                      (1 / sum(percent_votos_sq, na.rm = TRUE)) * (
                          (sum(percent_votos_sq, na.rm = TRUE) - mean(winning_percent_votos_sq)) /
                              mean(winning_percent_votos_sq)
                      )
                  ),
                  .groups = "drop")
    
    
    # Unimos los datos al data frame completo
    controles_infogob <- bind_rows(controles_infogob, curr_data)
}

# GENERO
genero_infogob <- data.frame()

# GENERO
for (curr_file in file_list) {
    cat("Processing", curr_file, "\n")
    
    if (!str_detect(curr_file, "Autor|AUTOr|autor") | !str_detect(curr_file, "Provincial|Distrital|PROV|DIST|prov|dist|REGIO")) {
        cat("Not an autoridades file, skipping...", "\n")
        next
    }
    
    # Leemos los datos
    curr_data <- read_excel(curr_file)
    curr_data <- clean_names(curr_data)
    
    # If the distrito column does not exist, create it and fill with "general"
    if (!("distrito" %in% colnames(curr_data))) {
        curr_data$distrito <- "general"
    }
    if (!("provincia" %in% colnames(curr_data))) {
        curr_data$provincia <- "general"
    }
    
    # Filter data for elected mayors
    curr_data <- curr_data %>%
        filter(cargo_electo == "ALCALDE PROVINCIAL" | cargo_electo == "ALCALDE DISTRITAL" | cargo_electo == "PRESIDENTE REGIONAL") %>%
        mutate(year = str_extract(curr_file, "[1-2][0-9]{3}"),
               tipo_municipalidad = if_else(str_detect(curr_file, "PROVINCIAL"), "provincial",
                                            if_else(str_detect(curr_file, "DISTR"), "distrital", "regional")))
    
    # Calculate gender of the elected mayor for each region, province, district, year, and type of municipality
    curr_data <- curr_data %>%
        group_by(region, provincia, distrito, year, tipo_municipalidad) %>%
        summarise(gender_of_mayor = first(sexo), 
                  .groups = "drop")
    
    # Append the data to the main data frame
    genero_infogob <- bind_rows(genero_infogob, curr_data)
}


# Vuelve panel 2002:2022
library(lubridate)

# Add a date column to the genero_infogob dataset
genero_infogob <- genero_infogob %>%
    mutate(date = as.Date(paste0(year, "-01-01")))

controles_infogob <- controles_infogob %>%
    mutate(date = as.Date(paste0(year, "-01-01")))

# Join controles_infogob and genero_infogob datasets
full_controles_infogob <- controles_infogob %>%
    left_join(genero_infogob, by = c("region", "provincia", "distrito", "tipo_municipalidad", "date", "year"))

# Function to create expanded datasets
create_panel_infogob <- function(dates){
    panel <- controles_infogob %>%
        tidyr::expand(nesting(region, provincia, distrito, tipo_municipalidad), date = dates) %>%
        left_join(full_controles_infogob, by = c("region", "provincia", "distrito", "tipo_municipalidad", "date")) %>%
        fill(fragmentation_index, max_porc_partido, numero_real_de_partidos, numero_efectivo_partidos, numero_partidos_molinar, hiperfraccionamiento, competitividad_index, gender_of_mayor, .direction = "down")
    return(panel)
}

# Generate list of all first dates of the months and semesters within the range
monthly_dates <- seq(as.Date("2002-01-01"), as.Date("2022-12-01"), by = "month")
semestral_dates <- seq(as.Date("2002-01-01"), as.Date("2022-07-01"), by = "6 months")
yearly_dates <- seq(as.Date("2002-01-01"), as.Date("2022-07-01"), by = "year")

# Create expanded datasets
monthly_infogob <- create_panel_infogob(monthly_dates)
semestral_infogob <- create_panel_infogob(semestral_dates)
yearly_infogob <- create_panel_infogob(yearly_dates)

# 'fill' yearly data to each month within the same year
monthly_infogob <- monthly_infogob %>%
    group_by(region, provincia, distrito, tipo_municipalidad, year(date)) %>%
    fill(fragmentation_index, max_porc_partido, numero_real_de_partidos, numero_efectivo_partidos, numero_partidos_molinar, hiperfraccionamiento, competitividad_index, gender_of_mayor, year, .direction = "downup")

# 'fill' yearly data to each semester within the same year
semestral_infogob <- semestral_infogob %>%
    group_by(region, provincia, distrito, tipo_municipalidad, year(date), semester(date)) %>%
    fill(fragmentation_index, max_porc_partido, numero_real_de_partidos, numero_efectivo_partidos, numero_partidos_molinar, hiperfraccionamiento, competitividad_index, gender_of_mayor, year, .direction = "downup")

# 'fill' yearly data to each year within the same term
yearly_infogob <- yearly_infogob %>%
    group_by(region, provincia, distrito, tipo_municipalidad, year = year(date)) %>%
    fill(fragmentation_index, max_porc_partido, numero_real_de_partidos, numero_efectivo_partidos, numero_partidos_molinar, hiperfraccionamiento, competitividad_index, gender_of_mayor, year, .direction = "downup")

# Write data
write_parquet(monthly_infogob, "data/02_intermediate/controles_infogob_mensual.parquet")
write_parquet(semestral_infogob, "data/02_intermediate/controles_infogob_semestral.parquet")
write_parquet(yearly_infogob, "data/02_intermediate/controles_infogob_anual.parquet")

## PLOTS FOR DEBUGGING AND ANALYSIS
library(waffle)

# GENDER OF MAYOR

# Unconditional bar plot
p1_gender <- genero_infogob %>%
    ggplot(aes(x = gender_of_mayor)) +
    geom_bar(fill = "steelblue") +
    labs(title = "Distribution of Mayor's Gender", x = "Mayor's Gender", y = "Frequency")

# Bar plot conditional on year
p2_gender <- genero_infogob %>%
    ggplot(aes(x = gender_of_mayor)) +
    geom_bar(fill = "steelblue") +
    facet_wrap(~year, scales = "free") +
    labs(title = "Distribution of Mayor's Gender by Year", x = "Mayor's Gender", y = "Frequency")

# Bar plot conditional on tipo_municipalidad
p3_gender <- genero_infogob %>%
    ggplot(aes(x = gender_of_mayor)) +
    geom_bar(fill = "steelblue") +
    facet_wrap(~tipo_municipalidad, scales = "free") +
    labs(title = "Distribution of Mayor's Gender by Tipo Municipalidad", x = "Mayor's Gender", y = "Frequency")

# Display the plots
p1_gender
p2_gender
p3_gender

# Load necessary libraries
library(waffle)
library(patchwork)
library(gridExtra)

# Get unique years
unique_years <- unique(genero_infogob$year)

# Empty list to store plots
plots <- list()

# Loop through each year
for (year in unique_years) {
    # Subset data for the year
    data_year <- genero_infogob %>% 
        filter(year == year) %>% 
        count(gender_of_mayor) %>% 
        mutate(percent = n / sum(n) * 100,
               percent = round(percent),
               gender_of_mayor = factor(gender_of_mayor, levels = c("Femenino", "Masculino")))
    
    # Create parts for waffle plot
    parts <- setNames(data_year$percent, data_year$gender_of_mayor)
    
    # Create waffle plot
    p <- waffle(parts, rows = 10, size = 0.5, colors = c("#FF69B4", "#1E90FF"), title = paste("Year:", year))
    
    # Add plot to list
    plots[[as.character(year)]] <- p
}

# Combine all plots
combined_plot <- wrap_plots(plots, ncol = 3, guides = "collect", widths = 1) & theme(legend.position = "right")
combined_plot

# NUMERO EFECTIVO DE PARTIDOS
p1_n_efec_partidos <- controles_infogob %>%
    ggplot(aes(x = numero_efectivo_partidos)) +
    geom_histogram(fill = "white", color = "black") +
    labs(title = "Distribution Numero Efectivo de Partidos", x = "Numero Efectivo", y = "Frequency")

# Bar plot conditional on year
p2_n_efec_partidos <- controles_infogob %>%
    ggplot(aes(x = numero_efectivo_partidos)) +
    geom_histogram(fill = "white", color = "black") +
    facet_wrap(~year, scales = "free_y") +
    labs(title = "Distribution Numero Efectivo de Partidos", x = "Numero Efectivo", y = "Frequency")

# Bar plot conditional on tipo_municipalidad
p3_n_efec_partidos <- controles_infogob %>%
    ggplot(aes(x = numero_efectivo_partidos)) +
    geom_histogram(fill = "white", color = "black") +
    facet_wrap(~tipo_municipalidad, scales = "free_y", ncol = 1) +
    labs(title = "Distribution Numero Efectivo de Partidos by Tipo Municipalidad", x = "Numero Efectivo", y = "Frequency")

# Display the plots
p1_n_efec_partidos
p2_n_efec_partidos
p3_n_efec_partidos

# COMPETITIVIDAD
p1_competitividad <- controles_infogob %>%
    ggplot(aes(x = competitividad_index)) +
    geom_histogram(fill = "white", color = "black") +
    labs(title = "Distribution Competitivity Index", x = "Competitivity Index", y = "Frequency")

# Bar plot conditional on year
p2_competitividad <- controles_infogob %>%
    ggplot(aes(x = competitividad_index)) +
    geom_histogram(fill = "white", color = "black") +
    facet_wrap(~year, scales = "free_y") +
    labs(title = "Distribution Competitivity Index", x = "Competitivity Index", y = "Frequency")

# Bar plot conditional on tipo_municipalidad
p3_competitividad <- controles_infogob %>%
    ggplot(aes(x = competitividad_index)) +
    geom_histogram(fill = "white", color = "black") +
    facet_wrap(~tipo_municipalidad, scales = "free_y", ncol = 1) +
    labs(title = "Distribution Competitivity Index by Tipo Municipalidad", x = "Competitivity Index", y = "Frequency")

# Display the plots
p1_competitividad
p2_competitividad
p3_competitividad
