# Cargamos las bibliotecas necesarias
library(readxl)
library(stringr)
library(janitor)
library(tidyverse)
library(arrow)

# Creamos un data frame vacío para almacenar los datos
controles_infogob <- data.frame()

file_list <- dir(path = "data/01_raw/Base_de_datos_INFOgob/", full.names = TRUE, recursive = TRUE)

# Iteramos sobre cada archivo en la lista
for (curr_file in file_list) {
    cat("Processing", curr_file, "\n")
    
    if (!str_detect(curr_file, "Resultados|RESU|resu") | !str_detect(curr_file, "Provincial|Distrital|PROV|DIST|prov|dist")) {
        cat("Not a results file, skipping...", "\n")
        next
    }
    
    # Leemos los datos
    curr_data <- read_excel(curr_file)
    curr_data <- clean_names(curr_data)
    
    # Transformamos las características
    curr_data$year <- str_extract(curr_file, "[1-2][0-9]{3}")
    curr_data$tipo_municipalidad <- if_else(str_detect(curr_file, "PROVINCIAL"), "provincial", "distrital")
    if (!("distrito" %in% colnames(curr_data))) {
        curr_data$distrito <- "general"
    }
    
    # Calculamos el índice de fragmentación por entidad y ciclo
    curr_data <- curr_data %>%
        group_by(region, provincia, distrito, year, tipo_municipalidad) %>%
        mutate(percent_votos_sq = percent_votos^2) %>%
        mutate(winning_percent_votos_sq = max(percent_votos, na.rm = TRUE)^2) %>%
        summarise(fragmentation_index = 1 - sum(percent_votos_sq, na.rm = TRUE),
                  competitividad_index = 1 + (
                      (1 / sum(percent_votos_sq, na.rm = TRUE)) * (
                          (sum(percent_votos_sq, na.rm = TRUE) - winning_percent_votos_sq) /
                              winning_percent_votos_sq
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
    
    if (!str_detect(curr_file, "Autor|AUTOr|autor") | !str_detect(curr_file, "Provincial|Distrital|PROV|DIST|prov|dist")) {
        cat("Not an autoridades file, skipping...", "\n")
        next
    }
    
    # Leemos los datos
    curr_data <- read_excel(curr_file)
    curr_data <- clean_names(curr_data)
    
    # If the distrito column does not exist, create it and fill with "general"
    if (!"distrito" %in% colnames(curr_data)) {
        curr_data$distrito <- "general"
    }
    
    # Filter data for elected mayors
    curr_data <- curr_data %>%
        filter(cargo_electo == "ALCALDE PROVINCIAL" | cargo_electo == "ALCALDE DISTRITAL") %>%
        mutate(year = str_extract(curr_file, "[1-2][0-9]{3}"),
               tipo_municipalidad = if_else(str_detect(curr_file, "PROVINCIAL"), "provincial", "distrital"))
    
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
controles_infogob <- controles_infogob %>%
    left_join(genero_infogob, by = c("region", "provincia", "distrito", "tipo_municipalidad", "date"))

# Function to create expanded datasets
create_panel_infogob <- function(dates){
    panel <- controles_infogob %>%
        tidyr::expand(nesting(region, provincia, distrito, tipo_municipalidad), date = dates) %>%
        left_join(controles_infogob, by = c("region", "provincia", "distrito", "tipo_municipalidad", "date")) %>%
        fill(fragmentation_index, competitividad_index, gender_of_mayor, .direction = "down")
    return(panel)
}

# Generate list of all first dates of the months and semesters within the range
monthly_dates <- seq(as.Date("2002-01-01"), as.Date("2022-12-01"), by = "month")
semestral_dates <- seq(as.Date("2002-01-01"), as.Date("2022-07-01"), by = "6 months")

# Create expanded datasets
monthly_infogob <- create_panel_infogob(monthly_dates)
semestral_infogob <- create_panel_infogob(semestral_dates)

# 'fill' yearly data to each month within the same year
monthly_infogob <- monthly_infogob %>%
    group_by(region, provincia, distrito, tipo_municipalidad, year(date)) %>%
    fill(fragmentation_index, competitividad_index, gender_of_mayor, .direction = "downup")

# 'fill' yearly data to each semester within the same year
semestral_infogob <- semestral_infogob %>%
    group_by(region, provincia, distrito, tipo_municipalidad, year(date), semester(date)) %>%
    fill(fragmentation_index, competitividad_index, gender_of_mayor, .direction = "downup")

# Write data
write_parquet(monthly_infogob, "data/02_intermediate/controles_infogob_mensual.parquet")
write_parquet(semestral_infogob, "data/02_intermediate/controles_infogob_semestral.parquet")
