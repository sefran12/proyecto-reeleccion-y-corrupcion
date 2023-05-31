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
                  competitividad_index = - 1 * (1 + (1 / sum(percent_votos_sq, na.rm = TRUE)) * (
                      sum(percent_votos_sq - winning_percent_votos_sq, na.rm = TRUE) /
                          sum(winning_percent_votos_sq, na.rm = TRUE))
                  ),
                  .groups = "drop")
    
    # Unimos los datos al data frame completo
    controles_infogob <- bind_rows(controles_infogob, curr_data)
}

# Vuelve panel 2002:2022


controles_infogob <- controles_infogob %>%
    mutate(year = as.integer(year)) %>% # convert year to integer
    group_by(region, provincia, distrito, tipo_municipalidad) %>% # group by entity
    complete(year = 2002:2022, fill = list(fragmentation_index = NA, competitividad_index = NA)) %>% # create missing years
    fill(fragmentation_index, competitividad_index, .direction = "down") # fill forward by each entity

# Write data
write_parquet(controles_infogob, "data/02_intermediate/controles_infogob.parquet")


# Make monthly and semestral versions
monthly_dates <- seq(as.Date("2002-01-01"), as.Date("2022-12-01"), by = "month")
semestral_dates <- seq(as.Date("2002-01-01"), as.Date("2022-07-01"), by = "6 months")

# Function to generate panel data
create_panel_infogob <- function(dates){
    panel <- controles_infogob %>% 
        tidyr::uncount(length(dates)/length(unique(controles_infogob$year)), .id = "date_id") %>% 
        mutate(date = dates[date_id]) %>% 
        select(-date_id)
    return(panel)
}

# Create monthly and semestral panel data
monthly_infogob <- create_panel_infogob(monthly_dates)
semestral_infogob <- create_panel_infogob(semestral_dates)

# Write data
write_parquet(monthly_infogob, "data/02_intermediate/controles_infogob_mensual.parquet")
write_parquet(semestral_infogob, "data/02_intermediate/controles_infogob_semestral.parquet")

