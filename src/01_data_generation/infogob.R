# Stitching infogob 

library(readxl)
library(tidyverse)
library(janitor)

file_list <- dir(path = "data/01_raw/Base_de_datos_INFOgob/", full.names = TRUE)

## RESULTADOS ELECTORALES 

full_data = data.frame()
for (curr_file in file_list) {
    cat("Processing", curr_file, "\n")
    
    if (!grepl(pattern = "Resultados", curr_file)) {
        cat("Not a results file, skipping...", "\n")
        next
    }
    
    # read data
    curr_data = read_excel(curr_file)
    curr_data = janitor::clean_names(curr_data)
    
    # basic feature transformation
    curr_data$year = str_extract(curr_file, "[1-2][0-9]{3}")
    curr_data$vuelta = ifelse(grepl("2V", curr_file), "2", "1")
    
    full_data <- full_data %>% rbind(curr_data)
}

# save
write.csv(full_data, file = "data/02_intermediate/resultados_electorales_regionales_2010_2018.csv")


## AUTORIDADES REGIONALES

full_data = data.frame()
for (curr_file in file_list) {
    cat("Processing", curr_file, "\n")
    
    if (!grepl(pattern = "Autoridades", curr_file)) {
        cat("Not an elected officials file, skipping...", "\n")
        next
    }
    
    # read data
    curr_data = read_excel(curr_file)
    curr_data = janitor::clean_names(curr_data)
    if ((length(colnames(full_data)) > 0) && any(colnames(full_data) != colnames(curr_data))) {
        colnames(curr_data) = colnames(full_data)
    }
    
    # basic feature transformation
    curr_data$year = str_extract(curr_file, "[1-2][0-9]{3}")
    curr_data$vuelta = ifelse(grepl("2V", curr_file), "2", "1")
    curr_data$full_name = str_c(curr_data$prenombres, curr_data$primer_apellido, curr_data$segundo_apellido, sep = " ")
    
    full_data <- full_data %>% rbind(curr_data)
}

# save
write.csv(full_data, file = "data/02_intermediate/autoridades_electas_regionales_2010_2018.csv")

### RESULTADOS ELECTORALES DISTRITALES Y PROVINCIALES

file_list <- dir(path = "data/01_raw/Base_de_datos_INFOgob/", full.names = TRUE, recursive = TRUE)

full_data = data.frame()
for (curr_file in file_list) {
    cat("Processing", curr_file, "\n")
    
    if (!grepl(pattern = "Resultados", curr_file) | !grepl(pattern = "Provincial|Distrital", curr_file)) {
        cat("Not a results file, skipping...", "\n")
        next
    }
    
    # read data
    curr_data = read_excel(curr_file)
    curr_data = janitor::clean_names(curr_data)
    
    # basic feature transformation
    curr_data$year = str_extract(curr_file, "[1-2][0-9]{3}")
    curr_data$tipo_municipalidad = ifelse(grepl("PROVINCIAL", curr_file), "provincial", "distrital")
    if (!("distrito" %in% colnames(curr_data))) {
        curr_data$distrito = "general"
    }
    #    curr_data$vuelta = ifelse(grepl("2V", curr_file), "2", "1")
    
    full_data <- full_data %>% rbind(curr_data)
}

write.csv(full_data, file = "data/02_intermediate/resultados_electorales_provinciales_distritales_2002_2018.csv")


## AUTORIDADES ELEGIDAS DISTRITALES Y REGIONALES

full_data = data.frame()
for (curr_file in file_list) {
    cat("Processing", curr_file, "\n")
    
    if (!grepl(pattern = "Autoridades", curr_file) | !grepl(pattern = "Provincial|Distrital", curr_file)) {
        cat("Not an elected officials file, skipping...", "\n")
        next
    }
    
    # read data
    curr_data = read_excel(curr_file)
    curr_data = janitor::clean_names(curr_data)
    
    # basic feature transformation
    curr_data$year = str_extract(curr_file, "[1-2][0-9]{3}")
    curr_data$vuelta = ifelse(grepl("2V", curr_file), "2", "1")
    curr_data$full_name = str_c(
        str_replace_na(curr_data$prenombres, ""), 
        str_replace_na(curr_data$primer_apellido, ""), 
        str_replace_na(curr_data$segundo_apellido, ""), 
        sep = " ")
    curr_data$tipo_cargo = ifelse(grepl("ALCALDE", curr_file), "alcalde", "regidor")
    curr_data$tipo_municipalidad = ifelse(grepl("PROVINCIAL", curr_file), "provincial", "distrital")
    
    # column harmonization
    if ((length(colnames(full_data)) > 0) && any(colnames(full_data) != colnames(curr_data))) {
        cat("Nonmatching schema. Harmonizing: \n")
        cat(paste(colnames(curr_data), "\n"))
        cat(paste(colnames(full_data), "\n"))
        common_columns = intersect(colnames(curr_data), colnames(full_data))
        cat(paste(common_columns, "\n"))
        curr_data = curr_data[,common_columns]
        full_data = full_data[,common_columns]
    }
    
    if (!("percent_votos_obtenidos_por_la_orgpanizacion_politica" %in% colnames(curr_data))) {
        curr_data$percent_votos_obtenidos_por_la_organizacion_politica = NA
    }
    
    full_data <- full_data %>% rbind(curr_data)
}

write.csv(full_data, file = "data/02_intermediate/autoridades_electas_provinciales_distritales_2002_2018.csv")

