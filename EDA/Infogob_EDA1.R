library(readxl)
library(tidyverse)
library(janitor)

file_list <- dir(path = "data/01_raw/Base_de_datos_INFOgob/", full.names = TRUE)

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

full_data %>% 
    skimr::skim()

full_data %>% 
    count(tipo_organizacion_politica) %>% 
    arrange(desc(n))

full_data %>% 
    count(organizacion_politica) %>% 
    arrange(desc(n))

full_data %>% 
    group_by(organizacion_politica) %>% 
    summarise(
        n_elecciones = n_distinct(year),
        n_regiones = n_distinct(region),
        mean_percent_votos = mean(percent_votos),
        max_percent_votos = max(percent_votos),
        min_percent_votos = min(percent_votos)
    ) %>% 
    arrange(desc(n_regiones + n_elecciones), desc(mean_percent_votos)) %>% 
    print(n = Inf)


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

oficiales_2018 <- read_excel("data/01_raw/Base_de_datos_INFOgob/REGIONAL 2018 - 2DA VUELTA_ERM2018-2V_Autoridades_Regional.xlsx ")
oficiales_2018 <- clean_names(oficiales_2018)

oficiales_2014 <- read_excel("data/01_raw/Base_de_datos_INFOgob/REGIONAL 2014 - 2DA VUELTA_ERM2014-2V_Autoridades_Regional.xlsx ")
oficiales_2014 <- clean_names(oficiales_2014)

colnames(oficiales_2014)
colnames(oficiales_2018)

full_data %>% 
    count(full_name) %>% 
    arrange(desc(n))

full_data %>% 
    count(organizacion_politica) %>% 
    arrange(desc(n))

full_data %>% 
    count(organizacion_politica, region) %>% 
    arrange(desc(n))


### FULL DATA

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


## AUTORIDADES PROVINCIALES Y DISTRITALES

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
    if ((length(colnames(full_data)) > 0) && any(colnames(full_data) != colnames(curr_data))) {
        colnames(curr_data) = colnames(full_data)
    }
    
    # basic feature transformation
    curr_data$year = str_extract(curr_file, "[1-2][0-9]{3}")
    curr_data$vuelta = ifelse(grepl("2V", curr_file), "2", "1")
    curr_data$full_name = str_c(curr_data$prenombres, curr_data$primer_apellido, curr_data$segundo_apellido, sep = " ")
    curr_data$tipo_municipalidad = ifelse(grepl("PROVINCIAL", curr_file), "provincial", "distrital")
    
    if (!("percent_votos_obtenidos_por_la_organizacion_politica" %in% colnames(curr_data))) {
        curr_data$percent_votos_obtenidos_por_la_organizacion_politica = NA
    }
    
    full_data <- full_data %>% rbind(curr_data)
}

# save
write.csv(full_data, file = "data/02_intermediate/autoridades_electas_regionales_2010_2018.csv")
colnames(full_data)
colnames(curr_data)
