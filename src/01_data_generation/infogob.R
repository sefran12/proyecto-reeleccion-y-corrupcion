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
