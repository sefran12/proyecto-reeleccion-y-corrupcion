# MODELING
library(tidyverse)
library(sjPlot)
library(lme4)
library(broom.mixed)
library(stargazer)
library(skimr)
library(lme4)
library(plm)

# load data
osce_infogob_oci <- read_parquet("data/03_model/osce_infogob_oci_semester.parquet") %>% ungroup()
colnames(osce_infogob_oci)
#skimr::skim(osce_infogob_oci)

# Feature engineering
model_df <- osce_infogob_oci %>% mutate(
    cutoff = semester > "2014-01-01",
    fragmentation_category = case_when(fragmentation_index < 0 ~ "<0",
                                       fragmentation_index < 0.25 ~ "<0.25",
                                       fragmentation_index < 0.5 ~ "<0.5",
                                       fragmentation_index < 0.75 ~ "<0.75",
                                       fragmentation_index > 0.75 ~ ">0.75",
                                       is.na(fragmentation_index) ~ "No disponible"),
    competitividad_category = case_when(competitividad_index < 1 ~ "<1",
                                        competitividad_index < 2 ~ "<2",
                                        competitividad_index < 3 ~ "<3",
                                        competitividad_index < 4 ~ "<4",
                                        competitividad_index > 5 ~ ">5",
                                        is.na(competitividad_index) ~ "No disponible")
)

# make plm dataframe for panel models
plm_model_df <- pdata.frame(model_df %>% filter(!is.na(gobierno), !is.na(semester)) %>% distinct(gobierno, semester, .keep_all = TRUE),
                            index = c("gobierno", "semester"))

# Indices to be analyzed
indices <- c("repeated_postores", "total_postores", "perc_repeated_postores", 
             "concentration_at_3", "concentration_at_5", "coberture_at_80",
             "perc_coberture_at_80")
explanation <- c("Número de postores repetidos semestre a semestre",
                 "Total de postores",
                 "Porcentaje de postores repetidos semestra a semestre",
                 "Indice de concentracion en los tres mayores proyectos",
                 "Indice de concentracion en los cinco mayores proyectos", 
                 "Número de proyectos que cubren el 80% del valor total, de mayor a menor",
                 "Porcentaje de los proyectos que cubren el 80% del valor total, de mayor a menor")

# Initialize a list to store the results
result_list <- list()

# Loop over each index
for (i in seq_along(indices)) {
    index <- indices[i]
    print(paste0("Processing ", index, " ..."))
    
    # Linear model
    linear_model <- lm(data = model_df, 
                       formula = reformulate("cutoff +
                      fragmentation_category + competitividad_category + 
                      gender_of_mayor + OCI_exists_any + OCI_incorporated_any", 
                      response = index))
    
    # Mixed model on panel info
    mixed_model <- lmer(data = model_df, 
                        formula = reformulate("(1|gobierno) + (1|semester) + 
                      cutoff + fragmentation_category + competitividad_category + 
                      gender_of_mayor + OCI_exists_any + OCI_incorporated_any", 
                      response = index))
    
    # Panel data, within on individual
    individual_within_model <- plm(data = plm_model_df, 
                                   formula = reformulate("cutoff +
                                  fragmentation_category + competitividad_category +
                                  gender_of_mayor + OCI_exists_any + OCI_incorporated_any", 
                                  response = index),
                                  model = "within", effect = "individual")
    
    # Capture the tab_model output, using the corresponding explanation as the title
    model_table <- tab_model(linear_model, mixed_model, individual_within_model, 
                             title = explanation[i])
    result_list[[index]] <- model_table
    
    print(paste0("Completed ", index))
}

# Save each model table to an HTML file
for (i in seq_along(result_list)) {
    index <- indices[i]
    sink(paste0("data/04_tables/", index, "_model.html"))
    cat(result_list[[index]]$page.complete)
    sink()
}
