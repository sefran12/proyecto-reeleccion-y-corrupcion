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
osce_infogob_oci <- read_parquet("data/03_model/osce_infogob_oci_monthly.parquet") %>% ungroup()
skimr::skim(osce_infogob_oci)

# Feature engineering
model_df <- osce_infogob_oci %>% mutate(
    cutoff = mesanho_publicacion > "2014-01-01",
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
plm_model_df <- pdata.frame(model_df %>% filter(!is.na(gobierno), !is.na(mesanho_publicacion)) %>% distinct(gobierno, mesanho_publicacion, .keep_all = TRUE),
                            index = c("gobierno", "mesanho_publicacion"))

# Indices to be analyzed
indices <- c("valor_adjudicado_mean", "tiempo_promedio", "single_bidder_projects", 
             "perc_single_bidder", "total_bidders", "avg_diff", "avg_ratio")
explanation <- c("Valor adjudicado promedio", "Tiempo promedio de publicación a buena pro",
                 "Número de proyectos con un solo postor", "Porcentaje de proyectos con u solo postor",
                 "Total de postores", "Diferencia promedio entre el valor postulado versus el valor de referencia",
                 "Ratio promedio entre el valor postulado versus el valor de referencia")

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
                        formula = reformulate("(1|gobierno) + (1|mesanho_publicacion) + 
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

