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
mef_infogob_oci <- read_parquet("data/03_model/mef_infogob_oci_anual.parquet") %>% ungroup()
colnames(mef_infogob_oci)
#skimr::skim(mef_infogob_oci)

# Feature engineering
model_df <- mef_infogob_oci %>% mutate(
    cutoff = date > "2014-01-01",
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
                                        is.na(competitividad_index) ~ "No disponible"),
    gender_of_mayor = replace_na(gender_of_mayor, "unknown")
)

# make plm dataframe for panel models
plm_model_df <- pdata.frame(model_df %>% filter(!is.na(gobierno), !is.na(date)) %>% distinct(gobierno, date, .keep_all = TRUE),
                            index = c("gobierno", "date"))

# Indices to be analyzed
indices <- c("perc_proyectos_debajo_cutoff", "num_proyectos_debajo_cutoff", "perc_proyectos_sobrecosto", 
             "total_pia_value_proyectos_debajo_cutoff")
explanation <- c("Porcentaje de proyectos debajo del corte OSCE",
                 "Número de proyectos debajo del corte OSCE",
                 "Porcentaje de proyectos con sobrecosto",
                 "Total del PIA de los proyectos debajo del corte OSCE")

# Calculate models
result_list <- list()
tipo_bien <- c("Total")

for (i in seq_along(indices)) {
    # Initialize a list to store the results for each objeto
    result_list_objeto <- list()
    
    for (objeto in tipo_bien) {
        index <- indices[i]
        print(paste0("Processing ", index, " for ", objeto, " ..."))
        
        # Subset the data for the current objeto
        subset_model_df <- model_df
        subset_plm_model_df <- plm_model_df
        
        # Try to fit each model, and print a message if an error occurs
        linear_model <- tryCatch({
            lm(data = subset_model_df, 
               formula = reformulate("cutoff + fragmentation_category + competitividad_category + 
              gender_of_mayor + OCI_exists + OCI_incorporated", 
              response = index))
        }, error = function(e) {
            print(paste0("Error in linear model for index ", index, ", objeto ", objeto, ": ", e$message))
            NULL
        })
        
        mixed_model <- tryCatch({
            lmer(data = subset_model_df, 
                 formula = reformulate("(1|gobierno) + (1|date) + 
              cutoff  + fragmentation_category + competitividad_category + 
              gender_of_mayor + OCI_exists + OCI_incorporated", 
              response = index))
        }, error = function(e) {
            print(paste0("Error in mixed model for index ", index, ", objeto ", objeto, ": ", e$message))
            NULL
        })
        
        individual_within_model <- tryCatch({
            plm(data = subset_plm_model_df, 
                formula = reformulate("cutoff + fragmentation_category + competitividad_category +
              gender_of_mayor + OCI_exists + OCI_incorporated", 
              response = index),
              model = "within", effect = "individual")
        }, error = function(e) {
            print(paste0("Error in individual within model for index ", index, ", objeto ", objeto, ": ", e$message))
            NULL
        })
        
        # Capture the tab_model output, using the corresponding explanation as the title
        
        # Initialize a list to store the models
        models <- list()
        
        if (!is.null(linear_model)) {
            models$linear_model <- linear_model
        }
        
        if (!is.null(mixed_model)) {
            models$mixed_model <- mixed_model
        }
        
        if (!is.null(individual_within_model)) {
            models$individual_within_model <- individual_within_model
        }
        
        # Capture the tab_model output, using the corresponding explanation as the title
        model_table <- tryCatch({
            if(length(models) == 3){
                tab_model(models$linear_model, models$mixed_model, models$individual_within_model, title = paste0(explanation[i], " for ", objeto))
            } else if(length(models) == 2){
                names <- names(models)
                tab_model(models[[names[1]]], models[[names[2]]], title = paste0(explanation[i], " for ", objeto))
            } else if(length(models) == 1){
                names <- names(models)
                tab_model(models[[names[1]]], title = paste0(explanation[i], " for ", objeto))
            }
        }, error = function(e) {
            print(paste0("Error in tab_model for index ", index, ", objeto ", objeto, ": ", e$message))
            NULL
        })
        
        if (!is.null(model_table)) {
            result_list_objeto[[paste0(index, "_", objeto)]] <- model_table
        }
        
        
        
        print(paste0("Completed ", index, " for ", objeto))
    }
    
    # Combine the tables for each objeto into a single table for the current index
    combined_table <- do.call("paste", lapply(result_list_objeto, function(x) x$page.complete))
    result_list[[index]] <- list(page.content = combined_table)
}

# Save each combined model table to an HTML file
for (i in seq_along(result_list)) {
    index <- indices[i]
    sink(paste0("data/04_tables/", index, "_model.html"))
    cat(result_list[[index]]$page.content)
    sink()
}

