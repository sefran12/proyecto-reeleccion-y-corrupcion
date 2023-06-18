# MODELING
library(tidyverse)
library(sjPlot)
library(lme4)
library(broom.mixed)
library(stargazer)
library(skimr)
library(lme4)
library(arrow)
library(plm)

# load data
osce_infogob_oci <- read_parquet("data/03_model/osce_infogob_oci_semester_2017.parquet") %>% ungroup()
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
                                        is.na(competitividad_index) ~ "No disponible"),
    gender_of_mayor = replace_na(gender_of_mayor, "unknown")
)

# make plm dataframe for panel models
plm_model_df <- pdata.frame(model_df %>% filter(!is.na(gobierno), !is.na(semester)) %>% distinct(gobierno, semester, .keep_all = TRUE),
                            index = c("gobierno", "semester"))

# Indices to be analyzed
indices <- c("repeated_postores", "total_postores", "perc_repeated_postores", 
             "repeated_winners", "perc_repeated_winners", "num_projects")
explanation <- c("Número de postores repetidos semestre a semestre",
                 "Total de postores",
                 "Porcentaje de postores repetidos semestra a semestre",
                 "Número de ganadores repetidos semestre a semestre",
                 "Porcentaje de ganadores repetidos semestra a semestre", 
                 "Número de proyectos que cubren el 80% del valor total, de mayor a menor")

# Calculate models
result_list <- list()
tipo_bien <- unique(plm_model_df$OBJETO)

for (i in seq_along(indices)) {
    # Initialize a list to store the results for each objeto
    result_list_objeto <- list()
    
    for (objeto in tipo_bien) {
        index <- indices[i]
        print(paste0("Processing ", index, " for ", objeto, " ..."))
        
        # Subset the data for the current objeto
        subset_model_df <- model_df %>% filter(OBJETO == objeto)
        subset_plm_model_df <- plm_model_df %>% filter(OBJETO == objeto)
        
        # Try to fit each model, and print a message if an error occurs
        linear_model <- tryCatch({
            lm(data = subset_model_df, 
               formula = reformulate("cutoff + fragmentation_category + competitividad_category + 
              gender_of_mayor + OCI_exists_any + OCI_incorporated_any", 
              response = index))
        }, error = function(e) {
            print(paste0("Error in linear model for index ", index, ", objeto ", objeto, ": ", e$message))
            NULL
        })
        
        mixed_model <- tryCatch({
            lmer(data = subset_model_df, 
                 formula = reformulate("(1|gobierno) + (1|semester) + 
              cutoff  + fragmentation_category + competitividad_category + 
              gender_of_mayor + OCI_exists_any + OCI_incorporated_any", 
              response = index),
              control = lmerControl(standardize.X = TRUE))
        }, error = function(e) {
            print(paste0("Error in mixed model for index ", index, ", objeto ", objeto, ": ", e$message))
            NULL
        })
        
        individual_within_model <- tryCatch({
            plm(data = subset_plm_model_df, 
                formula = reformulate("cutoff + fragmentation_category + competitividad_category +
              gender_of_mayor + OCI_exists_any + OCI_incorporated_any", 
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

