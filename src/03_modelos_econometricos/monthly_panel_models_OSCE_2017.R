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
osce_infogob_oci <- read_parquet("data/03_model/osce_infogob_oci_monthly_2017.parquet") %>% ungroup()
colnames(osce_infogob_oci)

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
indices <- c("desvicacion_respecto_al_promedio_anual", "tiempo_promedio", "single_bidder_projects", 
             "perc_single_bidder", "total_bidders", "avg_diff_between_winner_and_publicado", "avg_ratio_between_winner_and_publicado")
explanation <- c("Desviación del Valor adjudicado promedio respecto al año anterior",
                 "Tiempo promedio de publicación a buena pro",
                 "Número de proyectos con un solo postor", "Porcentaje de proyectos con un solo postor",
                 "Total de postores", "Diferencia promedio entre el valor postulado versus el valor de referencia",
                 "Ratio promedio entre el valor postulado versus el valor de referencia")

# Initialize a list to store the results for each objeto
result_list_objeto <- list()
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
        
        # Linear model
        linear_model <- lm(data = subset_model_df, 
                           formula = reformulate("cutoff + fragmentation_category + competitividad_category + 
                          gender_of_mayor + OCI_exists_any + OCI_incorporated_any", 
                          response = index))
        
        # Mixed model on panel info
        mixed_model <- lmer(data = subset_model_df, 
                            formula = reformulate("(1|gobierno) + (1|mesanho_publicacion) + 
                          cutoff  + fragmentation_category + competitividad_category + 
                          gender_of_mayor + OCI_exists_any + OCI_incorporated_any", 
                          response = index))
        
        # Panel data, within on individual
        individual_within_model <- plm(data = subset_plm_model_df, 
                                       formula = reformulate("cutoff + fragmentation_category + competitividad_category +
                                      gender_of_mayor + OCI_exists_any + OCI_incorporated_any", 
                                      response = index),
                                      model = "within", effect = "individual")
        
        # Capture the tab_model output, using the corresponding explanation as the title
        model_table <- tab_model(linear_model, mixed_model, individual_within_model, 
                                 title = paste0(explanation[i], " for ", objeto))
        result_list_objeto[[paste0(index, "_", objeto)]] <- model_table
        
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
