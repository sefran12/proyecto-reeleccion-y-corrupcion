library(tidyverse)
library(huxtable)
library(plm)
library(officer)
library(flextable)
library(lmtest)
library(purrr)

# Function to create huxtable
create_hux_table <- function(model) {
    summary_model <- coeftest(model, type = "HC1", save = TRUE)
    hux_table <- huxreg(summary_model, error_format = "({std.error})", number_format = "%.3f")
    return(hux_table)
}

# Function to round numbers
round_if_number <- function(x) {
    has_parentheses <- grepl("\\((.*?)\\)", x)
    num_asterisks <- sum(strsplit(x, NULL)[[1]] == "*")
    x <- gsub("\\((.*?)\\)", "\\1", x)
    x <- gsub("\\*", "", x)
    
    if (!suppressWarnings(is.na(as.numeric(x)))) {
        x <- round(as.numeric(x), 4)
        x <- as.character(x)
        
        if (has_parentheses) {
            x <- paste0("(", x, ")")
        }
        
        if (num_asterisks > 0) {
            x <- paste0(x, strrep("*", num_asterisks))
        }
    }
    return(x)
}

# Function to run generalized model
run_generalized_model <- function(df, index, model = "within", effect = "individual", controls = "full") {
    print(paste0("Processing ", index, " for ", model, " model..."))
    formula_string <- switch(controls,
                             "no_controls" = "cutoff + cutoff:time_var + time_var",
                             "month_publicacion" = "cutoff + cutoff:time_var + time_var + month_publicacion",
                             "full" = "cutoff + cutoff:time_var + time_var + month_publicacion + 
                             numero_efectivo_partidos_category + competitividad_category + 
                             gender_of_mayor + OCI_exists_any + percentage_canon_category",
                             "cutoff + cutoff:time_var + time_var")
    
    if (model == "pooling") {
        model <- plm(data = df, 
                     formula = reformulate(formula_string, response = index),
                     model = model)
    } else {
        model <- plm(data = df, 
                     formula = reformulate(formula_string, response = index),
                     model = model,
                     effect = effect)
    }
    
    model$model_name <- index
    return(model)
}

# Main function for generalized experiments
run_all_experiments <- function(df, indices, model = "within") {
    results <- list()
    control_var_specs <- c("no_controls", "month_publicacion", "full")
    for (control_var_spec in control_var_specs) {
        print(paste0("Running experiment for ", control_var_spec, " control variable specification..."))
        models <- list()
        for (index in indices) {
            model <- run_generalized_model(df, index, controls = control_var_spec)
            models[[index]] <- model
        }
        
        hux_tables <- lapply(models, create_hux_table)
        final_table <- do.call(cbind, hux_tables)
        final_table[] <- apply(final_table, c(1,2), round_if_number)
        final_table[] <- apply(final_table, c(1), function(x) paste0("'", as.character(x)))
        final_table <- final_table %>% select(!starts_with("names.")) 
        colnames(final_table) <- c("name", indices)
        # Assign experiment name to the final table
        final_table <- set_caption(final_table, control_var_spec)
        
        results[[control_var_spec]] <- list(models, final_table)
    }
    return(results)
}


# Run the experiments
latent_cols <- c("mean_indices", "F1", "F2")
concrete_indices_cols <- c("avg_bidders_per_project_original","avg_ratio_between_winner_and_publicado_original",
                           "perc_single_bidder_original", "perc_repeated_winners_original", "perc_proyectos_debajo_cutoff_original",
                           "perc_proyectos_sobrecosto_original", "perc_valor_adj_del_total_original")
results_latent_factor <- run_all_experiments(model_df, indices = latent_cols)
results_concrete_indices <- run_all_experiments(model_df, indices = concrete_indices_cols)

results_latent_factor$full[[2]] %>% write_clip()
results_concrete_indices$full[[2]] %>% write_clip()

results_latent_factor$full[[2]] %>% huxtable::quick_docx(file = "latent_regs.docx")
