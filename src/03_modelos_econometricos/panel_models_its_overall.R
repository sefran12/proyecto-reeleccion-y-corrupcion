library(huxtable)
library(plm)
library(purrr)

indices <- c(
    "avg_bidders_per_project_original","avg_ratio_between_winner_and_publicado_original",
    "perc_single_bidder_original", "perc_repeated_winners_original", "perc_proyectos_debajo_cutoff_original",
    "perc_proyectos_sobrecosto_original", "perc_valor_adj_del_total_original")
explanation <- c("Promedio de postores por proyecto")
tipo_municipalidad <- unique(model_df$tipo_municipalidad)

result_models_overall <- list()
for (i in seq_along(indices)) {
    index <- indices[i]
    print(paste0("Processing ", index, " for ", tipo_municipalidad, " municipalidad", " ..."))
    
    subset_model_df <- model_df
    subset_plm_model_df <- plm_model_df %>% mutate(perc_repeated_winners_original = perc_repeated_winners_original/100)
    
    individual_within_model <- plm(data = subset_plm_model_df, formula = reformulate(
        "cutoff + cutoff:time_var + time_var + 
        numero_efectivo_partidos + competitividad_category +
        gender_of_mayor + OCI_exists_any + percentage_canon_category +
        month_publicacion", 
        response = index),
        model = "within",
        effect = "individual"
    )
    
    individual_within_model$model_name <- index
    result_models_overall[[index]] <- individual_within_model
}

library(lmtest)
library(sandwich)

# Create huxtables and set common statistic labels
hux_tables <- map(result_models_overall, function(x) {
    summary_x <- coeftest(x, type = "HC1", save = TRUE)
    huxreg(summary_x, error_format = "({std.error})", number_format = "%.3f")
})

# Combine huxtables side by side
final_table <- reduce(hux_tables, add_columns)

# Function to check if a cell contains a number and, if it does, round it to 4 decimal places
round_if_number <- function(x) {
    # Check if parentheses are present
    has_parentheses <- grepl("\\((.*?)\\)", x)
    # Count number of asterisks
    num_asterisks <- sum(strsplit(x, NULL)[[1]] == "*")
    
    # Remove parentheses if present
    x <- gsub("\\((.*?)\\)", "\\1", x)
    # Remove asterisks if present
    x <- gsub("\\*", "", x)
    
    if (!suppressWarnings(is.na(as.numeric(x)))) {
        # Round the number to 4 decimal places
        x <- round(as.numeric(x), 4)
        
        x <- as.character(x)
        
        # Add back parentheses if they were originally present
        if (has_parentheses) {
            x <- paste0("(", x, ")")
        }
        
        # Add back the same number of asterisks if they were originally present
        if (num_asterisks > 0) {
            x <- paste0(x, strrep("*", num_asterisks))
        }
    }
    
    return(x)
}

# Apply function to each cell
final_table[] <- apply(final_table, c(1,2), round_if_number)

# Convert each cell to character and prepend a single quote
final_table[] <- apply(final_table, c(1,2), function(x) paste0("'", as.character(x)))

final_table <- final_table %>% select(!starts_with("names.")) %>%
    `colnames<-`(c("Variable", explanation))

# Write the table to a CSV file
write.csv(final_table, file = "results.csv", row.names = FALSE, quote = FALSE)


result_plots_overall <- list()
for (i in seq_along(indices)) {
    index <- indices[i]
    print(paste0("Processing ", index, " for ", tipo_municipalidad, " municipalidad", " ..."))
    
    subset_model_plot <- subset_plm_model_df %>% 
        group_by(mesanho_publicacion = as.Date(mesanho_publicacion)) %>% 
        summarise(
            mean_indices = mean(get(index), na.rm = TRUE)
        ) %>% 
        ggplot(aes(x = mesanho_publicacion, y = mean_indices)) +
        geom_line(alpha = 1) +
        #    geom_smooth(method = "lm", se = FALSE, color = "black") +
        stat_smooth(data = . %>% filter(mesanho_publicacion <= as.Date("2015-03-01")),
                    method = "lm", se = FALSE, color = "blue", fullrange = TRUE) +
        geom_smooth(data = . %>% filter(mesanho_publicacion > as.Date("2015-03-01")),
                    method = "lm", se = FALSE, color = "red") +
        geom_vline(xintercept = as.Date("2015-03-01")) +
        labs(x = NULL,
             y = paste(index)) #+
        #facet_wrap(~tipo_municipalidad, ncol = 1, scales = "free_y")
    
    result_plots_overall[[index]] <- subset_model_plot
    print(subset_model_plot)
}
result_plots_overall[[7]]
