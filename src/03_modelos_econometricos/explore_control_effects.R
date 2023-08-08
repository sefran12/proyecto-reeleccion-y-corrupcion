# Import required libraries
library(minerva)
library(parallel)
library(DescTools)  # for CramersV function
library(psych)  # for Spearman's rank correlation and its p-value

calculate_association <- function(df, controls, indices) {
    
    # Generate full column names for each control variable
    control_cols <- c()
    for (control in controls) {
        control_cols <- c(control_cols, paste0(control, "_value"), paste0(control, "_pval"))
    }
    
    # Initialize empty dataframe to store results
    association_results <- data.frame(matrix(ncol = length(control_cols) + 1, nrow = 0))
    
    # Set column names
    colnames(association_results) <- c("Index", control_cols)
    
    for (index in indices) {
        cat(paste0("\n\nCalculating associations for ", index, "...\n"))
        
        # Initialize row to store results for current index
        row <- data.frame(matrix(ncol = length(control_cols) + 1, nrow = 1))
        colnames(row) <- colnames(association_results) # match column names with results dataframe
        row$Index <- index
        
        for (control in controls) {
            cat(paste0("\nControl variable: ", control, "\n"))
            
            # Check if control variable is numerical
            if (is.numeric(df[[control]])) {
                # Remove NA and infinite values
                df_clean <- df[!is.na(df[[control]]) & !is.na(df[[index]]) & is.finite(df[[control]]) & is.finite(df[[index]]), ]
                cat("\nRemoved NA and infinite values for numeric columns\n")
                cat("\nCalculating Spearman's rank correlation...\n")
                # Calculate Spearman's rank correlation and save value
                cor_result <- cor.test(df_clean[[index]], df_clean[[control]], method="spearman")
                row[,paste0(control, "_value")] <- round(cor_result$estimate, 4)
                row[,paste0(control, "_pval")] <- round(cor_result$p.value, 4)
                cat("\nSpearman's rank correlation calculated\n")
            } else {
                # Remove NA values
                df_clean <- df[!is.na(df[[control]]) & !is.na(df[[index]]), ]
                
                # Calculate eta-squared
                model <- aov(df_clean[[index]] ~ df_clean[[control]])
                anova_table <- summary(model)
                print(anova_table)
                effect_ss <- anova_table[[1]]["Sum Sq"][[1]][1]
                residual_ss <- anova_table[[1]]["Sum Sq"][[1]][2]
                print(effect_ss)
                
                eta_sq <- effect_ss / (effect_ss + residual_ss)
                
                # Add result to row
                row[,paste0(control, "_value")] <- round(eta_sq, 4)
                
                # p-value for the F-test in the ANOVA model
                row[,paste0(control, "_pval")] <- round(summary(model)[[1]][["Pr(>F)"]][1], 4)
            }
        }
        
        # Add asterisks for significance level
        for (control in controls) {
            col <- paste0(control, "_value")
            pval_col <- paste0(control, "_pval")
            if (!pval_col %in% colnames(row)) {
                next
            }
            
            pval <- row[,pval_col]
            if (pval < 0.01) {
                row[,col] <- paste0(row[,col], "***")
            } else if (pval < 0.05) {
                row[,col] <- paste0(row[,col], "**")
            } else if (pval < 0.1) {
                row[,col] <- paste0(row[,col], "*")
            }
        }
        cat("\nAdded asterisks for significance level\n")
        
        # Append row to results dataframe
        association_results <- rbind(association_results, row)
        cat("\nRow added to the result dataframe\n")
    }
    
    return(association_results)
}

# Define list of control variables
controls <- c("numero_efectivo_partidos", "competitividad_index", 
              "gender_of_mayor", "OCI_exists_any", "percentage_canon")

# Define list of indices
indices <- c("mean_indices", "F1", "F2")

# Calculate association
association_results <- calculate_association(model_df %>% mutate(OCI_exists_any = as.character(OCI_exists_any)), controls, indices)

association_results_long <- association_results %>% 
    mutate_all(as.character) %>% 
    pivot_longer(-Index, names_to = "name", values_to = "value") %>%
    mutate(control = str_replace(name, "_pval|_value", ""),
           measure = ifelse(str_detect(name, "_pval"), "pval", "value")) %>%
    select(-name) %>%
    pivot_wider(names_from = measure, values_from = value)

print(association_results_long)
association_results_long %>% write_clip()


# Define list of control variables
controls <- c("numero_efectivo_partidos", "competitividad_index",
              "gender_of_mayor", "OCI_exists_any", "percentage_canon")

# Define list of indices
indices <- c("avg_bidders_per_project_original","avg_ratio_between_winner_and_publicado_original",
             "perc_single_bidder_original", "perc_repeated_winners_original", "perc_proyectos_debajo_cutoff_original",
             "perc_proyectos_sobrecosto_original", "perc_valor_adj_del_total_original")

# Calculate association
association_results <- calculate_association(model_df %>% mutate(OCI_exists_any = as.character(OCI_exists_any)), controls, indices)

association_results_long <- association_results %>% 
    mutate_all(as.character) %>% 
    pivot_longer(-Index, names_to = "name", values_to = "value") %>%
    mutate(control = str_replace(name, "_pval|_value", ""),
           measure = ifelse(str_detect(name, "_pval"), "pval", "value")) %>%
    select(-name) %>%
    pivot_wider(names_from = measure, values_from = value)

print(association_results_long)
association_results_long %>% write_clip()
