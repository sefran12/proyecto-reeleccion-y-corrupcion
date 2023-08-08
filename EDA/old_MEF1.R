# Filter the data into local and non-local governments
df_gastos_local <- df_gastos %>% 
    filter(NIVEL_GOBIERNO_NOMBRE %in% c("GOBIERNOS REGIONALES", "GOBIERNOS LOCALES"))
df_gastos_nonlocal <- df_gastos 

compute_indices <- function(df) {
    overspending_index <- df %>%
        group_by(ANO_EJE) %>%
        summarise(Overspending = mean(ifelse(MONTO_EJECUCION > MONTO_PIM, 1, 0), na.rm = TRUE), .groups = "drop")
    
    underimplementation_index <- df %>%
        group_by(ANO_EJE) %>%
        summarise(UnderImplementation = sum(MONTO_PIA, na.rm = TRUE) / sum(MONTO_EJECUCION, na.rm = TRUE), .groups = "drop")
    
    concentration_index <- df %>%
        group_by(ANO_EJE) %>%
        summarise(Concentration = max(MONTO_EJECUCION, na.rm = TRUE) / sum(MONTO_EJECUCION, na.rm = TRUE), .groups = "drop")
    
    funding_diversity_index <- df %>%
        group_by(ANO_EJE) %>%
        summarise(FundingDiversity = n_distinct(FUENTE_NOMBRE), .groups = "drop")
    
    excessive_variation_index <- df %>%
        group_by(ANO_EJE, GENERICA_NOMBRE, SUBGENERICA_NOMBRE, ESPECIFICA_NOMBRE) %>%
        summarise(Variation = sd(MONTO_EJECUCION, na.rm = TRUE), .groups = "drop")
    
    execution_speed_index <- df %>%
        group_by(ANO_EJE, GENERICA_NOMBRE, SUBGENERICA_NOMBRE, ESPECIFICA_NOMBRE) %>%
        summarise(ExecutionSpeed = sum(MONTO_EJECUCION, na.rm = TRUE) / n_distinct(MES_EJE), .groups = "drop")
    
    transaction_type_index <- df %>%
        group_by(ANO_EJE) %>%
        summarise(TransactionType = sum(ifelse(TIPO_TRANSACCION == "Gastos", 1, 0), na.rm = TRUE) / n(), .groups = "drop")
    
    frequent_modification_index <- df %>%
        group_by(ANO_EJE, GENERICA_NOMBRE, SUBGENERICA_NOMBRE, ESPECIFICA_NOMBRE) %>%
        summarise(Modification = n_distinct(MONTO_PIM) + n_distinct(MONTO_PIA), .groups = "drop")
    
    # Not including the GeographicalDisparity index as it is at the geographical level and cannot be assigned to each executor
    
    temporal_irregularity_index <- df %>%
        group_by(ANO_EJE, GENERICA_NOMBRE, SUBGENERICA_NOMBRE, ESPECIFICA_NOMBRE) %>%
        summarise(TemporalIrregularity = sum(ifelse(MONTO_EJECUCION > mean(MONTO_EJECUCION, na.rm = TRUE), 1, 0), na.rm = TRUE) / n(), .groups = "drop")
    
    # Combine the indices
    combined_dataset <- excessive_variation_index %>%
        left_join(execution_speed_index, by = c("ANO_EJE", "GENERICA_NOMBRE", "SUBGENERICA_NOMBRE", "ESPECIFICA_NOMBRE")) %>%
        left_join(frequent_modification_index, by = c("ANO_EJE", "GENERICA_NOMBRE", "SUBGENERICA_NOMBRE", "ESPECIFICA_NOMBRE")) %>%
        left_join(temporal_irregularity_index, by = c("ANO_EJE", "GENERICA_NOMBRE", "SUBGENERICA_NOMBRE", "ESPECIFICA_NOMBRE")) %>%
        group_by(ANO_EJE) %>%
        summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>%
        left_join(overspending_index, by = "ANO_EJE") %>%
        left_join(underimplementation_index, by = "ANO_EJE") %>%
        left_join(concentration_index, by = "ANO_EJE") %>%
        left_join(funding_diversity_index, by = "ANO_EJE") %>%
        left_join(transaction_type_index, by = "ANO_EJE")
    
    return(combined_dataset)
}


# Compute the indices for local and non-local governments
local_indices <- compute_indices(df_gastos_local)
nonlocal_indices <- compute_indices(df_gastos_non_local)

# Convert the year and month to a date for local and non-local datasets
local_indices <- local_indices %>%
    mutate(date = as.Date(paste(ANO_EJE, "01", "01", sep = "-"), format = "%Y-%m-%d"))

nonlocal_indices <- nonlocal_indices %>%
    mutate(date = as.Date(paste(ANO_EJE, "01", "01", sep = "-"), format = "%Y-%m-%d"))

# Compute the differences and cumulative differences
# Exclude "ANO_EJE" from the differential calculations
indices_diff <- local_indices %>%
    inner_join(nonlocal_indices, by = "ANO_EJE", suffix = c("_local", "_non_local")) %>%
    select(-starts_with(c("ANO_EJE", "GEN", "ESP", "SUBG"))) %>% 
    arrange(date_local)

# List of columns for which we need to calculate differences
cols_to_diff <- colnames(local_indices)[!str_detect(colnames(local_indices), "date|ANO|GEN|ESP|SUBG")]

# Calculate differences and cumulative differences
for (col in cols_to_diff) {
    diff_col_name <- paste0(col, "_diff")
    cumdiff_col_name <- paste0(col, "_cumdiff")
    
    local_col_name <- paste0(col, "_local")
    nonlocal_col_name <- paste0(col, "_non_local")
    
    indices_diff[[diff_col_name]] <- indices_diff[[local_col_name]] - indices_diff[[nonlocal_col_name]]
    indices_diff[[cumdiff_col_name]] <- cumsum(indices_diff[[diff_col_name]])
}


# Get a list of index names
index_names <- unique(unlist(lapply(strsplit(colnames(indices_diff), "_"), `[`, 1)))

# Remove non-index columns
index_names <- setdiff(index_names, c("ANO_EJE", "date"))

# Define the function to plot indices
plot_indices <- function(index_name) {
    
    # Define the columns to use
    local_col <- paste0(index_name, "_local")
    nonlocal_col <- paste0(index_name, "_non_local")
    diff_col <- paste0(index_name, "_diff")
    cumdiff_col <- paste0(index_name, "_cumdiff")
    
    # Plot 1: Level trends
    p1 <- ggplot(indices_diff, aes(x = date_local)) +
        geom_line(aes_string(y = local_col, colour = "'Local'")) +
        geom_line(aes_string(y = nonlocal_col, colour = "'Non-local'")) +
        labs(x = "Date", y = paste(index_name, "Index"), colour = "Government Type") +
        theme_minimal()
    
    # Plot 2: Difference between the two
    p2 <- ggplot(indices_diff, aes(x = date_local)) +
        geom_line(aes_string(y = diff_col, colour = "'Difference'")) +
        labs(x = "Date", y = paste("Difference in", index_name, "Index"), colour = "Metric") +
        theme_minimal()
    
    # Plot 3: Cumulative difference
    p3 <- ggplot(indices_diff, aes(x = date_local)) +
        geom_line(aes_string(y = cumdiff_col, colour = "'Cumulative Difference'")) +
        labs(x = "Date", y = paste("Cumulative Difference in", index_name, "Index"), colour = "Metric") +
        theme_minimal()
    
    # Combine the plots
    combined_plot <- p1 / p2 / p3
    
    return(combined_plot)
}

# Call the function for each index and store the results in a list
all_plots <- lapply(index_names, plot_indices)

all_plots[[3]]
