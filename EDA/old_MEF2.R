library(tidyverse)

overspending_index <- df_gastos %>%
    group_by(ANO_EJE, MES_EJE, NIVEL, PLIEGO) %>%
    summarise(Overspending = sum(ifelse(MONTO_EJECUCION > MONTO_PIM, 1, 0), na.rm = TRUE) / n(), .groups = "drop")

underimplementation_index <- df_gastos %>%
    group_by(ANO_EJE, MES_EJE, NIVEL, PLIEGO) %>%
    summarise(UnderImplementation = sum(MONTO_PIA, na.rm = TRUE) / sum(MONTO_EJECUCION, na.rm = TRUE), .groups = "drop")

concentration_index <- df_gastos %>%
    group_by(ANO_EJE, MES_EJE, NIVEL, PLIEGO) %>%
    summarise(Concentration = max(MONTO_EJECUCION, na.rm = TRUE) / sum(MONTO_EJECUCION, na.rm = TRUE), .groups = "drop")

funding_diversity_index <- df_gastos %>%
    group_by(ANO_EJE, MES_EJE, NIVEL, PLIEGO) %>%
    summarise(FundingDiversity = n_distinct(FUENTE_NOMBRE), .groups = "drop")

excessive_variation_index <- df_gastos %>%
    group_by(ANO_EJE, MES_EJE, NIVEL, PLIEGO, GENERICA_NOMBRE, SUBGENERICA_NOMBRE, ESPECIFICA_NOMBRE) %>%
    summarise(Variation = sd(MONTO_EJECUCION, na.rm = TRUE), .groups = "drop")

execution_speed_index <- df_gastos %>%
    group_by(ANO_EJE, MES_EJE, NIVEL, PLIEGO, GENERICA_NOMBRE, SUBGENERICA_NOMBRE, ESPECIFICA_NOMBRE) %>%
    summarise(ExecutionSpeed = sum(MONTO_EJECUCION, na.rm = TRUE) / n_distinct(MES_EJE), .groups = "drop")

frequent_modification_index <- df_gastos %>%
    group_by(ANO_EJE, MES_EJE, NIVEL, PLIEGO, GENERICA_NOMBRE, SUBGENERICA_NOMBRE, ESPECIFICA_NOMBRE) %>%
    summarise(Modification = n_distinct(MONTO_PIM) + n_distinct(MONTO_PIA), .groups = "drop")

geographical_disparity_index <- df_gastos %>%
    group_by(ANO_EJE, MES_EJE, DEPARTAMENTO_EJECUTORA, PROVINCIA_EJECUTORA, DISTRITO_EJECUTORA) %>%
    summarise(GeographicalDisparity = sd(MONTO_EJECUCION, na.rm = TRUE), .groups = "drop")

temporal_irregularity_index <- df_gastos %>%
    group_by(ANO_EJE, MES_EJE, NIVEL, PLIEGO, GENERICA_NOMBRE, SUBGENERICA_NOMBRE, ESPECIFICA_NOMBRE) %>%
    summarise(TemporalIrregularity = sum(ifelse(MONTO_EJECUCION > mean(MONTO_EJECUCION, na.rm = TRUE), 1, 0), na.rm = TRUE) / n(), .groups = "drop")

# summarisation at the adequate level

# Additional group_by and summarise for item-level indices

excessive_variation_index <- excessive_variation_index %>%
    group_by(ANO_EJE, MES_EJE, NIVEL, PLIEGO) %>%
    summarise(Variation = mean(Variation, na.rm = TRUE), .groups = "drop")

execution_speed_index <- execution_speed_index %>%
    group_by(ANO_EJE, MES_EJE, NIVEL, PLIEGO) %>%
    summarise(ExecutionSpeed = mean(ExecutionSpeed, na.rm = TRUE), .groups = "drop")

frequent_modification_index <- frequent_modification_index %>%
    group_by(ANO_EJE, MES_EJE, NIVEL, PLIEGO) %>%
    summarise(Modification = mean(Modification, na.rm = TRUE), .groups = "drop")

temporal_irregularity_index <- temporal_irregularity_index %>%
    group_by(ANO_EJE, MES_EJE, NIVEL, PLIEGO) %>%
    summarise(TemporalIrregularity = mean(TemporalIrregularity, na.rm = TRUE), .groups = "drop")

# Now join these datasets into the combined dataset as before
combined_dataset <- excessive_variation_index %>%
    left_join(execution_speed_index, by = c("ANO_EJE", "MES_EJE", "NIVEL", "PLIEGO")) %>%
    left_join(frequent_modification_index, by = c("ANO_EJE", "MES_EJE", "NIVEL", "PLIEGO")) %>%
    left_join(temporal_irregularity_index, by = c("ANO_EJE", "MES_EJE", "NIVEL", "PLIEGO")) %>% 
    left_join(underimplementation_index, by = c("ANO_EJE", "MES_EJE", "NIVEL", "PLIEGO")) %>%
    left_join(concentration_index, by = c("ANO_EJE", "MES_EJE", "NIVEL", "PLIEGO")) %>%
    left_join(geographical_disparity_index, by = c("ANO_EJE", "MES_EJE", "NIVEL", "PLIEGO")) %>%
    left_join(funding_diversity_index, by = c("ANO_EJE", "MES_EJE", "NIVEL", "PLIEGO")) %>% 
    left_join(overspending_index, by = c("ANO_EJE", "MES_EJE", "NIVEL", "PLIEGO"))
