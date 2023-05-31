library(data.table)

df_gastos <- fread("data/01_raw/EP_Estado_Ejecucion_Gastos_2011_2020.csv")
df_ingresos <- fread("data/01_raw/EP_Estado_Ejecucion_Ingresos_2011_2020.csv")

colnames(df_gastos)
colnames(df_ingresos)

library(dplyr)

# Preprocess the data: create a new variable indicating if the execution amount 
# exceeds the final authorized amount
df_gastos <- df_gastos %>%
    mutate(overexpenditure = ifelse(MONTO_EJECUCION > MONTO_PIM, 1, 0),
           large_overexpenditure = ifelse(MONTO_EJECUCION > 1.5 * MONTO_PIM, 1, 0))

# Compute the indices monthly
corruption_indices <- df_gastos %>%
    group_by(ANO_EJE) %>%
    summarise(overexpenditure_index = mean(overexpenditure),
              large_overexpenditure_index = mean(large_overexpenditure))

# Convert the year and month to a date
corruption_indices <- corruption_indices %>%
    mutate(date = as.Date(paste(ANO_EJE, "01", "01", sep = "-"), format = "%Y-%m-%d")) %>%
    select(date, overexpenditure_index, large_overexpenditure_index)

# View the data
print(corruption_indices)

##

# Filter the data into local and non-local governments
df_gastos_local <- df_gastos %>% 
    filter(NIVEL_GOBIERNO_NOMBRE %in% c("GOBIERNOS REGIONALES", "GOBIERNOS LOCALES"))
df_gastos_non_local <- df_gastos 

# Compute the indices for local governments
corruption_indices_local <- df_gastos_local %>%
    group_by(ANO_EJE) %>%
    summarise(overexpenditure_index = mean(overexpenditure),
              large_overexpenditure_index = mean(large_overexpenditure))

corruption_indices_local <- corruption_indices_local %>%
    mutate(date = as.Date(paste(ANO_EJE, "01", "01", sep = "-"), format = "%Y-%m-%d")) %>%
    select(date, overexpenditure_index, large_overexpenditure_index)

# Compute the indices for non-local governments
corruption_indices_non_local <- df_gastos_non_local %>%
    group_by(ANO_EJE) %>%
    summarise(overexpenditure_index = mean(overexpenditure),
              large_overexpenditure_index = mean(large_overexpenditure))

corruption_indices_non_local <- corruption_indices_non_local %>%
    mutate(date = as.Date(paste(ANO_EJE, "01", "01", sep = "-"), format = "%Y-%m-%d")) %>%
    select(date, overexpenditure_index, large_overexpenditure_index)

# Plot the data
library(ggplot2)

# Compute the differences and cumulative differences
corruption_indices <- corruption_indices_local %>%
    inner_join(corruption_indices_non_local, by = "date", suffix = c("_local", "_non_local")) %>%
    mutate(overexpenditure_diff = overexpenditure_index_local - overexpenditure_index_non_local,
           large_overexpenditure_diff = large_overexpenditure_index_local - large_overexpenditure_index_non_local) %>%
    arrange(date) %>%
    mutate(overexpenditure_cum_diff = cumsum(overexpenditure_diff),
           large_overexpenditure_cum_diff = cumsum(large_overexpenditure_diff))

# Create the plots
library(patchwork)

# Plot 1: Level trends
p1 <- ggplot(corruption_indices, aes(x = date)) +
    geom_line(aes(y = overexpenditure_index_local, colour = "Local")) +
    geom_line(aes(y = overexpenditure_index_non_local, colour = "Non-local")) +
    labs(x = "Date", y = "Overexpenditure Index", colour = "Government Type") +
    theme_minimal()

# Plot 2: Difference between the two
p2 <- ggplot(corruption_indices, aes(x = date)) +
    geom_line(aes(y = overexpenditure_diff, colour = "Difference")) +
    labs(x = "Date", y = "Difference in Overexpenditure Index", colour = "Metric") +
    theme_minimal()

# Plot 3: Cumulative difference
p3 <- ggplot(corruption_indices, aes(x = date)) +
    geom_line(aes(y = overexpenditure_cum_diff, colour = "Cumulative Difference")) +
    labs(x = "Date", y = "Cumulative Difference in Overexpenditure Index", colour = "Metric") +
    theme_minimal()

# Combine the plots
p1 / p2 / p3


# Plot 1: Level trends
p1 <- ggplot(corruption_indices, aes(x = date)) +
    geom_line(aes(y = large_overexpenditure_index_local, colour = "Local")) +
    geom_line(aes(y = large_overexpenditure_index_non_local, colour = "Non-local")) +
    labs(x = "Date", y = "Overexpenditure Index", colour = "Government Type") +
    theme_minimal()

# Plot 2: Difference between the two
p2 <- ggplot(corruption_indices, aes(x = date)) +
    geom_line(aes(y = large_overexpenditure_diff, colour = "Difference")) +
    labs(x = "Date", y = "Difference in Overexpenditure Index", colour = "Metric") +
    theme_minimal()

# Plot 3: Cumulative difference
p3 <- ggplot(corruption_indices, aes(x = date)) +
    geom_line(aes(y = large_overexpenditure_cum_diff, colour = "Cumulative Difference")) +
    labs(x = "Date", y = "Cumulative Difference in Overexpenditure Index", colour = "Metric") +
    theme_minimal()

# Combine the plots
p1 / p2 / p3

