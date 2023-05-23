library(tidyverse)
library(patchwork)
library(lubridate)
library(skimr)

sobrecostos <- combined_df %>%
    mutate(Publication_Year = floor_date(FECHA_PUBLICACION_CONTRATO, unit = "month")) %>%
    filter(Publication_Year > as.Date("2009-12-31", format = "%Y-%m-%d")) %>% 
    group_by(Publication_Year, Entity_RUC) %>%
    summarise(Entity = first(Entity),
              total_overcost = sum(overcost[overcost_flag == 1], na.rm = TRUE),
              num_projects = n_unique(Process),
              num_projects_overcost = sum(overcost_flag, na.rm = TRUE),
              proportion_overcost = num_projects_overcost / num_projects,
              total_overcost_percentage = sum(overcost_percentage[overcost_flag == 1], na.rm = TRUE),
              avg_overcost_percentage = mean(overcost_percentage[overcost_flag == 1], na.rm = TRUE),
              Consortium_Winners = sum(Is_Consortium == "S", na.rm = TRUE) / n(),
              Avg_Time_Publication_Buenapro = mean(as.numeric(difftime(Buenapro_Date, Publication_Date, units = "days")), na.rm = TRUE)
    )

# Filter grouped_df for all years using the extracted Entity_RUCs

muni_rucs_before_2017 <- sobrecostos %>%
    filter(Publication_Year <= as.Date("2016-12-31", format = "%Y-%m-%d"), str_detect(Entity, "MUNICIPALIDAD|GOBIERNO REGIONAL")) %>%
    distinct(Entity_RUC)

muni_df <- sobrecostos %>%
    semi_join(muni_rucs_before_2017, by = "Entity_RUC") %>%
    ungroup()


# Calculate summary statistics for sobrecostos and muni_df
grouped_summary <- sobrecostos %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Total_Overcost_Percentage = mean(proportion_overcost, na.rm = TRUE),
        Std_Error = sd(proportion_overcost, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Total_Overcost_Percentage - (1.96 * Std_Error), 0),
        Upper_Bound = Mean_Total_Overcost_Percentage + (1.96 * Std_Error)
    )

muni_summary <- muni_df %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Total_Overcost_Percentage = mean(proportion_overcost, na.rm = TRUE),
        Std_Error = sd(proportion_overcost, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Total_Overcost_Percentage - (1.96 * Std_Error), 0),
        Upper_Bound = Mean_Total_Overcost_Percentage + (1.96 * Std_Error)
    )

# Plot for sobrecostos
plot1 <- ggplot(grouped_summary, aes(x = Publication_Year, y = Mean_Total_Overcost_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    labs(title = "All processes",
         x = "Year",
         y = "% of projects with overcosts")

# Plot for muni_df
plot2 <- ggplot(muni_summary, aes(x = Publication_Year, y = Mean_Total_Overcost_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    labs(title = "Processes for Municipalidades",
         x = "Year",
         y = "% of projects with overcosts")

(plot1 / plot2) + plot_annotation("Proportion of projects of each entity with overcost")


### CONSORTIUM WINNERS
grouped_summary <- sobrecostos %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Total_Consortium_Winners  = mean(Consortium_Winners, na.rm = TRUE),
        Std_Error = sd(Consortium_Winners, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Total_Consortium_Winners - (1.96 * Std_Error), 0),
        Upper_Bound = Mean_Total_Consortium_Winners + (1.96 * Std_Error)
    )

muni_summary <- muni_df %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Total_Consortium_Winners = mean(Consortium_Winners, na.rm = TRUE),
        Std_Error = sd(Consortium_Winners, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Total_Consortium_Winners - (1.96 * Std_Error), 0),
        Upper_Bound = Mean_Total_Consortium_Winners + (1.96 * Std_Error)
    )

# Plot for consortium
plot1 <- ggplot(grouped_summary, aes(x = Publication_Year, y = Mean_Total_Consortium_Winners)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    labs(title = "All processes",
         x = "Year",
         y = "% of projects adjudicated to consortiums")

# Plot for muni_df
plot2 <- ggplot(muni_summary, aes(x = Publication_Year, y = Mean_Total_Consortium_Winners)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    labs(title = "Processes for Municipalidades",
         x = "Year",
         y = "% of projects adjudicated to consortiums")

(plot1 / plot2) + plot_annotation("Proportion of projects adjudicated to consortiums")


## PUBLICATION TO BUENA PRO

grouped_summary <- sobrecostos %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Avg_Time_Publication_Buenapro  = mean(Avg_Time_Publication_Buenapro, na.rm = TRUE),
        Std_Error = sd(Avg_Time_Publication_Buenapro, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Avg_Time_Publication_Buenapro - (1.96 * Std_Error), 0),
        Upper_Bound = Mean_Avg_Time_Publication_Buenapro + (1.96 * Std_Error)
    )

muni_summary <- muni_df %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Avg_Time_Publication_Buenapro = mean(Avg_Time_Publication_Buenapro, na.rm = TRUE),
        Std_Error = sd(Avg_Time_Publication_Buenapro, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Avg_Time_Publication_Buenapro - (1.96 * Std_Error), 0),
        Upper_Bound = Mean_Avg_Time_Publication_Buenapro + (1.96 * Std_Error)
    )

# Plot for consortium
plot1 <- ggplot(grouped_summary, aes(x = Publication_Year, y = Mean_Avg_Time_Publication_Buenapro)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
    scale_y_continuous(labels = scales::comma, limits = c(0, 30)) +
    theme_minimal() +
    labs(title = "All processes",
         x = "Year",
         y = "Mean day difference")

# Plot for muni_df
plot2 <- ggplot(muni_summary, aes(x = Publication_Year, y = Mean_Avg_Time_Publication_Buenapro)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
    scale_y_continuous(labels = scales::comma, limits = c(0, 30)) +
    theme_minimal() +
    labs(title = "Processes for Municipalidades",
         x = "Year",
         y = "Mean day difference")

(plot1 / plot2) + plot_annotation("Mean days between publication and buena pro by Year")



## Ratio sobrecostos promedio

grouped_summary <- sobrecostos %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Avg_Time_Publication_Buenapro  = mean(Avg_Time_Publication_Buenapro, na.rm = TRUE),
        Std_Error = sd(Avg_Time_Publication_Buenapro, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Avg_Time_Publication_Buenapro - (1.96 * Std_Error), 0),
        Upper_Bound = Mean_Avg_Time_Publication_Buenapro + (1.96 * Std_Error)
    )

muni_summary <- muni_df %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Avg_Time_Publication_Buenapro = mean(Avg_Time_Publication_Buenapro, na.rm = TRUE),
        Std_Error = sd(Avg_Time_Publication_Buenapro, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = pmax(Mean_Avg_Time_Publication_Buenapro - (1.96 * Std_Error), 0),
        Upper_Bound = Mean_Avg_Time_Publication_Buenapro + (1.96 * Std_Error)
    )

# Plot for consortium
plot1 <- ggplot(grouped_summary, aes(x = Publication_Year, y = Mean_Avg_Time_Publication_Buenapro)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
    scale_y_continuous(labels = scales::comma, limits = c(0, 30)) +
    theme_minimal() +
    labs(title = "All processes",
         x = "Year",
         y = "Mean day difference")

# Plot for muni_df
plot2 <- ggplot(muni_summary, aes(x = Publication_Year, y = Mean_Avg_Time_Publication_Buenapro)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = as_datetime("2015-01-01"), color = "red") +
    scale_y_continuous(labels = scales::comma, limits = c(0, 30)) +
    theme_minimal() +
    labs(title = "Processes for Municipalidades",
         x = "Year",
         y = "Mean day difference")

(plot1 / plot2) + plot_annotation("Mean days between publication and buena pro by Year")

