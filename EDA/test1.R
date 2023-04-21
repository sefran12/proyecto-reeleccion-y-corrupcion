
## PLOTS

library(ggplot2)

plot1 <- ggplot(muni_df, aes(x = Publication_Year, y = Number_Processes, group = Entity_RUC)) +
    geom_line(alpha = 0.1) +
    theme_minimal() +
    labs(title = "Number of Processes by Year",
         x = "Year",
         y = "Number of Processes")

plot1

plot2 <- ggplot(grouped_df, aes(x = Publication_Year, y = Single_Bidder_Percentage, group = Entity_RUC)) +
    geom_line(alpha = 0.1) +
    theme_minimal() +
    labs(title = "Percentage of Single-Bidder Processes by Year",
         x = "Year",
         y = "Single-Bidder Percentage")

plot2

plot3 <- ggplot(muni_df, aes(x = Publication_Year, y = Avg_Time_Publication_Buenapro, group = Entity_RUC)) +
    geom_line(alpha = 0.1) +
    labs(title = "Average Time Between Publication and Buenapro by Year",
         x = "Year",
         y = "Average Time Between Publication and Buenapro (days)") +
    theme_minimal()

plot3

## Tests

muni_df <- 
    muni_df %>% 
    mutate(
        cutoff = year(Publication_Year) >= 2015
    )

# muni_df %>% 
#     lm(data = .,
#        Single_Bidder_Percentage ~ cutoff) %>% 
#     summary()

muni_df %>% 
    lm(data = .,
       Avg_Time_Publication_Buenapro ~ cutoff) %>% 
    summary()



##
# Calculate summary statistics for grouped_df and muni_df
grouped_summary <- grouped_df %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Single_Bidder_Percentage = mean(Single_Bidder_Percentage, na.rm = TRUE),
        Lower_Bound = quantile(Single_Bidder_Percentage, 0.025, na.rm = TRUE),
        Upper_Bound = quantile(Single_Bidder_Percentage, 0.975, na.rm = TRUE)
    )

muni_summary <- muni_df %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Single_Bidder_Percentage = mean(Single_Bidder_Percentage, na.rm = TRUE),
        Lower_Bound = quantile(Single_Bidder_Percentage, 0.025, na.rm = TRUE),
        Upper_Bound = quantile(Single_Bidder_Percentage, 0.975, na.rm = TRUE)
    )

# Plot for grouped_df
plot2 <- ggplot(grouped_summary, aes(x = Publication_Year, y = Mean_Single_Bidder_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "Percentage of Single-Bidder Processes by Year",
         x = "Year",
         y = "Single-Bidder Percentage")

plot2

plot2b <- ggplot(muni_summary, aes(x = Publication_Year, y = Mean_Single_Bidder_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "Percentage of Single-Bidder Processes by Year",
         x = "Year",
         y = "Single-Bidder Percentage")

plot2b

####

# Calculate summary statistics for grouped_df and muni_df
grouped_summary <- grouped_df %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Single_Bidder_Percentage = mean(Single_Bidder_Percentage, na.rm = TRUE),
        Std_Error = sd(Single_Bidder_Percentage, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = Mean_Single_Bidder_Percentage - (1.96 * Std_Error),
        Upper_Bound = Mean_Single_Bidder_Percentage + (1.96 * Std_Error)
    )

muni_summary <- muni_df %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Single_Bidder_Percentage = mean(Single_Bidder_Percentage, na.rm = TRUE),
        Std_Error = sd(Single_Bidder_Percentage, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = Mean_Single_Bidder_Percentage - (1.96 * Std_Error),
        Upper_Bound = Mean_Single_Bidder_Percentage + (1.96 * Std_Error)
    )

# Plot for grouped_df
plot2 <- ggplot(grouped_summary, aes(x = Publication_Year, y = Mean_Single_Bidder_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "Percentage of Single-Bidder Processes by Year",
         x = "Year",
         y = "Single-Bidder Percentage")

plot2

# Plot for muni_df
plot2 <- ggplot(muni_summary, aes(x = Publication_Year, y = Mean_Single_Bidder_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "Percentage of Single-Bidder Processes by Year",
         x = "Year",
         y = "Single-Bidder Percentage")

plot2

#####
library(patchwork)

# Calculate summary statistics for grouped_df and muni_df
grouped_summary <- grouped_df %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Single_Bidder_Percentage = mean(Avg_Time_Publication_Buenapro, na.rm = TRUE),
        Std_Error = sd(Avg_Time_Publication_Buenapro, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = Mean_Single_Bidder_Percentage - (1.96 * Std_Error),
        Upper_Bound = Mean_Single_Bidder_Percentage + (1.96 * Std_Error)
    )

muni_summary <- muni_df %>%
    group_by(Publication_Year) %>%
    summarise(
        Mean_Single_Bidder_Percentage = mean(Avg_Time_Publication_Buenapro, na.rm = TRUE),
        Std_Error = sd(Avg_Time_Publication_Buenapro, na.rm = TRUE) / sqrt(n()),
        Lower_Bound = Mean_Single_Bidder_Percentage - (1.96 * Std_Error),
        Upper_Bound = Mean_Single_Bidder_Percentage + (1.96 * Std_Error)
    )

# Plot for grouped_df
plot1 <- ggplot(grouped_summary, aes(x = Publication_Year, y = Mean_Single_Bidder_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = 2015, color = "red") +
    theme_minimal() +
#    scale_x_continuous(labels = scales::label_number(accuracy = 1, big.mark = "")) +
    labs(title = "All processes",
         x = "Year",
         y = "Mean day difference")

# Plot for muni_df
plot2 <- ggplot(muni_summary, aes(x = Publication_Year, y = Mean_Single_Bidder_Percentage)) +
    geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2) +
    geom_point() +
    geom_line(size = 1) +
    geom_vline(xintercept = 2015, color = "red") +
#    scale_x_continuous(labels = scales::label_number(accuracy = 1, big.mark = "")) +
    theme_minimal() +
    labs(title = "Processes for Municipalidades",
         x = "Year",
         y = "Mean day difference")

(plot1 / plot2) + plot_annotation("Mean days between publication and buena pro by Year")

