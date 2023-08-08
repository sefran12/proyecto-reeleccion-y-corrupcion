library(tidyverse)
library(patchwork)

theme_set(theme_bw())

df <- single_bidder_percentage  %>% 
    mutate(
        tipo_municipalidad = case_when(str_detect(gobierno, "PROVINCIAL") ~ "provincial",
                                       str_detect(gobierno, "REGI") ~ "regional",
                                       str_detect(gobierno, "DIS") ~ "distrital",
                                       TRUE ~ "Otras entidades"),
        mesanho_publicacion = floor_date(mesanho_publicacion, "quarter")
    ) %>% 
    group_by(gobierno, mesanho_publicacion, OBJETO, tipo_municipalidad) %>% 
    summarise(
        total_projects = sum(total_projects),
        total_bidders = sum(total_bidders),
        single_bidder_projects = sum(single_bidder_projects),
        perc_single_bidder = single_bidder_projects / total_projects
    )


# Calculate the mean perc_single_bidder by mesanho_publicacion and OBJETO
mean_df <- df %>%
    group_by(mesanho_publicacion, OBJETO, tipo_municipalidad) %>%
    summarise(mean_perc_single_bidder = mean(perc_single_bidder, na.rm = TRUE))

# Initialize memory
effects_df <- data.frame()

# Iterate over each unique gobierno
for (gob in unique(df$gobierno)) {
    cat(gob)
    tryCatch({
        # Filter data for the current gobierno
        gob_df <- df %>%
            filter(gobierno == gob)
        
        # Merge with mean_perc_single_bidder that corresponds to its OBJETO
        gob_df <- left_join(gob_df, mean_df, by = c("mesanho_publicacion", "OBJETO", "tipo_municipalidad"))
        
        # Filter pre-treatment period
        pre_treat_df <- gob_df %>%
            filter(mesanho_publicacion < as.Date("2014-10-10"))
        
        # Create a linear model
        lm_model <- lm(perc_single_bidder ~ mean_perc_single_bidder, 
                       data = pre_treat_df)
        
        pre_treat_df$predictions <- predict(lm_model, newdata = pre_treat_df)
        
        # Generate predictions for the post-treatment period
        post_treat_df <- gob_df %>%
            filter(mesanho_publicacion >= as.Date("2014-10-10"))
        post_treat_df$predictions <- predict(lm_model, newdata = post_treat_df)
        
        # Generate full predictions
        gob_df$predictions <- predict(lm_model, newdata = gob_df)
        gob_df$cumulative_effect <- cumsum(gob_df$perc_single_bidder - gob_df$predictions)
        
        # Save results
        # Convert effects to a dataframe and add to effects_df
        gob_effects <- data.frame(
            gobierno = rep(gob, length(gob_df$perc_single_bidder)),
            mesanho_publicacion = gob_df$mesanho_publicacion,
            effect = gob_df$perc_single_bidder - gob_df$predictions,
            cumulative_effect = gob_df$cumulative_effect
        )
        
        effects_df <- rbind(effects_df, gob_effects)
    }, error = function(e) {
        # This will print the error message
        print(paste("Error processing gobierno: ", gob, " - ", e$message))
    })
    
    
    # Plot original series and counterfactual series
    # p1 <- ggplot() +
    #     geom_line(data = gob_df, aes(x = mesanho_publicacion, y = perc_single_bidder), color = "blue") +
    #     geom_line(data = gob_df, aes(x = mesanho_publicacion, y = predictions), color = "red") +
    #     geom_vline(xintercept = as.Date("2014-10-10")) +
    #     labs(title = paste0("Gobierno: ", gob),
    #          x = "Time",
    #          y = "Percentage of Single Bidders",
    #          color = "Series") +
    #     theme_minimal() +
    #     scale_color_manual(values = c("blue" = "Original", "red" = "Counterfactual"))
    # 
    # 
    # # Plot cumulative effect
    # p2 <- ggplot(gob_df, aes(x = mesanho_publicacion, y = cumulative_effect)) +
    #     geom_line(color = "green") +
    #     geom_hline(yintercept = 0) +
    #     geom_vline(xintercept = as.Date("2014-10-10")) +
    #     labs(title = paste0("Cumulative Effect for Gobierno: ", gob),
    #          x = "Time",
    #          y = "Cumulative Effect") +
    #     theme_minimal()
    # 
    # 
    # # Combine the two plots
    # combined_plot <- p1 / p2
    # print(combined_plot)
}

# Plot full distributions
# Calculate mean effect and confidence intervals for each time point
#ef_df <- effects_df
#effects_df <- ef_df

effects_summary <- effects_df %>%
    filter(!is.na(mesanho_publicacion)) %>% 
    group_by(mesanho_publicacion) %>%
    summarise(
        mean_effect = mean(effect, na.rm = TRUE),
        se = sd(effect, na.rm = TRUE) / sqrt(n()),
        lower_ci = mean_effect - qt(0.975, n()-1)*se,
        upper_ci = mean_effect + qt(0.975, n()-1)*se
    )

# Plot all individual series
p <- ggplot(effects_df, aes(x = mesanho_publicacion, y = effect, group = gobierno)) #+
#    geom_line(alpha = 0.1)

# Add mean series in red
p <- p + geom_line(data = effects_summary, aes(x = mesanho_publicacion, y = mean_effect), color = "red", inherit.aes = FALSE)

# Add confidence intervals in orange
p <- p + geom_ribbon(data = effects_summary, aes(x = mesanho_publicacion, ymin = lower_ci, ymax = upper_ci), fill = "orange", alpha = 0.3, inherit.aes = FALSE)

# Customize plot
p <- p +
    labs(title = "Effects Over Time", x = "Time", y = "Effect") +
    theme_minimal()

# Calculate mean cumulative effect and confidence intervals for each time point
cumulative_effects_summary <- effects_df %>%
    filter(!is.na(mesanho_publicacion)) %>% 
    group_by(mesanho_publicacion) %>%
    summarise(
        mean_cumulative_effect = mean(cumulative_effect, na.rm = TRUE),
        se = sd(cumulative_effect, na.rm = TRUE) / sqrt(n()),
        lower_ci = mean_cumulative_effect - qt(0.975, n()-1)*se,
        upper_ci = mean_cumulative_effect + qt(0.975, n()-1)*se
    )

# Plot all individual cumulative effect series
p2 <- ggplot(effects_df, aes(x = mesanho_publicacion, y = cumulative_effect, group = gobierno)) #+
#    geom_line(alpha = 0.04)

# Add mean cumulative effect series in red
p2 <- p2 + geom_line(data = cumulative_effects_summary, aes(x = mesanho_publicacion, y = mean_cumulative_effect), color = "red", inherit.aes = FALSE)

# Add confidence intervals in orange
p2 <- p2 + geom_ribbon(data = cumulative_effects_summary, aes(x = mesanho_publicacion, ymin = lower_ci, ymax = upper_ci), fill = "orange", alpha = 0.3, inherit.aes = FALSE)

# Customize plot
p2 <- p2 +
    labs(title = "Cumulative Effects Over Time", x = "Time", y = "Cumulative Effect") +
    theme_minimal()

# Print plot
print(p / p2)


# Last effect:

last_point <- ef_df %>% 
    group_by(gobierno) %>% 
    do(tail(., n = 1))

hist(last_point$cumulative_effect, breaks = 100)

### FULL 
# Get unique combinations of gobierno, OBJETO and tipo_municipalidad
gob_obj_tipo_pairs <- df %>% ungroup %>% distinct(gobierno, OBJETO, tipo_municipalidad)

effects_df <- data.frame()

# Loop over the rows of gob_obj_tipo_pairs
for (i in 1:nrow(gob_obj_tipo_pairs)) {
    gob <- gob_obj_tipo_pairs$gobierno[i]
    obj <- gob_obj_tipo_pairs$OBJETO[i]
    tipo <- gob_obj_tipo_pairs$tipo_municipalidad[i]
    
    cat(gob, obj, tipo)
    tryCatch({
        # Filter data for the current gobierno, OBJETO and tipo_municipalidad
        gob_df <- df %>%
            filter(gobierno == gob, OBJETO == obj, tipo_municipalidad == tipo)
        
        # Merge with mean_perc_single_bidder that corresponds to its OBJETO and tipo_municipalidad
        gob_df <- left_join(gob_df, mean_df, by = c("mesanho_publicacion", "OBJETO", "tipo_municipalidad"))
        
        # Filter pre-treatment period
        pre_treat_df <- gob_df %>%
            filter(mesanho_publicacion < as.Date("2014-10-10"))
        
        # Create a linear model
        lm_model <- lm(perc_single_bidder ~ mean_perc_single_bidder, 
                       data = pre_treat_df)
        
        pre_treat_df$predictions <- predict(lm_model, newdata = pre_treat_df)
        
        # Generate predictions for the post-treatment period
        post_treat_df <- gob_df %>%
            filter(mesanho_publicacion >= as.Date("2014-10-10"))
        post_treat_df$predictions <- predict(lm_model, newdata = post_treat_df)
        
        # Generate full predictions
        gob_df$predictions <- predict(lm_model, newdata = gob_df)
        gob_df$cumulative_effect <- cumsum(gob_df$perc_single_bidder - gob_df$predictions)
        
        # Save results
        # Convert effects to a dataframe and add to effects_df
        gob_effects <- data.frame(
            gobierno = rep(gob, length(gob_df$perc_single_bidder)),
            OBJETO = rep(obj, length(gob_df$perc_single_bidder)),
            tipo_municipalidad = rep(tipo, length(gob_df$perc_single_bidder)),
            mesanho_publicacion = gob_df$mesanho_publicacion,
            effect = gob_df$perc_single_bidder - gob_df$predictions,
            cumulative_effect = gob_df$cumulative_effect
        )
        
        effects_df <- rbind(effects_df, gob_effects)
        
        # # Plot original series and counterfactual series
        # p1 <- ggplot() +
        #     geom_line(data = gob_df, aes(x = mesanho_publicacion, y = perc_single_bidder), color = "blue") +
        #     geom_line(data = gob_df, aes(x = mesanho_publicacion, y = predictions), color = "red") +
        #     geom_vline(xintercept = as.Date("2014-10-10")) +
        #     labs(title = paste0("Gobierno: ", gob, "\nTipo: ", obj),
        #          x = "Time",
        #          y = "Percentage of Single Bidders",
        #          color = "Series") +
        #     theme_minimal() +
        #     scale_color_manual(values = c("blue" = "Original", "red" = "Counterfactual"))
        # 
        # 
        # # Plot cumulative effect
        # p2 <- ggplot(gob_df, aes(x = mesanho_publicacion, y = cumulative_effect)) +
        #     geom_line(color = "green") +
        #     geom_hline(yintercept = 0) +
        #     geom_vline(xintercept = as.Date("2014-10-10")) +
        #     labs(title = paste0("Cumulative Effect"),
        #          x = "Time",
        #          y = "Cumulative Effect") +
        #     theme_minimal()
        # 
        # 
        # # Combine the two plots
        # combined_plot <- p1 / p2
        # print(combined_plot)

    }, error = function(e) {
        # This will print the error message
        print(paste("Error processing gobierno, OBJETO, and tipo_municipalidad: ", gob, obj, tipo, " - ", e$message))
    })
}

effects_summary <- effects_df %>%
    filter(!is.na(mesanho_publicacion)) %>% 
    group_by(OBJETO, mesanho_publicacion) %>%
    summarise(
        mean_effect = mean(effect, na.rm = TRUE),
        se = sd(effect, na.rm = TRUE) / sqrt(n()),
        lower_ci = mean_effect - qt(0.975, n()-1)*se,
        upper_ci = mean_effect + qt(0.975, n()-1)*se
    )

cumulative_effects_summary <- effects_df %>%
    filter(!is.na(mesanho_publicacion)) %>% 
    group_by(OBJETO, mesanho_publicacion) %>%
    summarise(
        mean_cumulative_effect = mean(cumulative_effect, na.rm = TRUE),
        se = sd(cumulative_effect, na.rm = TRUE) / sqrt(n()),
        lower_ci = mean_cumulative_effect - qt(0.975, n()-1)*se,
        upper_ci = mean_cumulative_effect + qt(0.975, n()-1)*se
    )

# Plot all individual series
p <- ggplot(effects_df, aes(x = mesanho_publicacion, y = effect, group = gobierno)) +
#    geom_line(alpha = 0.04) +
    facet_wrap(~OBJETO, scales = "free") +
    # Add mean series in red
    geom_line(data = effects_summary, aes(x = mesanho_publicacion, y = mean_effect), color = "red", inherit.aes = FALSE) +
    # Add confidence intervals in orange
    geom_ribbon(data = effects_summary, aes(x = mesanho_publicacion, ymin = lower_ci, ymax = upper_ci), fill = "orange", alpha = 0.3, inherit.aes = FALSE) +
    # Customize plot
    labs(title = "Effects Over Time", x = "Time", y = "Effect") +
    theme_minimal()

# Plot all individual cumulative effect series
p2 <- ggplot(effects_df, aes(x = mesanho_publicacion, y = cumulative_effect, group = gobierno)) +
#    geom_line(alpha = 0.04) +
    facet_wrap(~OBJETO, scales = "free") +
    # Add mean cumulative effect series in red
    geom_line(data = cumulative_effects_summary, aes(x = mesanho_publicacion, y = mean_cumulative_effect), color = "red", inherit.aes = FALSE) +
    # Add confidence intervals in orange
    geom_ribbon(data = cumulative_effects_summary, aes(x = mesanho_publicacion, ymin = lower_ci, ymax = upper_ci), fill = "orange", alpha = 0.3, inherit.aes = FALSE) +
    # Customize plot
    labs(title = "Cumulative Effects Over Time", x = "Time", y = "Cumulative Effect") +
    theme_minimal()

# Print plots
print(p / p2)
