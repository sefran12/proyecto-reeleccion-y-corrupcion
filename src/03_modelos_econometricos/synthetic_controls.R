library(tidyverse)
library(patchwork)

theme_set(theme_bw())

# OSCE
# Promedio de postores de proyectos por municipalidad mes
# Porcentaje de proyectos con un solo postor
# Porcentaje de ganadores “repetidos” por una unidad subnacional-semestre
# Ratio promedio entre el valor adjudicado y valor de referencia
# SIAF
# Porcentaje promedio de contratos que no pasaron por el sistema OSCE
# Porcentaje de proyectos con sobrecostos

df <- single_bidder_percentage  %>% 
    mutate(
        tipo_municipalidad = case_when(str_detect(gobierno, "PROVINCIAL") ~ "provincial",
                                       str_detect(gobierno, "REGI") ~ "regional",
                                       str_detect(gobierno, "DIS") ~ "distrital",
                                       TRUE ~ "Otras entidades"),
        mesanho_publicacion = floor_date(mesanho_publicacion, "quarter")
    ) %>% 
    group_by(mesanho_publicacion, gobierno, OBJETO, tipo_municipalidad) %>% 
    summarise(
        perc_single_bidder = sum(single_bidder_projects, na.rm = TRUE) / sum(total_projects, na.rm = TRUE)
    )

# Import rlang if not already done
library(rlang)

get_synthetic_experiment <- function(df, col_name) {
    # Get unique combinations of gobierno, OBJETO and tipo_municipalidad
    gob_obj_tipo_pairs <- df %>% 
        ungroup() %>% 
        filter(tipo_municipalidad != "Otras entidades") %>% 
        distinct(gobierno, OBJETO, tipo_municipalidad)
    
    # Get mean effects as base for synthetic control
    mean_df <- df %>%
        filter(tipo_municipalidad == "Otras entidades") %>% 
        group_by(mesanho_publicacion, OBJETO) %>%
        summarise(mean_value = mean(!!sym(col_name), na.rm = TRUE))
    
    # Start loop
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
            
            # Merge with mean that corresponds to its OBJETO and tipo_municipalidad
            gob_df <- left_join(gob_df, mean_df, by = c("mesanho_publicacion", "OBJETO"))
            
            # Filter pre-treatment period
            pre_treat_df <- gob_df %>%
                filter(mesanho_publicacion < as.Date("2014-10-10"))
            
            # Create a linear model
            lm_model <- lm(as.formula(paste(col_name, "~ mean_value")), 
                           data = pre_treat_df)
            
            pre_treat_df$predictions <- predict(lm_model, newdata = pre_treat_df)
            
            # Generate predictions for the post-treatment period
            post_treat_df <- gob_df %>%
                filter(mesanho_publicacion >= as.Date("2014-10-10"))
            post_treat_df$predictions <- predict(lm_model, newdata = post_treat_df)
            
            # Generate full predictions
            gob_df$predictions <- predict(lm_model, newdata = gob_df)
            gob_df$cumulative_effect <- cumsum(gob_df[[col_name]] - gob_df$predictions)
            
            # Save results
            # Convert effects to a dataframe and add to effects_df
            gob_effects <- data.frame(
                gobierno = rep(gob, length(gob_df[[col_name]])),
                OBJETO = rep(obj, length(gob_df[[col_name]])),
                tipo_municipalidad = rep(tipo, length(gob_df[[col_name]])),
                mesanho_publicacion = gob_df$mesanho_publicacion,
                effect = gob_df[[col_name]] - gob_df$predictions,
                cumulative_effect = gob_df$cumulative_effect
            )
            
            effects_df <- rbind(effects_df, gob_effects)
            
        }, error = function(e) {
            # This will print the error message
            print(paste("Error processing gobierno, OBJETO, and tipo_municipalidad: ", gob, obj, tipo, " - ", e$message))
        })
    }
    
    return(effects_df)
}



# Plot synthetic control experiment
plot_experiment <- function(effects_df) {
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
        facet_wrap(~OBJETO, scales = "free") +
        # Add mean series in red
        geom_line(data = effects_summary, aes(x = mesanho_publicacion, y = mean_effect), color = "red", inherit.aes = FALSE) +
        # Add confidence intervals in orange
        geom_ribbon(data = effects_summary, aes(x = mesanho_publicacion, ymin = lower_ci, ymax = upper_ci), fill = "orange", alpha = 0.3, inherit.aes = FALSE) +
        # guidelines
        geom_vline(xintercept = as.Date("2014-10-10")) +
        geom_hline(yintercept = 0) +
        # Customize plot
        labs(title = "Effects Over Time", x = "Time", y = "Effect") +
        theme_minimal()
    
    # Plot all individual cumulative effect series
    p2 <- ggplot(effects_df, aes(x = mesanho_publicacion, y = cumulative_effect, group = gobierno)) +
        facet_wrap(~OBJETO, scales = "free") +
        # Add mean cumulative effect series in red
        geom_line(data = cumulative_effects_summary, aes(x = mesanho_publicacion, y = mean_cumulative_effect), color = "red", inherit.aes = FALSE) +
        # Add confidence intervals in orange
        geom_ribbon(data = cumulative_effects_summary, aes(x = mesanho_publicacion, ymin = lower_ci, ymax = upper_ci), fill = "orange", alpha = 0.3, inherit.aes = FALSE) +
        geom_vline(xintercept = as.Date("2014-10-10")) +
        geom_hline(yintercept = 0) +
        # Customize plot
        labs(title = "Cumulative Effects Over Time", x = "Time", y = "Cumulative Effect") +
        theme_minimal()
    
    # Print plots
    print(p / p2)
}

effects_df <- get_synthetic_experiment(df, "perc_single_bidder")
plot_experiment(effects_df)
