library(plm)

# Remove rows with NAs in index dimensions
panel_time_series_filtered <- balanced_panel_time_series %>%
    mutate(cutoff = mesanho_publicacion > "2014-01-01") %>% 
    filter(!is.na(gobierno) & !is.na(mesanho_publicacion))

# Create pdata.frame with filtered data
annual_panel <- pdata.frame(panel_time_series_filtered,
                            index = c("gobierno", "mesanho_publicacion"))

write.csv(panel_time_series_filtered, "data/02_intermediate/yearmonth_panel_data.csv")
# models

annual_panel %>% 
    plm(data = .,
        model = "within",
        perc_single_bidder ~ cutoff) %>% 
    summary()

annual_panel %>% 
    plm(data = .,
        model = "within",
        n_total_projects ~ cutoff) %>% 
    summary()

annual_panel %>% 
    plm(data = .,
        model = "within",
        avg_bidders_per_project ~ cutoff) %>% 
    summary()

annual_panel %>% 
    plm(data = .,
        model = "within",
        avg_diff ~ cutoff) %>% 
    summary()

annual_panel %>% 
    plm(data = .,
        model = "within",
        avg_ratio ~ cutoff) %>% 
    summary()

annual_panel %>% 
    plm(data = .,
        model = "within",
        valor_adjudicado_mean ~ cutoff) %>% 
    summary()

annual_panel %>% 
    plm(data = .,
        model = "within",
        tiempo_promedio ~ cutoff) %>% 
    summary()


# Plot trajectories

annual_panel %>% 
    filter(avg_bidders_per_project < 100) %>% 
    ggplot(aes(x = mesanho_publicacion, y = avg_bidders_per_project, group = gobierno)) +
    geom_line(alpha = 0.05)
