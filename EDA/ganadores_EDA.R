ganadores <- full_data %>% 
    filter(distrito != "general",
           organizacion_politica != "VOTOS NULOS",
           organizacion_politica != "VOTOS EN BLANCO") %>% 
    group_by(region, provincia, distrito, year) %>% 
    arrange(region, provincia, distrito, year, votos) %>% 
    mutate(percent_votos = replace_na(votos/votos_emitidos)) %>% 
    top_n(1, wt = percent_votos)

ganadores %>% 
    ggplot(aes(x = year, y = percent_votos, group = paste(provincia, distrito))) +
    geom_line(alpha = 0.4) +
    geom_point(alpha = 0.4) +
    geom_vline(xintercept = 2015, alpha = 1, color = "red") +
    scale_y_continuous(labels = scales::percent) +
    facet_wrap(~region) 

ganadores %>% 
    ggplot(aes(x = year, y = percent_votos, group = paste(provincia, distrito))) +
    geom_line(alpha = 0.4) +
    geom_point(alpha = 0.4) +
    geom_vline(xintercept = 2015, alpha = 1, color = "red") +
    scale_y_continuous(labels = scales::percent)

##

full_data %>% 
    count(tipo_municipalidad)

datos_reeleccion <- full_data %>% 
    group_by(full_name) %>% 
    mutate(
        times_elected = n(),
        reelected = times_elected > 1,
        was_alcalde = any(str_detect(cargo_electo, "ALCALDE")),
        was_regidor = any(str_detect(cargo_electo, "REGIDOR")),
        changed_district = length(unique(paste(region, provincia, distrito))) > 1
    ) %>% 
    mutate(
        elected_number_all = order_by(year, row_number(year))
    ) %>% 
    ungroup() %>% 
    group_by(full_name, tipo_municipalidad) %>% 
    mutate(
        elected_number_tipo_municipalidad = order_by(year, row_number(year))
    )

datos_reeleccion <- full_data %>% 
    group_by(full_name) %>% 
    mutate(
        times_elected = order_by(year, row_number(year))
    )

datos_reeleccion %>%
    group_by(year, changed_district, reelected) %>% 
    summarise(
        number_of_people = length(unique(full_name))
    ) %>% 
    ungroup() %>% 
    ggplot(aes(x = year, y = number_of_people)) +
    geom_point() +
    geom_line() +
    facet_wrap(~changed_district + reelected, scales = "free", ncol = 1) +
    theme_bw()

