library(tidyverse)

reporte_20102014_muni <- reporte_20102014 %>%
    filter(
        str_detect(entidad, "MUNICIPALIDAD")
    ) 

# Numero de ganadores proyectos municipalidades (año)

# Valor total de proyectos adjudicados

# % de projectos adjudicados de postores miembros de consorcio

# Ratio entre cantidad adjudicada y cantidad convocada

reporte_20102014_muni %>% 
    group_by(entidad, ano) %>% 
    transmute(
        ratio_cantidad_pub_cantidad_convoc = case_when(
            (cant_adjudicada/cantidad_convocada < 0) ~ -99,
            (cant_adjudicada/cantidad_convocada > 999) ~ -99,
            TRUE ~ cant_adjudicada/cantidad_convocada
        )
    ) %>% ungroup() %>% skimr::skim_without_charts()

# Tiempo entre fecha de publicación y fecha de buena pro (días)

reporte_20102014_muni %>% 
    group_by(entidad, ano) %>% 
    transmute(
        dias_publicacion_buena_pro = difftime(fecha_buenapro, fecha_publicacion, units = "day")
    )
