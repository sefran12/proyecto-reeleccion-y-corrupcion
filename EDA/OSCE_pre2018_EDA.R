library(foreign)
library(tidyverse)
library(readxl)
library(skimr)

reporte_20102014 <- read_excel("data/01_raw/reporte-adjudicaciones/1. Reporte Adjudicaciones 2010-2014.xlsx")
reporte_20152017 <- read_excel("data/01_raw/reporte-adjudicaciones/2. Reporte Adjudicaciones 2015-2017.xlsx")
skim_without_charts(reporte_20102014) %>% clipr::write_clip()
skim_without_charts(reporte_20152017) %>% clipr::write_clip()
head(reporte_20102014, 3) %>% clipr::write_clip() 
head(reporte_20152017, 3) %>% clipr::write_clip() 


reporte_20102014 %>% 
    filter(
        str_detect(ENTIDAD, "MUNICIPALIDAD")
    ) %>% 
    group_by(ENTIDAD, AÑO) %>% 
    summarise(
        n = n_unique(GANADOR),
    ) %>% 
    arrange(desc(n))

reporte_20102014 %>% 
    filter(
        str_detect(ENTIDAD, "MUNICIPALIDAD")
    ) %>% 
    count(ENTIDAD, AÑO) %>% 
    arrange(desc(n)) %>% 
    mutate(AÑO = as.numeric(AÑO)) %>% 
    ggplot(
        aes(x = AÑO, y = n, group = ENTIDAD)
    ) +
    geom_line(alpha = 0.1) +
    scale_y_continuous(trans = "log1p")

reporte_20102014_muni <- reporte_20102014 %>% 
    filter(
        str_detect(ENTIDAD, "MUNICIPALIDAD")
    ) %>% 
    mutate(
        NIVEL_MUNICIPALIDAD = case_when(
            str_detect(ENTIDAD, "PROVINCIAL") ~ "PROVINCIAL",
            str_detect(ENTIDAD, "DISTRITAL") ~ "DISTRITAL",
            TRUE ~ "OTRO"
        ),
        year = as.numeric(AÑO)
    )

reporte_20102014_muni %>% 
    group_by(ENTIDAD, year) %>% 
    summarise(
        total_adjudicado = sum(`VALOR ADJUDICADO ITEM` * CANT_ADJUDICADA, na.rm = TRUE)
    ) %>% ggplot(
        aes(x = year, y = total_adjudicado, group = ENTIDAD)
    ) +
    geom_line(alpha = 0.1) +
    scale_y_continuous(trans = "log")

reporte_20152017_muni <- reporte_20152017 %>% 
    filter(
        str_detect(ENTIDAD, "MUNICIPALIDAD")
    ) %>% 
    mutate(
        NIVEL_MUNICIPALIDAD = case_when(
            str_detect(ENTIDAD, "PROVINCIAL") ~ "PROVINCIAL",
            str_detect(ENTIDAD, "DISTRITAL") ~ "DISTRITAL",
            TRUE ~ "OTRO"
        ),
        year = as.numeric(AÑO),
        valor_adjudicado_item = as.numeric(`VALOR ADJUDICADO ITEM`),
        cantidad_adjudicado_item = as.numeric(`CANTIDAD ADJUDICADO ITEM`)
    )

reporte_20152017_muni %>% 
    group_by(ENTIDAD, year) %>% 
    summarise(
        total_adjudicado = sum(valor_adjudicado_item * cantidad_adjudicado_item, na.rm = TRUE)
    ) %>% ggplot(
        aes(x = year, y = total_adjudicado, group = ENTIDAD)
    ) +
    geom_line(alpha = 0.1) +
    scale_y_continuous(trans = "log", labels = scales::comma)

