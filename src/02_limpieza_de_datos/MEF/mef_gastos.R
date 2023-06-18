library(data.table)
library(tidyverse)
library(lubridate)

df_gastos <- fread("data/01_raw/EP_Estado_Ejecucion_Gastos_2011_2020.csv")

## Reproduce informe
# Create the vectors for the UIT data
AÑO <- c(2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010)
VALOR_UIT <- c(4.95, 4.6, 4.4, 4.3, 4.2, 4.15, 4.05, 3.95, 3.85, 3.8, 3.7, 3.65, 3.6, 3.6)
uit_value <- data.frame(year = AÑO, uit_value = VALOR_UIT)

# Create vectors for cutoff data
mesanho <- seq(as.Date("2010-01-01"), as.Date("2022-12-01"), by = "month")
uit_cutoff <- rep(3, length(mesanho))
uit_cutoff[mesanho < as.Date("2014-07-01")] <- 3

# Set values from July 2014 to December 2022 to 8
uit_cutoff[mesanho >= as.Date("2014-07-01")] <- 8

# Create the dataframe
uit_cutoff <- data.frame(mesanho = mesanho, uit_cutoff = uit_cutoff)

# Merge
# Expand uit_value to include a row for each month of each year
uit_value_monthly <- uit_value %>%
    mutate(year = as.Date(paste(year, 1, 1, sep = "-"))) %>%
    complete(year = seq.Date(min(year), max(year), by = "month")) %>%
    fill(uit_value)

# Merge with uit_cutoff
uit_data <- left_join(uit_cutoff, uit_value_monthly, by = c("mesanho" = "year"))

# Create value_in_uit
uit_data <- uit_data %>%
    mutate(value_in_uit_thousand = uit_cutoff * uit_value,
           value_in_uit = value_in_uit_thousand * 1000)

# 1. Selecciona orden de compra o servicio
df_gastos <- df_gastos %>% 
    mutate(
        mesanho = as.Date(paste(ANO_EJE, 1, 1, sep = "-"), format="%Y-%m-%d")
    ) %>% 
    left_join(uit_data, by = "mesanho") %>% 
    mutate(
        passes_cutoff = MONTO_PIA < value_in_uit_thousand
    )

# desestima servicio de deuda, CAS y otros gastos.
df_gastos_bs_serv <- df_gastos %>% 
    filter(GENERICA_NOMBRE == "BIENES Y SERVICIOS")

# Filtra a gobiernos relevantes
regex_pattern <- "^(GOBIERNO REGIONAL (?:DE )?\\w+|MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DE )?[\\w\\s]+?(?= -|- |$))"
df_gastos_bs_serv <- df_gastos_bs_serv %>% 
    mutate(
        gobierno = str_extract(PLIEGO_NOMBRE, regex_pattern),
        gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central")
    ) %>% 
    filter(!is.na(gobierno))

#2. 
contrataciones_sin_proceso <- df_gastos_bs_serv %>% 
    group_by(ANO_EJE, gobierno) %>% 
    summarise(
        perc_proyectos_debajo_cutoff = mean(passes_cutoff, na.rm = TRUE),
        total_pia_value_proyectos_debajo_cutoff = sum(passes_cutoff * MONTO_PIA, na.rm = TRUE),
        num_proyectos_debajo_cutoff = sum(passes_cutoff, na.rm = TRUE),
        perc_proyectos_sobrecosto = mean(MONTO_PIM > MONTO_PIA, na.rm = TRUE)
    )

# save
write_parquet(contrataciones_sin_proceso, "data/02_intermediate/MEF/mef_data.parquet")

# Segmentar por municipalidades estables y no estables (fragmentación)
# quedarte con muni con las cuales estaban debajo de la mediana de fragmentacion

 