library(tidyverse)
library(lubridate)
library(stringi)
library(arrow)

# globals
regex_pattern <- "^(?:(GOBIERNO REGIONAL [\\w\\s]+?)|(?<=^)MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DEL [\\w\\s]+|[\\w\\s]+))(?: -|$)"

## LOAD DATA
df_gastos <- data.table::fread("data/01_raw/EP_Estado_Ejecucion_Gastos_2011_2020.csv")

## HELPERS
clean_gobierno <- function(df, col_name) {
    # Ensure the column exists in the data frame
    if (!(col_name %in% names(df))) stop("The specified column does not exist in the data frame.")
    
    # Extract
    df <- df %>%
        mutate(!!col_name := str_squish(!!sym(col_name)), 
               !!col_name := str_replace_all(!!sym(col_name), "/", "-"))
    
    regex_pattern <- "^(?:(GOBIERNO REGIONAL [\\w\\s]+?)|(?<=^)MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DEL [\\w\\s]+|[\\w\\s]+))(?: -|$)"
    
    df <- df %>%
        mutate(gobierno = str_extract(!!sym(col_name), regex_pattern))
    
    df$gobierno <- stri_trans_general(df$gobierno, "Latin-ASCII")
    
    # Remove Known Suffixes and Trailing Characters
    known_suffixes <- c(" RED ", " UNIDAD ", " PROGRAMA ", " ZONA ", " GERENCIA ", " SALUD ", " DIRECCION ", " UGEL ", "(?i)SEDE CENTRAL", " -$", "  -$")
    for (suffix in known_suffixes) {
        df$gobierno <- str_replace(df$gobierno, paste0(suffix, ".*$"), "")
    }
    
    # Final Cleanup
    df$gobierno <- str_squish(df$gobierno)
    df$gobierno <- str_replace_all(df$gobierno, "^(GOBIERNO REGIONAL) (DE |DEL )?", "\\1 ")
    df$gobierno <- str_replace_all(df$gobierno, "^(MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL)) (DE |DEL )?", "\\1 ")
    df$es_gobierno_subnacional = str_detect(df[[col_name]], regex_pattern)
    
    return(df)
}

clean_national_entities <- function(df, col_name) {
    return(df)
}

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
df_gastos_bs_serv <- clean_gobierno(df_gastos_bs_serv, "PLIEGO_NOMBRE")

# Calcula metricas
contrataciones_sin_proceso <- df_gastos_bs_serv %>% 
    mutate(tipo_municipalidad = case_when(str_detect(gobierno, "PROVINCIAL") ~ "provincial",
                                          str_detect(gobierno, "GOBI") ~ "regional",
                                          str_detect(gobierno, "DIS") ~ "distrital",
                                          TRUE ~ "Otra entidad")) %>% 
    group_by(ANO_EJE, gobierno, tipo_municipalidad) %>% 
    summarise(
              perc_proyectos_debajo_cutoff = mean(passes_cutoff, na.rm = TRUE),
              total_pia_value_proyectos_debajo_cutoff = sum(passes_cutoff * MONTO_PIA, na.rm = TRUE),
              total_pim_value_proyectos_debajo_cutoff = sum(passes_cutoff * MONTO_PIM, na.rm = TRUE),
              num_proyectos_debajo_cutoff = sum(passes_cutoff, na.rm = TRUE),
              perc_proyectos_sobrecosto = mean(MONTO_PIM > MONTO_PIA, na.rm = TRUE)
    )

## Calcula métricas para control nacional
contrataciones_sin_proceso_nacional <- df_gastos_bs_serv %>% 
    clean_national_entities("PLIEGO_NOMBRE") %>% 
    filter(!str_detect(PLIEGO_NOMBRE, regex_pattern)) %>% 
    group_by(ANO_EJE, PLIEGO_NOMBRE) %>% 
    summarise(
              perc_proyectos_debajo_cutoff = mean(passes_cutoff, na.rm = TRUE),
              total_pia_value_proyectos_debajo_cutoff = sum(passes_cutoff * MONTO_PIA, na.rm = TRUE),
              total_pim_value_proyectos_debajo_cutoff = sum(passes_cutoff * MONTO_PIM, na.rm = TRUE),
              num_proyectos_debajo_cutoff = sum(passes_cutoff, na.rm = TRUE),
              perc_proyectos_sobrecosto = mean(MONTO_PIM > MONTO_PIA, na.rm = TRUE)
    )

# save
#write_parquet(contrataciones_sin_proceso, "data/02_intermediate/MEF/mef_data.parquet")
