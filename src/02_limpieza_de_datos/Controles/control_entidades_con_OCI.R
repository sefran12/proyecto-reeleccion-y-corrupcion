# Cargamos las bibliotecas necesarias
library(lubridate)
library(tidyverse)
library(readxl)
library(stringr)
library(janitor)

# OCI data
OCI_df <- read_excel("data/01_raw/ESTADO DE ENTIDADES CON OCI INCORPORADOS AL_15FEB2023_rev (1).xlsx", sheet = 2)
OCI_df <- clean_names(OCI_df)

# Cast
# Convert 'fecha_de_incorporacion' into proper date format
OCI_df$fecha_de_incorporacion <- ifelse(
    OCI_df$fecha_de_incorporacion != "NO INCORPORADO",
    as.numeric(OCI_df$fecha_de_incorporacion),
    NA
)

# Explicitly cast to date
OCI_df$fecha_de_incorporacion <- as.Date(OCI_df$fecha_de_incorporacion, origin = "1899-12-30")

# Display unique dates to check the result
unique(OCI_df$fecha_de_incorporacion)

# Create a sequence of dates from 2010 to 2022 for each month and semester
monthly_dates <- seq(as.Date("2010-01-01"), as.Date("2022-12-31"), by = "month")
semestral_dates <- seq(as.Date("2010-01-01"), as.Date("2022-12-31"), by = "6 months")

# Create a function to generate panel data
create_panel <- function(dates){
    # Create a data frame with all combinations of entities and dates
    panel <- expand.grid(nombre_entidad = unique(OCI_df$nombre_entidad), date = dates)
    
    # Merge this with OCI_df to get OCI information for each entity and date
    panel <- panel %>%
        left_join(OCI_df %>%
                      select(nombre_entidad, fecha_de_incorporacion, fecha_creacion_oci),
                  by = "nombre_entidad")
    
    # Create flags for whether the OCI existed and was incorporated at each date
    panel <- panel %>%
        mutate(OCI_exists = ifelse(date >= fecha_creacion_oci, 1, 0),
               OCI_incorporated = ifelse(date >= fecha_de_incorporacion, 1, 0)) %>%
        replace_na(list(OCI_exists = 0, OCI_incorporated = 0))
    
    return(panel)
}

# Create monthly and semestral panel data
monthly_panel <- create_panel(monthly_dates)
semestral_panel <- create_panel(semestral_dates)
