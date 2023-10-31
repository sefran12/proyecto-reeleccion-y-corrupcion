library(data.table)
library(tidyverse)
library(lubridate)

df_ingresos <- fread("data/01_raw/EP_Estado_Ejecucion_Ingresos_2011_2020.csv")
df_ingresos <- fread("~/Downloads/DATA_CIES/EP_Estado_Ejecucion_Ingresos_2011_2020.csv")


canon_petrolifero <- df_ingresos %>% 
  distinct(ESPECIFICA_DET_NOMBRE) %>% 
  filter(str_detect(ESPECIFICA_DET_NOMBRE, "PETRO"))

canon_minero <- df_ingresos %>% 
  distinct(ESPECIFICA_DET_NOMBRE) %>% 
  filter(str_detect(ESPECIFICA_DET_NOMBRE, "MINER"))

canon_gasifero <- df_ingresos %>% 
  distinct(ESPECIFICA_DET_NOMBRE) %>% 
  filter(str_detect(ESPECIFICA_DET_NOMBRE, "GAS"))

##

regex_pattern <- "^(GOBIERNO REGIONAL (?:DE )?\\w+|MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DE )?[\\w\\s]+?(?= -|- |$))"
df_ingresos_canon <- df_ingresos %>% 
  filter(
    str_detect(FUENTE_NOMBRE, "CANON Y SOBRECANON")
  )

df_ingresos_canon_gobs <- df_ingresos_canon %>% 
  mutate(
    gobierno = str_extract(PLIEGO_NOMBRE, regex_pattern),
    gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
  ) %>% 
  filter(!is.na(gobierno))

control_ingresos_canon <- df_ingresos_canon_gobs %>% 
  group_by(gobierno, ANO_EJE) %>% 
  summarise(
    total_ingresos_canon = sum(MONTO_PIM, na.rm = TRUE)
  )

# Calculate total earnings by gobierno and ANO_EJE
total_ingresos <- df_ingresos %>% 
  mutate(
    gobierno = str_extract(PLIEGO_NOMBRE, regex_pattern),
    gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
  ) %>% 
  filter(!is.na(gobierno)) %>%
  group_by(gobierno, ANO_EJE) %>% 
  summarise(
    total_ingresos = sum(MONTO_PIM, na.rm = TRUE),
  )

# Join the two data frames on gobierno and ANO_EJE
joined_ingresos <- left_join(total_ingresos, control_ingresos_canon, by = c("gobierno", "ANO_EJE"))

# Calculate the percentage
joined_ingresos <- joined_ingresos %>%
  mutate(
    tipo_municipalidad = case_when(str_detect(gobierno, "PROVINCIAL") ~ "provincial",
                                   str_detect(gobierno, "REGI") ~ "regional",
                                   str_detect(gobierno, "DIS") ~ "distrital",
                                   TRUE ~ "unknown"),
    percentage_canon = total_ingresos_canon / total_ingresos * 100,
    percentage_canon_category = case_when(
      percentage_canon >= 0 & percentage_canon <= 33 ~ "0-33%",
      percentage_canon >= 34 & percentage_canon <= 66 ~ "34-66%",
      percentage_canon >= 67 & percentage_canon <= 100 ~ "67-100%",
      TRUE ~ "Unknown" # this handles any other case (e.g., NA or values out of range 0-100)
    )
  )

library(zoo) # for as.yearmon function

# Converting ANO_EJE to Date
joined_ingresos$ANO_EJE <- as.Date(paste0(joined_ingresos$ANO_EJE, "-01-01"))

# Generate list of all first dates of the months, semesters and years within the range
monthly_dates <- seq(as.Date("2011-01-01"), as.Date("2020-12-01"), by = "month")
semestral_dates <- seq(as.Date("2011-01-01"), as.Date("2020-07-01"), by = "6 months")
yearly_dates <- seq(as.Date("2011-01-01"), as.Date("2020-01-01"), by = "year")

# Function to create expanded datasets
create_panel_ingresos <- function(dates, df){
  df$ANO_EJE <- as.Date(paste0(df$ANO_EJE, "-01-01")) # convert to Date
  panel <- df %>%
    group_by(gobierno, tipo_municipalidad) %>%
    tidyr::complete(ANO_EJE = dates) %>%
    fill(everything(), .direction = "downup") # fill down first and then up
  return(panel)
}

# Create expanded datasets
monthly_ingresos <- create_panel_ingresos(monthly_dates, joined_ingresos)
semestral_ingresos <- create_panel_ingresos(semestral_dates, joined_ingresos)
yearly_ingresos <- create_panel_ingresos(yearly_dates, joined_ingresos)

# Write data
write_parquet(monthly_ingresos, "~/Downloads/DATA_CIES/02_intermediate/controles_percentage_canon_mensual.parquet")
write_parquet(semestral_ingresos, "~/Downloads/DATA_CIES/02_intermediate/controles_percentage_canon_semestral.parquet")
write_parquet(yearly_ingresos, "~/Downloads/DATA_CIES/02_intermediate/controles_percentage_canon_anual.parquet")


# PLOTTING FOR DEBUGGING AND ANALYSIS
library(ggplot2)
library(patchwork)

theme_set(theme_bw())

# Unconditional histogram
p1 <- joined_ingresos %>%
  ggplot(aes(x = percentage_canon)) +
  geom_histogram(bins = 50, color = "black", fill = "white") +
  labs(title = "Distribution of Canon Percentage", x = "Percentage Canon", y = "Frequency", fill = "Percentage Category")

# Histogram conditional on year
p2 <- joined_ingresos %>%
  ggplot(aes(x = percentage_canon)) +
  geom_histogram(bins = 50, color = "black", fill = "white") +
  facet_wrap(~ANO_EJE, scales = "free") +
  labs(title = "Distribution of Canon Percentage by Year", x = "Percentage Canon", y = "Frequency", fill = "Percentage Category")

# Histogram conditional on tipo_municipalidad
p3 <- joined_ingresos %>%
  ggplot(aes(x = percentage_canon)) +
  geom_histogram(bins = 50, color = "black", fill = "white") +
  facet_wrap(~tipo_municipalidad, ncol = 1, scales = "free") +
  labs(title = "Distribution of Canon Percentage by Tipo Municipalidad", x = "Percentage Canon", y = "Frequency", fill = "Percentage Category")

# Use patchwork to combine plots
p1
p2
p3
