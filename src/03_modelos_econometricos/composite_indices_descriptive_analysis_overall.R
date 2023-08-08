# Load necessary libraries
library(tidyverse)
library(arrow)
library(lubridate)
library(broom)
library(lavaan)
library(plm)

# Set theme
theme_set(theme_bw())

# read data
model_df <- read_parquet("data/02_intermediate/model_data_overall.parquet")
model_df_objeto <- read_parquet("data/02_intermediate/model_data_objeto.parquet")

# Summaries

summary_index <- model_df %>% 
    select(mean_indices) %>% 
    skim()

summary_index %>% 
    select(-skim_type, -n_missing, -complete_rate, -numeric.hist) %>% 
    write_clip()


summary_index_tipomuni <- model_df %>% 
    select(mean_indices, tipo_municipalidad) %>% 
    group_by(tipo_municipalidad) %>% 
    skim()

summary_index_tipomuni %>% 
    select(-skim_type, -n_missing, -complete_rate, -numeric.hist) %>% 
    write_clip()

summary_index_objeto <- model_df_objeto %>% 
    select(mean_indices, OBJETO) %>% 
    group_by(OBJETO) %>% 
    skim()

summary_index_objeto %>% 
    select(-skim_type, -n_missing, -complete_rate, -numeric.hist) %>% 
    write_clip()
