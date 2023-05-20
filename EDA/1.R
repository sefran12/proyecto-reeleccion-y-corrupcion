library(foreign)
library(tidyverse)
library(readxl)
library(skimr)
library(clipr)

# Load the necessary libraries
library(readxl)
library(tidyverse)

# Define the directory containing the files
dir <- "C:/Users/kirul/OneDrive/Consultorias/CIES/proyecto-reeleccion-y-corrupcion/data/01_raw/reporte-adjudicaciones"

# Get a list of all the file names in the directory
file_list <- list.files(dir, full.names = TRUE)

# Initialize an empty data frame to store the results
results_df <- data.frame(filename = character(), n_rows = integer(), n_cols = integer(), columnnames = character(), stringsAsFactors = FALSE)

# Loop over the files in the list
for (file in file_list) {
    
    # Load the data from the file
    data <- read_excel(file)
    
    # Get the number of rows and columns in the data
    n_rows <- nrow(data)
    n_cols <- ncol(data)
    
    # Get the column names in the data
    column_names <- colnames(data)
    
    # Store the results in the data frame
    results_df <- rbind(results_df, data.frame(filename = file, n_rows = n_rows, n_cols = n_cols, columnnames = paste(column_names, collapse = ", ")))
}

# Show the results data frame
results_df
