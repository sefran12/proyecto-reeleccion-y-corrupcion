# Load necessary libraries
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(forcats)
library(lubridate)
library(stringr)
library(arrow)

# Define helper functions
clean_column_names <- function(col_names) {
    clean_names <- col_names %>%
        tolower() %>%
        str_replace_all("[^[:alnum:]_]", "_") %>%
        str_replace_all("__+", "_")
    
    # Remove empty column names
    clean_names <- clean_names[clean_names != ""]
    
    # Ensure unique column names
    clean_names <- make.unique(clean_names, sep = "_")
    
    return(clean_names)
}

process_file <- function(file_path) {
    cat("\nProcessing file:", file_path, "\n")
    
    file_extension <- tools::file_ext(file_path)
    switch(file_extension,
           csv = {data <- read_csv(file_path)},
           xlsx = {data <- read_excel(file_path)},
           xls = {data <- read_excel(file_path)},
           stop("Unsupported file type:", file_extension)
    )
    
    cat("File read successfully\n")
    
    # Find first non-empty row
    first_non_empty_row <- which.max(apply(data, 1, function(x) any(!is.na(x))))
    colnames(data) <- data[first_non_empty_row,]
    data <- data[-(1:first_non_empty_row),]
    
    cat("First non-empty row set as column names\n")
    
    # Clean column names
    colnames(data) <- clean_column_names(colnames(data))
    
    cat("Column names cleaned\n")
    
    # Parse dates and numbers saved as text
    data <- mutate_all(data, parse_guess)
    
    cat("Dates and numbers parsed\n")
    
    # Give appropriate types to columns
    data <- type_convert(data)
    
    cat("Column types converted\n")
    
    # Fill categorical NAs with "NA" category
    data <- mutate_if(data, is.factor, as.character)
    data <- mutate_all(data, function(x) ifelse(is.na(x), "NA", x))
    data <- mutate_if(data, is.character, as.factor)
    
    cat("Categorical NAs filled\n")
    
    # Eliminate all completely empty rows
    data <- drop_na(data, how = "all")
    
    cat("Empty rows eliminated\n")
    
    # Save cleaned dataset as a PARQUET file
    output_dir <- file.path(dirname(file_path), "processed")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    
    # Create a synthesized name for the output file
    file_name <- tools::file_path_sans_ext(basename(file_path))
    file_name <- paste(word(file_name, 1, 3), collapse = "_")
    output_file <- file.path(output_dir, paste0(file_name, ".parquet"))
    
    write_parquet(data, output_file)
    cat("Saved cleaned dataset as a PARQUET file:", output_file, "\n")
}

process_directory <- function(dir_path) {
    files <- list.files(dir_path, recursive = TRUE, full.names = TRUE)
    for (file_path in files) {
        if (tools::file_ext(file_path) %in% c("csv", "xls", "xlsx")) {
            tryCatch({
                process_file(file_path)
            }, error = function(e) {
                cat("Error processing file:", file_path, "\nError message:", e$message, "\n")
            })
        }
    }
}

# Set the root directory
root_dir <- "C:/Users/kirul/OneDrive/Consultorias/CIES/proyecto-reeleccion-y-corrupcion/data/01_raw/Base_de_datos_INFOgob/"

# Process the directory
process_directory(root_dir)
