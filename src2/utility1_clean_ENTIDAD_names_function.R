library(stringi)
library(dplyr)
library(stringr)

# Utility function to clean government entity names
clean_gov_entity_names <- function(entity_column) {
    # Replace slashes with hyphens and remove leading/trailing whitespace
    cleaned_entities <- str_squish(entity_column)
    cleaned_entities <- str_replace_all(cleaned_entities, "/", "-")
    
    # Extract government names using a regex pattern
    regex_pattern <- "^(?:(GOBIERNO REGIONAL [\\w\\s]+?)|(?<=^)MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL) (?:DEL [\\w\\s]+|[\\w\\s]+))(?: -|$)"
    gobierno <- str_extract(cleaned_entities, regex_pattern)
    
    # Convert to Latin ASCII
    gobierno <- stri_trans_general(gobierno, "Latin-ASCII")
    
    # Remove known suffixes and trailing characters
    known_suffixes <- c(" RED ", " UNIDAD ", " PROGRAMA ", " ZONA ", " GERENCIA ", " SALUD ", " DIRECCION ", " UGEL ", "(?i)SEDE CENTRAL", " -$")
    for(suffix in known_suffixes) {
        gobierno <- str_replace(gobierno, paste0(suffix, ".*$"), "")
    }
    
    # Final Cleanup
    gobierno <- str_squish(gobierno)
    gobierno <- str_replace_all(gobierno, "^(GOBIERNO REGIONAL) (DE |DEL )?", "\\1 ")
    gobierno <- str_replace_all(gobierno, "^(MUNICIPALIDAD (?:DISTRITAL|PROVINCIAL)) (DE |DEL )?", "\\1 ")
    
    # Detect if it is a subnational government
    es_gobierno_subnacional <- str_detect(entity_column, regex_pattern)
    
    # Return a data frame with the cleaned government names and subnational flag
    return(data.frame(gobierno = gobierno, es_gobierno_subnacional = es_gobierno_subnacional))
}

# Example usage:
# Assuming 'combined_df_adjudicaciones', which is one of the base OSCE datasets
# is your original data frame and 'ENTIDAD' is the column with entity name,
# you can check the results with the following code:

data.frame(combined_df_adjudicaciones$ENTIDAD %>%
               unique(),
           clean_gov_entity_names(combined_df_adjudicaciones$ENTIDAD %>%
                                      unique())) %>%
    View()