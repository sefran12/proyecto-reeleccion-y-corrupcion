library(dplyr)
library(lubridate)
library(arrow)
library(stringr)

# Read data
read_parquet("data/02_intermediate/OSCE/merged_adjudicaciones_data.parquet")

# Calculate the price ratio
combined_df <- combined_df %>%
    mutate(Price_Ratio = Adjudicated_Value_Item / Reference_Value_Item)

# Group by entity and year
library(dplyr)
library(lubridate)

# Calculate the price ratio
combined_df <- combined_df %>%
    mutate(Price_Ratio = Adjudicated_Value_Item / Reference_Value_Item)

# Group by entity and year
grouped_df <- combined_df %>%
    mutate(Year = as.numeric(Year)) %>%
    mutate(Publication_Year = year(Publication_Date)) %>%
    filter(Publication_Year > 2009) %>% 
    group_by(Entity_RUC, Publication_Year) %>%
    summarise(
        # Entity name
        Entity = first(Entity),
        
        # Number processes:
        Number_Processes = n_distinct(Process),
        
        # Total Overall Bidders
        Total_Overall_Bidders = sum(Quantity_Convocated),
        
        # Number of single-bidder processes
        Single_Bidder_Processes = sum(Quantity_Convocated == 1, na.rm = TRUE),
        
        # % of single-bidder processes
        Single_Bidder_Percentage = Single_Bidder_Processes/sum(Quantity_Convocated, na.rm = TRUE),
        
        # Proportion of consortium winners
        Consortium_Winners = sum(Is_Consortium == "Consortium", na.rm = TRUE) / sum(!is.na(Is_Consortium)),
        
        # Average participation percentage
        Avg_Participation_Percentage = mean(Participation_Percentage, na.rm = TRUE),
        
        # Short time between publication and buenapro
        Avg_Time_Publication_Buenapro = mean(as.numeric(difftime(Buenapro_Date, Publication_Date, units = "days")), na.rm = TRUE),
        
        # Winner concentration (Herfindahl-Hirschman Index)
        HHI = sum((n() / n_distinct(Winner_RUC))^2)
    ) %>%
    # Replace NaN and Inf values with NA
    mutate(across(where(is.numeric), ~replace(., is.nan(.) | is.infinite(.), NA)))

# Print the grouped_df
grouped_df

# Extract Entity_RUCs with "MUNICIPALIDAD" in their names before 2017
muni_rucs_before_2017 <- grouped_df %>%
    filter(Publication_Year <= 2017, str_detect(Entity, "MUNICIPALIDAD")) %>%
    distinct(Entity_RUC)

# Filter grouped_df for all years using the extracted Entity_RUCs
muni_df <- grouped_df %>%
    semi_join(muni_rucs_before_2017, by = "Entity_RUC") %>%
    ungroup()

# grouped_df %>% 
#     filter(Publication_Year > 2017) %>% 
#     ungroup() %>% 
#     View()

