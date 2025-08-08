getwd()
###### Load Library ######
library(tidyverse)
library(reshape2)
library(hablar)
library(lubridate)
library(psych)
library(ggplot2)
library(readxl)
library(dplyr)
library(ggpubr)
library(readr)
library(data.table)
source("FTIR_data_cleaning_script.R")


######### Data importing & cleaning ###########
#FTIR1
ATB_FTIR.1 = ftclean(input_path = "D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/FTIR_1/20250408-14_FTIR_1_RESULTS.TXT",
                     
                     output_path = "D:/Data Analysis/Gas_data/Clean_data/FTIR_clean",
                     
                     result_file_name = "20250408-15_Ring_7.5_cycle_ATB_FTIR1.csv",
                     
                     gas = c("CO2", "NH3", "CH4", "H2O"),
                     
                     start_time = "2025-04-08 00:00:00",
                     
                     end_time = "2025-04-15 23:59:59")


# Read in the data
ATB_FTIR.1 <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20250408-15_Ring_7.5_cycle_ATB_FTIR1.csv")

######### Post processing ##########
# Calculate hourly averages
ATB_FTIR.1 <- ATB_FTIR.1 %>% 
        mutate(CO2_ppm = CO2,
               CH4_ppm = CH4,
               NH3_ppm = NH3,
               H2O_vol = H2O,
               # Constants
               R = 8.314472,
               T = 273.15,
               P = 1013.25 * 100,
               
               CO2_mgm3 = (CO2_ppm/1000 * 44.01 * P) / (R * T),
               NH3_mgm3 = (NH3_ppm/1000 * 17.031 * P) / (R * T),
               CH4_mgm3 = (CH4_ppm/1000 * 16.04 * P) / (R * T)) %>%
        filter(Line %in% c(1, 2, 3)) %>%
        mutate(
                location = recode(as.factor(Line),
                                  `1` = "N",
                                  `2` = "in",
                                  `3` = "S"),
                lab = factor("ATB"),
                analyzer = factor("FTIR.1")) %>%
        select(DATE.TIME, location, lab, analyzer, everything())

# Calculate hourly averages
ATB_7.5_avg <- ATB_FTIR.1 %>%
        group_by(DATE.TIME, location, lab, analyzer) %>%
        summarise(CO2_ppm    = mean(CO2_ppm, na.rm = TRUE),
                  CO2_mgm3   = mean(CO2_mgm3, na.rm = TRUE),
                  CH4_ppm    = mean(CH4_ppm, na.rm = TRUE),
                  CH4_mgm3   = mean(CH4_mgm3, na.rm = TRUE),
                  NH3_ppm    = mean(NH3_ppm, na.rm = TRUE),
                  NH3_mgm3   = mean(NH3_mgm3, na.rm = TRUE),
                  H2O_vol    = mean(H2O_vol, na.rm = TRUE),
                  .groups    = "drop") 

# Write csv
ATB_7.5_avg <- ATB_7.5_avg %>% select(DATE.TIME, location, lab, analyzer, everything())
write.csv(ATB_7.5_avg,"20250408-15_ATB_7.5_avg_FTIR.1.csv" , row.names = FALSE, quote = FALSE)

# Reshape to wide format, each gas and Line combination becomes a column
ATB_wide <- ATB_7.5_avg %>%
        pivot_wider(names_from = c(location),
                    values_from = c("CO2_ppm", "CO2_mgm3", "CH4_ppm", "CH4_mgm3",
                                    "NH3_ppm", "NH3_mgm3", "H2O_vol"),
                    names_glue = "{.value}_{location}") %>%
        group_by(DATE.TIME, analyzer) %>%
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# Write csv day wise
write.csv(ATB_wide,"20250408-15_ATB_wide_FTIR.1.csv" , row.names = FALSE, quote = FALSE)
