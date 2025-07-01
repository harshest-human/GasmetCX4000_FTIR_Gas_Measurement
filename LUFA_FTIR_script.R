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
LUFA_FTIR = ftclean(input_path = "D:/Data Analysis/Gas_data/Raw_data/Ringversuche_2025_raw/LUFA_FTIR_raw/20250408-15_Ringversuch_Groß_Kreutz_LUFA.TXT",
                     
                     output_path = "D:/Data Analysis/Gas_data/Clean_data/FTIR_clean",
                     
                     result_file_name = "20250408-15_Ring_7.5_cycle_LUFA_FTIR.2.csv",
                     
                     gas = c("CO2", "NH3", "CH4", "H2O"),
                     
                     start_time = "2025-04-08 12:00:00",
                     
                     end_time = "2025-04-15 23:59:59")


# Read in the data
LUFA_FTIR <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20250408-15_Ring_7.5_cycle_LUFA_FTIR.2.csv")

# Convert DATE.TIME to datetime format
LUFA_FTIR$DATE.TIME <- as.POSIXct(LUFA_FTIR$DATE.TIME, format = "%Y-%m-%d %H:%M:%S")

# Create an hourly timestamp to group by
LUFA_FTIR$DATE.TIME <- floor_date(LUFA_FTIR$DATE.TIME, "hour")

# Calculate hourly averages
LUFA_avg <- LUFA_FTIR %>%
        group_by(DATE.TIME, Line) %>%
        summarise(CO2 = mean(CO2, na.rm = TRUE),
                  CH4 = mean(CH4, na.rm = TRUE),
                  NH3 = mean(NH3, na.rm = TRUE),
                  H2O = mean(H2O, na.rm = TRUE),
                  .groups = "drop") 

LUFA_avg <- LUFA_avg %>%
        filter(Line %in% c(1, 2, 3)) %>%
        mutate(
                location = recode(as.factor(Line),
                                  `1` = "in",
                                  `2` = "S",
                                  `3` = "N"),
                lab = factor("LUFA"),
                analyzer = factor("FTIR.2")
        )


# Write csv
LUFA_avg <- LUFA_avg %>% select(DATE.TIME, location, lab, analyzer, everything())
write.csv(LUFA_avg,"20250408-15_hourly_LUFA_FTIR.2.csv" , row.names = FALSE, quote = FALSE)

# Reshape to wide format, each gas and Line combination becomes a column
LUFA_long <- LUFA_avg %>%
        select(-Line) %>%
        pivot_wider(
                names_from = location,
                values_from = c(CO2, CH4, NH3, H2O),
                names_glue = "{.value}_{location}"
        )

# Convert DATE.TIME to datetime format
LUFA_long$DATE.TIME <- as.POSIXct(LUFA_long$DATE.TIME, format = "%d.%m.%Y %H:%M:%S")

# Write csv day wise
write.csv(LUFA_long,"20250408-15_long_LUFA_FTIR.2.csv" , row.names = FALSE, quote = FALSE)
