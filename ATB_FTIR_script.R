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
ATB_FTIR.1 <- fread("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20250408-15_Ring_7.5_cycle_ATB_FTIR1.csv")

# Convert DATE.TIME to datetime format
ATB_FTIR.1$DATE.TIME <- ymd_hms(ATB_FTIR.1$DATE.TIME)

# Create an hourly timestamp to group by
ATB_FTIR.1$DATE.TIME <- floor_date(ATB_FTIR.1$DATE.TIME, "hour")

# Calculate hourly averages
ATB_avg <- ATB_FTIR.1 %>%
        group_by(DATE.TIME, Line) %>%
        summarise(CO2 = mean(CO2, na.rm = TRUE),
                  CH4 = mean(CH4, na.rm = TRUE),
                  NH3 = mean(NH3, na.rm = TRUE),
                  H2O = mean(H2O, na.rm = TRUE),
                  .groups = "drop") 

ATB_avg <- ATB_avg %>%
        filter(Line %in% c(1, 2, 3)) %>%
        mutate(
                location = recode(as.factor(Line),
                                  `1` = "N",
                                  `2` = "in",
                                  `3` = "S"),
                lab = factor("ATB"),
                analyzer = factor("FTIR.1")
        )


# Write csv
ATB_avg <- ATB_avg %>% select(DATE.TIME, location, lab, analyzer, everything())
write.csv(ATB_avg,"20250408-15_hourly_ATB_FTIR.1.csv" , row.names = FALSE, quote = FALSE)

# Reshape to wide format, each gas and Line combination becomes a column
ATB_long <- ATB_avg %>%
        select(-Line) %>%
        pivot_wider(
                names_from = location,
                values_from = c(CO2, CH4, NH3, H2O),
                names_glue = "{.value}_{location}"
        )

# Convert DATE.TIME to datetime format
ATB_long$DATE.TIME <- as.POSIXct(ATB_long$DATE.TIME, format = "%d.%m.%Y %H:%M:%S")

# Write csv day wise
write.csv(ATB_long,"20250408-15_long_ATB_FTIR.1.csv" , row.names = FALSE, quote = FALSE)
