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
ATB_FTIR.1$hour <- floor_date(ATB_FTIR.1$DATE.TIME, "hour")

# Calculate the hourly average for each gas by Line
ATB_averages <- ATB_FTIR.1 %>%
        group_by(hour, Line) %>%
        summarise(
                CO2_avg = mean(CO2, na.rm = TRUE),
                CH4_avg = mean(CH4, na.rm = TRUE),
                NH3_avg = mean(NH3, na.rm = TRUE),
                H2O_avg = mean(H2O, na.rm = TRUE),
                .groups = "drop"  # To avoid warning about grouping
        )

# Reshape to wide format, each gas and Line combination becomes a column
reshaped_ATB_FTIR.1 <- ATB_averages %>%
        pivot_wider(
                names_from = Line,
                values_from = c(CO2_avg, CH4_avg, NH3_avg, H2O_avg),
                names_glue = "{.value}_MPV{Line}"
        )

# Rename columns as needed
reshaped_ATB_FTIR.1 <- reshaped_ATB_FTIR.1 %>%
        rename(
                CO2_in = CO2_avg_MPV2,
                CO2_N = CO2_avg_MPV1,
                CO2_S = CO2_avg_MPV3,
                CH4_in = CH4_avg_MPV2,
                CH4_N = CH4_avg_MPV1,
                CH4_S = CH4_avg_MPV3,
                NH3_in = NH3_avg_MPV2,
                NH3_N = NH3_avg_MPV1,
                NH3_S = NH3_avg_MPV3,
                H2O_in = H2O_avg_MPV2,
                H2O_N = H2O_avg_MPV1,
                H2O_S = H2O_avg_MPV3
        )
# Convert hour to datetime format
reshaped_ATB_FTIR.1$hour <- ymd_hms(reshaped_ATB_FTIR.1$hour)

# Write csv day wise
reshaped_ATB_FTIR.1 <- reshaped_ATB_FTIR.1 %>% filter(hour >= ymd_hms("2025-04-08 12:00:00"), hour <= ymd_hms("2025-04-15 12:59:59"))
write.csv(reshaped_ATB_FTIR.1,"20250408-15_hourly_ATB_FTIR1.csv" , row.names = FALSE, quote = FALSE)



