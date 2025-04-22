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
FTIR.1 = ftclean(input_path = "D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/FTIR_1/20250408-14_FTIR_1_RESULTS.TXT",
                 
                 output_path = "D:/Data Analysis/Gas_data/Clean_data/FTIR_clean",
                 
                 result_file_name = "20250408-15_Ring_7.5_cycle_FTIR1.csv",
                 
                 gas = c("CO2", "NH3", "CH4", "H2O"),
                 
                 start_time = "2025-04-08 00:00:00",
                 
                 end_time = "2025-04-15 23:59:59")


# Read in the data
FTIR.1 <- fread("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20250408-15_Ring_7.5_cycle_FTIR1.csv")

# Convert DATE.TIME to datetime format
FTIR.1$DATE.TIME <- ymd_hms(FTIR.1$DATE.TIME)

# Create an hourly timestamp to group by
FTIR.1$hour <- floor_date(FTIR.1$DATE.TIME, "hour")

# Calculate the hourly average for each gas by Line
averages <- FTIR.1 %>%
        group_by(hour, Line) %>%
        summarise(
                CO2_avg = mean(CO2, na.rm = TRUE),
                CH4_avg = mean(CH4, na.rm = TRUE),
                NH3_avg = mean(NH3, na.rm = TRUE),
                H2O_avg = mean(H2O, na.rm = TRUE),
                .groups = "drop"  # To avoid warning about grouping
        )

# Reshape to wide format, each gas and Line combination becomes a column
reshaped_FTIR.1 <- averages %>%
        pivot_wider(
                names_from = Line,
                values_from = c(CO2_avg, CH4_avg, NH3_avg, H2O_avg),
                names_glue = "{.value}_MPV{Line}"
        )

# Rename columns as needed
reshaped_FTIR.1 <- reshaped_FTIR.1 %>%
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
reshaped_FTIR.1$hour <- ymd_hms(reshaped_FTIR.1$hour)

# Write csv day wise
reshaped_FTIR_08_09 <- reshaped_FTIR.1 %>% filter(hour >= ymd_hms("2025-04-08 12:00:00"), hour <= ymd_hms("2025-04-09 23:00:00"))
write.csv(reshaped_FTIR_08_09,"20250408-09_hourly_FTIR1.csv" , row.names = FALSE, quote = FALSE)


reshaped_FTIR_10_11 <- reshaped_FTIR.1 %>% filter(hour >= ymd_hms("2025-04-10 00:00:00"), hour <= ymd_hms("2025-04-11 23:00:00"))
write.csv(reshaped_FTIR_10_11,"20250410-11_hourly_FTIR1.csv" , row.names = FALSE, quote = FALSE)


reshaped_FTIR_12_13 <- reshaped_FTIR.1 %>% filter(hour >= ymd_hms("2025-04-12 00:00:00"), hour <= ymd_hms("2025-04-13 23:00:00"))
write.csv(reshaped_FTIR_12_13,"20250412-13_hourly_FTIR1.csv" , row.names = FALSE, quote = FALSE)


reshaped_FTIR_14_15 <- reshaped_FTIR.1 %>% filter(hour >= ymd_hms("2025-04-14 00:00:00"), hour <= ymd_hms("2025-04-15 23:00:00"))
write.csv(reshaped_FTIR_14_15,"20250414-15_hourly_FTIR1.csv" , row.names = FALSE, quote = FALSE)

