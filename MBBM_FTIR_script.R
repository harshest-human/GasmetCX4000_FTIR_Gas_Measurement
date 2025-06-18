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
# Path to Excel file
file_path <- "D:/Data Analysis/Gas_data/Raw_data/Ringversuche_2025_raw/MBBM_FTIR_raw/MBBM_FTIR_raw.xlsx"

# Function to read and clean a single sheet
read_and_clean_sheet <- function(sheet) {read_excel(file_path, sheet = sheet, .name_repair = "minimal") %>%
                janitor::remove_empty("cols") %>%  # remove entirely empty columns
                mutate(sheet_name = sheet)}

MBBM_FTIR_raw <- excel_sheets(file_path) %>%
        map_dfr(read_and_clean_sheet) 


# Create DATE.TIME column
MBBM_FTIR_raw$MEASUREMENT_PERIOD <- sub(".*-", "", MBBM_FTIR_raw$MEASUREMENT_PERIOD)
MBBM_FTIR_raw <- MBBM_FTIR_raw %>% mutate(DATE.TIME = (paste(DATE_TIME, MEASUREMENT_PERIOD))) 
MBBM_FTIR_raw <- MBBM_FTIR_raw %>% select(-MEASUREMENT_PERIOD, -DATE_TIME, -MEASUREMENT_POINT_CODE)
MBBM_FTIR_raw$DATE.TIME <- ymd_hms(MBBM_FTIR_raw$DATE.TIME)
MBBM_FTIR_raw$hour <- floor_date(MBBM_FTIR_raw$DATE.TIME, "hour")
MBBM_FTIR_raw <- MBBM_FTIR_raw %>% select(hour,LOCATION,CO2_DRY_PPM,CH4_DRY_PPM,NH3_DRY_PPM,H2O_VOL_PCT)


# Calculate hourly averages
MBBM_hourly <- MBBM_FTIR_raw %>%
        group_by(hour, LOCATION) %>%
        summarise(MBBM_CO2 = mean(CO2_DRY_PPM, na.rm = TRUE),
                  MBBM_CH4 = mean(CH4_DRY_PPM, na.rm = TRUE),
                  MBBM_NH3 = mean(NH3_DRY_PPM, na.rm = TRUE),
                  MBBM_H2O = mean(H2O_VOL_PCT, na.rm = TRUE),
                  .groups = "drop") 



reshaped_MBBM_FTIR <- MBBM_hourly %>%
        pivot_wider(names_from = LOCATION,
                    values_from = c(MBBM_CO2, MBBM_CH4, MBBM_NH3, MBBM_H2O),
                    names_glue = "{.value}_{LOCATION}")


# Write csv day wise
reshaped_MBBM_FTIR <- reshaped_MBBM_FTIR %>% filter(hour >= ymd_hms("2025-04-08 00:00:00"), hour <= ymd_hms("2025-04-15 12:59:59"))
write.csv(reshaped_MBBM_FTIR,"20250414-15_hourly_MBBM_FTIR.csv" , row.names = FALSE, quote = FALSE)

