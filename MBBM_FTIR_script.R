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
MBBM_FTIR_raw$DATE.TIME <- floor_date(MBBM_FTIR_raw$DATE.TIME, "hour")
MBBM_FTIR_raw <- MBBM_FTIR_raw %>% select(DATE.TIME,location,CO2_DRY_PPM,CH4_DRY_PPM,NH3_DRY_PPM,H2O_VOL_PCT)


# Calculate hourly averages
MBBM_avg <- MBBM_FTIR_raw %>%
        group_by(DATE.TIME, location) %>%
        summarise(CO2 = mean(CO2_DRY_PPM, na.rm = TRUE),
                  CH4 = mean(CH4_DRY_PPM, na.rm = TRUE),
                  NH3 = mean(NH3_DRY_PPM, na.rm = TRUE),
                  H2O = mean(H2O_VOL_PCT, na.rm = TRUE),
                  .groups = "drop") %>%
        
        mutate(
                lab = factor("MBBM"),
                analyzer = factor("FTIR")
        )

# Write csv
MBBM_avg <- MBBM_avg %>% select(DATE.TIME, location, lab, analyzer, everything())
write.csv(MBBM_avg,"20250408-15_hourly_MBBM_FTIR.csv" , row.names = FALSE, quote = FALSE)

# Reshape to wide format, each gas and Line combination becomes a column
MBBM_long <- MBBM_avg %>%
        pivot_wider(
                names_from = c(location,lab),
                values_from = c(CO2, CH4, NH3, H2O),
                names_glue = "{.value}_{location}_{lab}"
        )

# Convert DATE.TIME to datetime format
MBBM_long$DATE.TIME <- ymd_hms(MBBM_long$DATE.TIME)

# Write csv day wise
write.csv(MBBM_long,"20250408-15_long_MBBM_FTIR.csv" , row.names = FALSE, quote = FALSE)

