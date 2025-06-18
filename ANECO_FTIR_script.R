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
library(tidyr)
library(stringr)
library(purrr)
library(janitor)


######### Data importing & cleaning ###########
# Path to Excel file
file_path <- "D:/Data Analysis/Gas_data/Raw_data/Ringversuche_2025_raw/ANECO_FTIR_raw/Auswertung FTIR Aneco_060525.xlsx"

# Function to read and clean a single sheet
read_and_clean_sheet <- function(sheet) {read_excel(file_path, sheet = sheet, .name_repair = "minimal") %>%
                janitor::remove_empty("cols") %>%  # remove entirely empty columns
                janitor::clean_names() %>%         # clean column names
                mutate(sheet_name = sheet)}

ANECO_FTIR_raw <- excel_sheets(file_path) %>%
        map_dfr(read_and_clean_sheet)

ANECO_FTIR_raw <- ANECO_FTIR_raw %>% select("datum_uhrzeit",
                                            "co2_stall",
                                            "ch4_stall",
                                            "nh3_stall",
                                            "co2_aussen_1",
                                            "ch4_aussen_1",
                                            "nh3_aussen_1",
                                            "co2_aussen_2",
                                            "ch4_aussen_2",
                                            "nh3_aussen_2") %>%
        rename( DATE.TIME        = datum_uhrzeit,
                ANECO_CO2_in     = co2_stall,
                ANECO_CH4_in     = ch4_stall,
                ANECO_NH3_in     = nh3_stall,
                ANECO_CO2_S      = co2_aussen_1,
                ANECO_CH4_S      = ch4_aussen_1,
                ANECO_NH3_S      = nh3_aussen_1,
                ANECO_CO2_N      = co2_aussen_2,
                ANECO_CH4_N      = ch4_aussen_2,
                ANECO_NH3_N      = nh3_aussen_2) 

ANECO_FTIR_raw <- ANECO_FTIR_raw %>%
        # Convert DATE.TIME to POSIXct with the correct format
        mutate(DATE.TIME = ymd_hms(DATE.TIME)) %>% 
        
        # Convert all other columns to numeric and fill NA downward
        mutate_at(vars(-DATE.TIME), ~ as.numeric(as.character(.))) %>%
        fill(-DATE.TIME, .direction = "down")

ANECO_hourly <- ANECO_FTIR_raw %>%
        mutate(hour = floor_date(DATE.TIME, unit = "hour")) %>%   # round down to full hour
        group_by(hour) %>%
        summarise(across(-DATE.TIME, ~ mean(.x, na.rm = TRUE))) %>% # average all but DATE.TIME
        rename(DATE.TIME = hour)

ANECO_hourly <- ANECO_hourly %>%
        mutate( # Convert CO2 vol% to ppm
                ANECO_CO2_in = ANECO_CO2_in * 10000,
                ANECO_CO2_S  = ANECO_CO2_S * 10000,
                ANECO_CO2_N  = ANECO_CO2_N * 10000,
                
                # Convert CH4 mg/m3 to ppm
                ANECO_CH4_in = ANECO_CH4_in * 24.45 / 16.04,
                ANECO_CH4_S  = ANECO_CH4_S * 24.45 / 16.04,
                ANECO_CH4_N  = ANECO_CH4_N * 24.45 / 16.04,
                
                # Convert NH3 mg/m3 to ppm
                ANECO_NH3_in = ANECO_NH3_in * 24.45 / 17.03,
                ANECO_NH3_S  = ANECO_NH3_S * 24.45 / 17.03,
                ANECO_NH3_N  = ANECO_NH3_N * 24.45 / 17.03)

