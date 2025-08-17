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
library(tibble)
source("round to interval function.R")

######### VERSION 2 Data importing & cleaning ###########
# Path to Excel file
file_path <- "D:/Data Analysis/Gas_data/Raw_data/Ringversuche_2025_raw/ANECO_FTIR_raw/ANECO_Results_new.xlsx"

# Function to read and clean a single sheet
read_and_clean_sheet <- function(sheet) {read_excel(file_path, sheet = sheet, .name_repair = "minimal") %>%
                janitor::remove_empty("cols") %>%  # remove entirely empty columns
                janitor::clean_names() %>%         # clean column names
                mutate(sheet_name = sheet)}

# Read all sheets
ANECO_raw <- excel_sheets(file_path) %>%
        map_dfr(read_and_clean_sheet)

# Select relevant columns
ANECO_raw <- ANECO_raw %>%
        select(messstelle, datum, zeit, water_vapor_h2o, carbon_dioxide_co2, nh3, ch4) %>%
        mutate(
                DATE.TIME = ymd(datum) + hms(format(zeit, "%H:%M:%S")),
                location = messstelle,
                H2O_gm3 = water_vapor_h2o,
                NH3_mgm3 = nh3,
                CH4_mgm3 = ch4,
                CO2_vol = carbon_dioxide_co2
        ) %>%
        mutate(
                # Constants
                R = 8.314472,
                T = 273.15,
                P = 101325,  # Pa (1013.25 hPa × 100)
                
                # Conversions
                H2O_vol = (H2O_gm3* 100 * R * T) / (18.015 * P),          # g/m3 → vol%
                CO2_ppm = CO2_vol * 10000,                           # vol% → ppm
                NH3_ppm = (NH3_mgm3* 1000 * R * T) / (17.031 * P),  # mg/m3 → ppm
                CH4_ppm = (CH4_mgm3* 1000 * R * T) / (16.04 * P)    # mg/m3 → ppm
        ) %>%
        select(DATE.TIME, location, CO2_ppm, NH3_ppm, CH4_ppm, H2O_vol)


######### Post processing ##########
# Define constants
start_time <- ymd_hms("2025-04-08 12:00:00")
end_time   <- ymd_hms("2025-04-14 23:00:00")
interval_sec <- 450
flush_sec <- 180
location_cycle <- c("in", "N", "in", "S")

# Generate full time sequence
time_seq <- tibble(DATE.TIME = seq(from = start_time, to = end_time, by = "1 sec"))

# Join with raw data to fill in missing timestamps
ANECO_full <- time_seq %>%
        left_join(ANECO_raw, by = "DATE.TIME")

# Annotate each timestamp with step index and offset within that step
ANECO_full <- ANECO_full %>%
        mutate(
                step_index = floor(as.numeric(difftime(DATE.TIME, start_time, units = "secs")) / interval_sec),
                interval_start = start_time + step_index * interval_sec,
                seconds_into_step = as.numeric(DATE.TIME - interval_start),
                location = location_cycle[(step_index %% length(location_cycle)) + 1]
        )

###### 7.5 minute averaged intervals #######
# Filter to rows used for averaging (after 180s flush)
ANECO_7.5_avg <- ANECO_full %>%
        filter(seconds_into_step >= flush_sec & seconds_into_step < interval_sec) %>%
        group_by(interval_start, location) %>%
        summarise(
                DATE.TIME  = max(interval_start) + interval_sec,  # average time stamp = end of interval
                CO2_ppm    = mean(CO2_ppm, na.rm = TRUE),
                CH4_ppm    = mean(CH4_ppm, na.rm = TRUE),
                NH3_ppm    = mean(NH3_ppm, na.rm = TRUE),
                H2O_vol    = mean(H2O_vol, na.rm = TRUE),
                .groups = "drop") %>%
        select(-interval_start) %>% 
        mutate(lab = factor("ANECO"),
               analyzer = factor("FTIR.4")) %>%
        select(DATE.TIME, location, lab, analyzer, everything())


#Round DATE.TIME to the nearest 450 seconds (7.5 minutes)
ANECO_7.5_avg <- ANECO_7.5_avg %>%
        mutate(DATE.TIME = ymd_hms(DATE.TIME),
               DATE.TIME = if_else(format(DATE.TIME, "%M:%S") == "00:00",
                                   DATE.TIME - 1, DATE.TIME))

# Remove outliers 
ANECO_7.5_avg <- ANECO_7.5_avg %>% 
        remove_outliers(exclude_cols = c("DATE.TIME", "lab", "analyzer"),
                        group_cols = c("location"))

write_excel_csv(ANECO_7.5_avg,"20250408-14_ANECO_7.5_avg_FTIR.4.csv")

###### hourly averaged intervals long format #######
# Calculate hourly mean and chage pivot to long
ANECO_long <- ANECO_7.5_avg %>%
        mutate(DATE.TIME = ymd_hms(DATE.TIME)) %>%
        mutate(DATE.TIME = floor_date(DATE.TIME, unit = "hour")) %>%
        group_by(DATE.TIME, location, analyzer, lab) %>%
        summarise(CO2_ppm = mean(CO2_ppm, na.rm = TRUE),
                  CH4_ppm = mean(CH4_ppm, na.rm = TRUE),
                  NH3_ppm = mean(NH3_ppm, na.rm = TRUE),
                  H2O_vol = mean(H2O_vol, na.rm = TRUE),
                  .groups = "drop")%>%
        pivot_longer(cols = c(CO2_ppm, CH4_ppm, NH3_ppm, H2O_vol),
                     names_to = "var_unit",
                     values_to = "value")

# Write csv long
write_excel_csv(ANECO_long,"20250408-14_ANECO_long_FTIR.4.csv")       

###### hourly averaged intervals wide format #######
# Reshape to wide format, each gas and Line combination becomes a column
ANECO_wide <- ANECO_long %>%
        pivot_wider(
                names_from = c(var_unit, location),
                values_from = value,
                names_sep = "_") %>%
        arrange(DATE.TIME)

# Write csv wide
write_excel_csv(ANECO_wide,"20250408-14_ANECO_wide_FTIR.4.csv")    
