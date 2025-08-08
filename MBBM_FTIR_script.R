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


######### VERSION 2 Data importing & cleaning ###########
MBBM_raw <- read.delim("D:/Data Analysis/Gas_data/Raw_data/Ringversuche_2025_raw/MBBM_FTIR_raw/2025-06-02_FTIR_Ringversuche_RESULTS_MBBM.TXT")

MBBM_raw <- MBBM_raw %>% 
        mutate(DATE.TIME = ymd_hms(paste(Date, Time))) %>% 
        select(DATE.TIME,Line,CO2,CH4,NH3,H2O)

MBBM_raw <- MBBM_raw %>%
        mutate(
                CO2_ppm = as.numeric(gsub(",", ".", CO2)),
                CH4_ppm = as.numeric(gsub(",", ".", CH4)),
                NH3_ppm = as.numeric(gsub(",", ".", NH3)),
                H2O_vol = as.numeric(gsub(",", ".", H2O)),
               # Constants
               R = 8.314472,
               T = 273.15,
               P = 1013.25 * 100,
               
               CO2_mgm3 = (CO2_ppm/1000 * 44.01 * P) / (R * T),
               NH3_mgm3 = (NH3_ppm/1000 * 17.031 * P) / (R * T),
               CH4_mgm3 = (CH4_ppm/1000 * 16.04 * P) / (R * T)) %>%
        mutate(lab = factor("MBBM"),
               analyzer = factor("FTIR.3")) %>%
        select(DATE.TIME, Line, lab, analyzer, everything())

######### Post processing ##########
# Define constants
start_time <- ymd_hms("2025-04-08 12:00:00")
end_time   <- ymd_hms("2025-04-15 23:00:00")
interval_sec <- 450
flush_sec <- 180
location_cycle <- c("in", "N", "in", "S")

# Generate full time sequence
time_seq <- tibble(DATE.TIME = seq(from = start_time, to = end_time, by = "1 sec"))

# Join with raw data to fill in missing timestamps
MBBM_full <- time_seq %>%
        left_join(MBBM_raw, by = "DATE.TIME")

# Annotate each timestamp with step index and offset within that step
MBBM_full <- MBBM_full %>%
        mutate(
                step_index = floor(as.numeric(difftime(DATE.TIME, start_time, units = "secs")) / interval_sec),
                interval_start = start_time + step_index * interval_sec,
                seconds_into_step = as.numeric(DATE.TIME - interval_start),
                location = location_cycle[(step_index %% length(location_cycle)) + 1]
        )

# Filter to rows used for averaging (after 180s flush)
MBBM_7.5_avg <- MBBM_full %>%
        filter(seconds_into_step >= flush_sec & seconds_into_step < interval_sec) %>%
        group_by(interval_start, location) %>%
        summarise(
                DATE.TIME  = max(interval_start) + interval_sec,  # average time stamp = end of interval
                CO2_ppm    = mean(CO2_ppm, na.rm = TRUE),
                CO2_mgm3   = mean(CO2_mgm3, na.rm = TRUE),
                CH4_ppm    = mean(CH4_ppm, na.rm = TRUE),
                CH4_mgm3   = mean(CH4_mgm3, na.rm = TRUE),
                NH3_ppm    = mean(NH3_ppm, na.rm = TRUE),
                NH3_mgm3   = mean(NH3_mgm3, na.rm = TRUE),
                H2O_vol    = mean(H2O_vol, na.rm = TRUE),
                .groups = "drop") %>%
        select(-interval_start)


# Write 7.5 minutes averages csv
MBBM_7.5_avg <- MBBM_7.5_avg %>% 
        mutate(DATE.TIME = format(MBBM_7.5_avg$DATE.TIME, "%Y-%m-%d %H:%M:%S"),           
               lab = factor("MBBM"),
               analyzer = factor("FTIR.3")) %>%
        select(DATE.TIME, location, lab, analyzer, everything())

write_excel_csv(MBBM_7.5_avg,"20250408-15_MBBM_7.5_avg_FTIR.4.csv")


# Reshape to wide format, each gas and Line combination becomes a column
MBBM_wide <- MBBM_7.5_avg %>%
        pivot_wider(names_from = c(location),
                    values_from = c("CO2_ppm", "CO2_mgm3", "CH4_ppm", "CH4_mgm3",
                                    "NH3_ppm", "NH3_mgm3", "H2O_vol"),
                    names_glue = "{.value}_{location}") %>%
        group_by(DATE.TIME, analyzer) %>%
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# Write csv long
write_excel_csv(MBBM_wide,"20250408-15_MBBM_wide_FTIR.4.csv")       
