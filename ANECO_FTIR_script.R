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


######### Data importing & cleaning ###########
# Path to Excel file
file_path <- "D:/Data Analysis/Gas_data/Raw_data/Ringversuche_2025_raw/ANECO_FTIR_raw/Auswertung FTIR Aneco_060525.xlsx"

# Function to read and clean a single sheet
read_and_clean_sheet <- function(sheet) {read_excel(file_path, sheet = sheet, .name_repair = "minimal") %>%
                janitor::remove_empty("cols") %>%  # remove entirely empty columns
                janitor::clean_names() %>%         # clean column names
                mutate(sheet_name = sheet)}

# Step 1. Read all sheets
ANECO_FTIR_raw <- excel_sheets(file_path) %>%
        map_dfr(read_and_clean_sheet)


# Step 2. Select relevant columns
ANECO_FTIR_raw <- ANECO_FTIR_raw %>% select(
        datum_uhrzeit,
        co2_stall, ch4_stall, nh3_stall,
        co2_aussen_1, ch4_aussen_1, nh3_aussen_1,
        co2_aussen_2, ch4_aussen_2, nh3_aussen_2)


# Step 3. Convert numeric columns
ANECO_FTIR_raw <- ANECO_FTIR_raw %>%
        mutate(across(-datum_uhrzeit, ~ as.numeric(as.character(.))))%>%
        mutate(DATE.TIME = floor_date(as.POSIXct(datum_uhrzeit, format = "%Y.%m.%d %H:%M:%S"), "hour"))

# Step 4: Convert units and add metadata
ANECO_FTIR_raw <- ANECO_FTIR_raw %>%
        mutate(
                CO2_in = co2_stall * 10000 * 37.2,      # M = 44.01 g/mol * T =(273 / (273 + 50°C)) * P = (1013 / 1013)  # 37.2 approx
                CO2_S  = co2_aussen_1 * 10000 * 37.2,          # M = 44.01 g/mol * T =(273 / (273 + 50°C)) * P = (1013 / 1013)  # 37.2 approx
                CO2_N  = co2_aussen_2 * 10000 * 37.2,         # M = 44.01 g/mol * T =(273 / (273 + 50°C)) * P = (1013 / 1013)  # 37.2 approx
                CH4_in = ch4_stall,              
                CH4_S  = ch4_aussen_1,           
                CH4_N  = ch4_aussen_2,           
                NH3_in = nh3_stall,              
                NH3_S  = nh3_aussen_1,           
                NH3_N  = nh3_aussen_2,           
                lab = factor("ANECO"),
                analyzer = factor("FTIR.4")
        )


# Step 5: Reshape to long format
ANECO_avg <- ANECO_FTIR_raw %>%
        pivot_longer(
                cols = matches("^(CO2|CH4|NH3)_(in|S|N)$"),
                names_to = c(".value", "location"),
                names_pattern = "(.*)_(.*)") %>%
        mutate(location = factor(location, levels = c("N", "in", "S"))
        )%>%
        group_by(DATE.TIME, location, lab, analyzer) %>%
        summarise(across(c(CO2, CH4, NH3), ~ mean(.x, na.rm = TRUE)), .groups = "drop")


# Step 7. Write hourly averages csv
ANECO_avg <- ANECO_avg %>% 
        mutate(DATE.TIME = format(ANECO_avg$DATE.TIME, "%Y-%m-%d %H:%M:%S")) %>%
        select(DATE.TIME, location, lab, analyzer, CO2, CH4, NH3)
                
write.csv(ANECO_avg,"20250408-15_hourly_ANECO_FTIR.4.csv" , row.names = FALSE, quote = FALSE)


# Step 7.Reshape to wide format, each gas and Line combination becomes a column
ANECO_long <- ANECO_avg %>%
        pivot_wider(
                names_from = c(location),
                values_from = c(CO2, CH4, NH3),
                names_glue = "{.value}_{location}"
        )

# Step 8. Write csv long
write.csv(ANECO_long,"20250408-15_long_ANECO_FTIR.4.csv" , row.names = FALSE, quote = FALSE)




######### VERSION 2 Data importing & cleaning ###########
# Path to Excel file
file_path <- "D:/Data Analysis/Gas_data/Raw_data/Ringversuche_2025_raw/ANECO_FTIR_raw/ANECO_Results_new.xlsx"

# Function to read and clean a single sheet
read_and_clean_sheet <- function(sheet) {read_excel(file_path, sheet = sheet, .name_repair = "minimal") %>%
                janitor::remove_empty("cols") %>%  # remove entirely empty columns
                janitor::clean_names() %>%         # clean column names
                mutate(sheet_name = sheet)}

# Read all sheets
ANECO_FTIR_raw <- excel_sheets(file_path) %>%
        map_dfr(read_and_clean_sheet)


# Select relevant columns
ANECO_FTIR_raw <- ANECO_FTIR_raw %>%
        select(messstelle, datum, zeit, water_vapor_h2o, carbon_dioxide_co2, nh3, ch4)%>%
        # Merge datum and zeit into DATE.TIME and convert to POSIXct
        mutate(
                DATE.TIME = ymd(datum) + hms(format(zeit, "%H:%M:%S")),
                
                # Rename and clean gas concentration variables
                location = messstelle,
                H2O = water_vapor_h2o,
                CO2 = carbon_dioxide_co2 * 10000 * 37.2,
                NH3 = nh3,
                CH4 = ch4
        ) %>%
        # Select only the needed columns
        select(DATE.TIME, location, H2O, CO2, NH3, CH4)


ANECO_FTIR_filtered <- ANECO_FTIR_raw %>%
        filter(
                DATE.TIME >= ymd_hms("2025-04-08 12:00:00") &
                        DATE.TIME <= ymd_hms("2025-04-14 10:00:00")
        )

# Define pattern of locations
locations <- c("Ring inside", "North outside", "Ring inside", "South outside")

# Start time for interval indexing (use min DATE.TIME in your data)
start_time <- min(ANECO_FTIR_filtered$DATE.TIME)

# Add cyclic messstelle based on 7.5-minute steps
ANECO_avg <- ANECO_FTIR_filtered %>%
        mutate(
                step_index = floor(as.numeric(difftime(DATE.TIME, start_time, units = "secs")) / 450),
                location = locations[(step_index %% length(locations)) + 1]
        )%>%
        mutate(DATE.TIME = floor_date(DATE.TIME, unit = "hour")) %>%
        group_by(DATE.TIME, location)%>%
        summarise(
                CO2 = mean(CO2, na.rm = TRUE),
                CH4 = mean(CH4, na.rm = TRUE),
                NH3 = mean(NH3, na.rm = TRUE),
                .groups = "drop"
        )

# Write hourly averages csv
ANECO_avg <- ANECO_avg %>% 
        mutate(DATE.TIME = format(ANECO_avg$DATE.TIME, "%Y-%m-%d %H:%M:%S"),           
               lab = factor("ANECO"),
               analyzer = factor("FTIR.4")) %>%
        select(DATE.TIME, location, lab, analyzer, CO2, CH4, NH3)

write_excel_csv(ANECO_avg,"20250408-15_hourly_v2_ANECO_FTIR.4.csv")
