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
                                            "nh3_aussen_2")


ANECO_FTIR_raw <- ANECO_FTIR_raw %>%
        # Convert DATE.TIME to POSIXct with the correct format
        mutate(DATE.TIME = ymd_hms(datum_uhrzeit)) %>% 
        
        # Convert all other columns to numeric and fill NA downward
        mutate_at(vars(-DATE.TIME), ~ as.numeric(as.character(.))) %>%
        fill(-DATE.TIME, .direction = "down")

ANECO_long <- ANECO_FTIR_raw %>%
        mutate(
                DATE.TIME = floor_date(ymd_hms(DATE.TIME), "hour"),
                CO2_in_ANECO = co2_stall * 10000,
                CO2_S_ANECO  = co2_aussen_1 * 10000,
                CO2_N_ANECO  = co2_aussen_2 * 10000,
                CH4_in_ANECO = ch4_stall * 24.45 / 16.04,
                CH4_S_ANECO  = ch4_aussen_1 * 24.45 / 16.04,
                CH4_N_ANECO  = ch4_aussen_2 * 24.45 / 16.04,
                NH3_in_ANECO = nh3_stall * 24.45 / 17.03,
                NH3_S_ANECO  = nh3_aussen_1 * 24.45 / 17.03,
                NH3_N_ANECO  = nh3_aussen_2 * 24.45 / 17.03) %>%
        group_by(DATE.TIME) %>%
        summarise(across(contains("_ANECO"), mean, na.rm = TRUE), .groups = "drop") 

ANECO_long <- ANECO_long %>%
        mutate(analyzer = factor("FTIR")) 


ANECO_long <- ANECO_long %>% select(DATE.TIME, analyzer, everything())
write.csv(ANECO_long,"20250408-15_long_ANECO_FTIR.csv" , row.names = FALSE, quote = FALSE)


ANECO_avg <- ANECO_long %>%
        pivot_longer(
                cols = -c(DATE.TIME, analyzer),
                names_to = c(".value", "location"),
                names_pattern = "(.*)_(.*)_ANECO"
        ) %>%
        mutate(
                lab = factor("ANECO"),
                location = factor(location, levels = c("N", "in", "S"))
        ) %>%
        select(DATE.TIME, location, lab, analyzer, CO2, CH4, NH3)


write.csv(ANECO_avg,"20250408-15_hourly_ANECO_FTIR.csv" , row.names = FALSE, quote = FALSE)
