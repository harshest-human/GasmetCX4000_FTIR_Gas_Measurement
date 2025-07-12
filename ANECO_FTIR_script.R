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
        mutate(across(-datum_uhrzeit, ~ as.numeric(as.character(.))))


# Step 4. Convert mg/m3to ppmv
ANECO_long <- ANECO_FTIR_raw %>%
        mutate( DATE.TIME = floor_date(as.POSIXct(datum_uhrzeit, format = "%Y.%m.%d %H:%M:%S"), "hour"),
                CO2_in = co2_stall * 10000,
                CO2_S  = co2_aussen_1 * 10000,
                CO2_N  = co2_aussen_2 * 10000,
                CH4_in = ch4_stall * 24.055 / 16.04,
                CH4_S  = ch4_aussen_1 * 24.055 / 16.04,
                CH4_N  = ch4_aussen_2 * 24.055 / 16.04,
                NH3_in = nh3_stall * 24.055 / 17.03,
                NH3_S  = nh3_aussen_1 * 24.055 / 17.03,
                NH3_N  = nh3_aussen_2 * 24.055 / 17.03,
                lab = factor("ANECO"),
                analyzer = factor("FTIR.3")) 


# Step 5. Summarise by hour
ANECO_long <- ANECO_long %>%
        group_by(DATE.TIME, lab, analyzer) %>%
        summarise(
                across(
                        c(CO2_in, CO2_S, CO2_N,
                          CH4_in, CH4_S, CH4_N,
                          NH3_in, NH3_S, NH3_N),
                        ~ mean(.x, na.rm = TRUE)
                ),
                .groups = "drop"
        )


# Step 6. Write csv
ANECO_long$DATE.TIME <- format(ANECO_long$DATE.TIME, "%Y-%m-%d %H:%M:%S")
write.csv(ANECO_long,"20250408-15_long_ANECO_FTIR.3.csv" , row.names = FALSE, quote = FALSE)


# Step 7. Change Pivot to wider
ANECO_avg <- ANECO_long %>%
        pivot_longer(
                cols = -c(DATE.TIME, lab, analyzer),
                names_to = c(".value", "location"),
                names_pattern = "(.*)_(.*)"
        ) %>%
        mutate(
                location = factor(location, levels = c("N", "in", "S"))
        ) %>%
        select(DATE.TIME, location, lab, analyzer, CO2, CH4, NH3)


# Step 9. Write csv
write.csv(ANECO_avg, "20250408-15_hourly_ANECO_FTIR.3.csv", row.names = FALSE, quote = FALSE)

