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


MBBM_FTIR_raw$MEASUREMENT_PERIOD <- sub(".*-", "", MBBM_FTIR_raw$MEASUREMENT_PERIOD)
