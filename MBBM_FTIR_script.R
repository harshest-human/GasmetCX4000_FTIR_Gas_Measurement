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

MBBM_FTIR_raw <- read_excel("D:/Data Analysis/Gas_data/Raw_data/Ringversuche_2025_raw/ANECO_FTIR_raw/Auswertung FTIR Aneco_060525.xlsx")

# Get all sheet names (assumed to be dates, e.g., "08.04")
sheet_names <- excel_sheets(file_path)

# Read each sheet into a separate data frame named after the date (e.g., `data_08.04`)
for (sheet in sheet_names) {
        # Create a clean name for the data frame (e.g., "data_08_04")
        sheet_clean <- gsub("\\.", "_", sheet)
        df_name <- paste0("data_", sheet_clean)
        
        # Read the sheet (skip the first two rows with metadata)
        df <- read_excel(file_path, sheet = sheet, skip = 2)
        
        # Assign to a data frame named like "data_08_04"
        assign(df_name, df)
}
