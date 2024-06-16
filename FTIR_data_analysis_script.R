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
#FTIR1
#raw_path           <- "D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/FTIR_1/2024-06-11_FTIR1.TXT"
#clean_path         <- "D:/Data Analysis/Gas_data/Clean_data/FTIR_clean"
#result_file_name   <- "2024-06-03_2024-06-11_FTIR1.csv"
#FTIR1_cleaned_data <- ftclean(raw_path, clean_path, result_file_name)
FTIR1_cleaned_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/2024-06-03_2024-06-11_FTIR1.csv")


#FTIR2
#raw_path           <- "D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/FTIR_2/2024-06-11_FTIR2.TXT"
#clean_path         <- "D:/Data Analysis/Gas_data/Clean_data/FTIR_clean"
#result_file_name   <- "2024-06-03_2024-06-11_FTIR2.csv"
#FTIR2_cleaned_data <- ftclean(raw_path, clean_path, result_file_name)
FTIR2_cleaned_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/2024-06-03_2024-06-11_FTIR2.csv")


######### Data combining ###########
# Convert DATE.TIME format 
FTIR1_cleaned_data$DATE.TIME = as.POSIXct(FTIR1_cleaned_data$DATE.TIME, format = "%Y-%m-%dT%H:%M:%SZ")
FTIR2_cleaned_data$DATE.TIME = as.POSIXct(FTIR2_cleaned_data$DATE.TIME, format = "%Y-%m-%dT%H:%M:%SZ")

# Filter Date by measuring campaign
start_date_time <- "2024-06-03 15:12:00"
end_date_time <- "2024-06-10 15:14:00"

FTIR1_cleaned_data <- FTIR1_cleaned_data %>% filter(DATE.TIME >= start_date_time & DATE.TIME <= end_date_time)
FTIR2_cleaned_data <- FTIR2_cleaned_data %>% filter(DATE.TIME >= start_date_time & DATE.TIME <= end_date_time)


# Set data as data.table
data.table::setDT(FTIR1_cleaned_data)
data.table::setDT(FTIR2_cleaned_data)

# Add sufffix
FTIR1_cleaned_data <- FTIR1_cleaned_data %>% rename_with(~paste0(., ".F1"), -DATE.TIME)
FTIR2_cleaned_data <- FTIR2_cleaned_data %>% rename_with(~paste0(., ".F2"), -DATE.TIME)

# Combine two dataframes by nearest times using library(data.table)
FTIR.comb <- FTIR1_cleaned_data[FTIR2_cleaned_data, on = .(DATE.TIME), roll = "nearest"]

write.csv(FTIR.comb, "FTIR.comb.csv", row.names = FALSE)

FTIR.comb <- read.csv("FTIR.comb.csv")



############ FTIR test ############
raw_path           <- "D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/FTIR_2/2024-06-11_FTIR2.TXT"
clean_path         <- "D:/Data Analysis/Gas_data/Clean_data/FTIR_clean"
result_file_name   <- "2024-05-15_2024-05-17_FTIR.test.csv"
FTIR2_test_data  <- ftclean(raw_path, clean_path, result_file_name)


FTIR2_test_data <- fread("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/2024-05-15_2024-05-17_FTIR.test.csv")
FTIR2_test_data$DATE.TIME = as.POSIXct(FTIR2_test_data$DATE.TIME, format = "%Y-%m-%dT%H:%M:%SZ")
FTIR2_test_data <- FTIR2_test_data %>% filter(DATE.TIME >= "2024-05-15 12:39:56" & DATE.TIME <= "2024-05-17 09:39:05")

# Set data as data.table
data.table::setDT(FTIR2_test_data)

# Add sufffix
FTIR2_test_data <- FTIR2_test_data %>% rename_with(~paste0(., ".F2"), -DATE.TIME)

# write
write.csv(FTIR2_test_data, "FTIR2_test.csv", row.names = FALSE)
