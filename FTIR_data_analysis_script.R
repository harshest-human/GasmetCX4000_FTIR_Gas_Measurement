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
raw_path           <- "D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/FTIR_1/2024_June-Oct_FTIR1.TXT"
clean_path         <- "D:/Data Analysis/Gas_data/Clean_data/FTIR_clean"
result_file_name   <- "2024_June-Sep__FTIR1.csv"
ftclean(raw_path, clean_path, result_file_name)
FTIR.1 <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/2024_June-Sep__FTIR1.csv")


#FTIR2
raw_path           <- "D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/FTIR_2/2024_June-Oct_FTIR2.TXT"
clean_path         <- "D:/Data Analysis/Gas_data/Clean_data/FTIR_clean"
result_file_name   <- "2024_June-Sep__FTIR2.csv"
ftclean(raw_path, clean_path, result_file_name)
FTIR.2 <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/2024_June-Sep__FTIR2.csv")


######### Data combining ###########
# Convert DATE.TIME format 
FTIR.1$DATE.TIME = as.POSIXct(FTIR.1$DATE.TIME, format = "%Y-%m-%dT%H:%M:%SZ")
FTIR.2$DATE.TIME = as.POSIXct(FTIR.2$DATE.TIME, format = "%Y-%m-%dT%H:%M:%SZ")

# Filter Date by measuring campaign
start_date_time <- "2024-06-30 22:00:00"
end_date_time <- "2024-07-16 16:18:00" 

FTIR.1 <- FTIR.1 %>% filter(DATE.TIME >= start_date_time & DATE.TIME <= end_date_time)
FTIR.2 <- FTIR.2 %>% filter(DATE.TIME >= start_date_time & DATE.TIME <= end_date_time)


# Set data as data.table
data.table::setDT(FTIR.1)
data.table::setDT(FTIR.2)

# Add sufffix
FTIR.1 <- FTIR.1 %>% rename_with(~paste0(., ".F1"), -DATE.TIME)
FTIR.2 <- FTIR.2 %>% rename_with(~paste0(., ".F2"), -DATE.TIME)

# Combine two dataframes by nearest times using library(data.table)
FTIR.comb <- FTIR.1[FTIR.2, on = .(DATE.TIME), roll = "nearest"]

write.csv(FTIR.comb, "2024_July_FTIR.comb.csv", row.names = FALSE)

