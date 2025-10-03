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
source("remove_outliers_function.R")
source("round to interval function.R")

######### Data importing & cleaning ###########
#FTIR1
ATB_7.5_avg = ftclean(input_path = "D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/FTIR_1/20250408-14_FTIR_1_RESULTS.TXT",
                     
                     output_path = "D:/Data Analysis/Gas_data/Clean_data/FTIR_clean",
                     
                     result_file_name = "20250408-15_Ring_7.5_cycle_ATB_FTIR1.csv",
                     
                     gas = c("CO2", "NH3", "CH4", "H2O", "N2O"),
                     
                     start_time = "2025-04-08 12:00:00",
                     
                     end_time = "2025-04-14 13:00:00")


######### Post processing ##########
# Calculate hourly averages
ATB_7.5_avg <- ATB_7.5_avg %>%
        filter(Line %in% c(1, 2, 3)) %>%
        mutate(
                location = recode(as.factor(Line),
                                  `1` = "N",
                                  `2` = "in",
                                  `3` = "S"),
                lab = factor("ATB"),
                analyzer = factor("FTIR.1")) %>%
        select(DATE.TIME, location, lab, analyzer, everything())

###### 7.5 minute averaged intervals #######
ATB_7.5_avg <- ATB_7.5_avg %>%
        group_by(DATE.TIME, location, lab, analyzer) %>%
        summarise(CO2_ppm    = mean(CO2, na.rm = TRUE),
                  CH4_ppm    = mean(CH4, na.rm = TRUE),
                  NH3_ppm    = mean(NH3, na.rm = TRUE),
                  N2O_ppm = mean(N2O, na.rm = TRUE),
                  H2O_vol = mean(H2O, na.rm = TRUE),
                  .groups = "drop")

#Round DATE.TIME to the nearest 450 seconds (7.5 minutes)
ATB_7.5_avg <- ATB_7.5_avg %>%
        mutate(DATE.TIME = ymd_hms(DATE.TIME),
               DATE.TIME = round_to_interval(DATE.TIME, interval_sec = 450)) %>%
        select(DATE.TIME, location, lab, analyzer, everything())

# Remove outliers 
ATB_7.5_avg <- ATB_7.5_avg %>% 
        remove_outliers(exclude_cols = c("DATE.TIME", "lab", "analyzer"),
                        group_cols = c("location"))

# Write csv
write.csv(ATB_7.5_avg,"20250408-14_ATB_7.5_avg_FTIR.1.csv" , row.names = FALSE, quote = FALSE)


###### hourly averaged intervals long format #######
# Calculate hourly mean and chage pivot to long
ATB_long <- ATB_7.5_avg %>%
        mutate(DATE.TIME = ymd_hms(DATE.TIME)) %>%
        mutate(DATE.TIME = floor_date(DATE.TIME, unit = "hour")) %>%
        group_by(DATE.TIME, location, analyzer, lab) %>%
        summarise(CO2_ppm = mean(CO2_ppm, na.rm = TRUE),
                  CH4_ppm = mean(CH4_ppm, na.rm = TRUE),
                  NH3_ppm = mean(NH3_ppm, na.rm = TRUE),
                  N2O_ppm = mean(N2O_ppm, na.rm = TRUE),
                  H2O_vol = mean(H2O_vol, na.rm = TRUE),
                  .groups = "drop")%>%
        pivot_longer(cols = c(CO2_ppm, CH4_ppm, NH3_ppm, N2O_ppm, H2O_vol),
                     names_to = "var_unit",
                     values_to = "value")

# Write csv long
write_excel_csv(ATB_long,"20250408-14_ATB_long_FTIR.1.csv")       


###### hourly averaged intervals wide format #######
# Reshape to wide format, each gas and Line combination becomes a column
ATB_wide <- ATB_long %>%
        pivot_wider(
                names_from = c(var_unit, location),
                values_from = value,
                names_sep = "_") %>%
        arrange(DATE.TIME)

# Write csv wide
write_excel_csv(ATB_wide,"20250408-14_ATB_wide_FTIR.1.csv")    
