getwd()
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


######### Data importing ###########

#FTIR1
raw_path <- "D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/FTIR_1/2024-02-07_FTIR1.TXT"
clean_path <- "D:/Data Analysis/Gas_data/Clean_data/FTIR_clean"
result_file_name <- "2024-06-03_2024-06-11_FTIR1.csv"
FTIR1_cleaned_data <- ftclean(raw_path, clean_path, result_file_name)

FTIR1_cleaned_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/2024-06-03_2024-06-11_FTIR1.csv")

#FTIR2
raw_path <- "D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/FTIR_2/2024-04-09_FTIR2.TXT"
clean_path <- "D:/Data Analysis/Gas_data/Clean_data/FTIR_clean"
result_file_name <- "2024-06-03_2024-06-11_FTIR2.csv"
FTIR2_cleaned_data <- ftclean(raw_path, clean_path, result_file_name)

FTIR2_cleaned_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/2024-06-03_2024-06-11_FTIR2.csv")


######### Data combining ###########
# Set data as data.table
data.table::setDT(FTIR1_cleaned_data)
data.table::setDT(FTIR2_cleaned_data)

FTIR1_cleaned_data$DATE.TIME = as.POSIXct(FTIR1_cleaned_data$DATE.TIME, format = "%Y-%m-%d %H:%M:%S")
FTIR2_cleaned_data$DATE.TIME = as.POSIXct(FTIR2_cleaned_data$DATE.TIME, format = "%Y-%m-%d %H:%M:%S")

FTIR1_cleaned_data <- FTIR1_cleaned_data %>% rename_with(~paste0(., ".F1"), -DATE.TIME)
FTIR2_cleaned_data <- FTIR2_cleaned_data %>% rename_with(~paste0(., ".F2"), -DATE.TIME)

# Combine two dataframes by nearest times using library(data.table)
FTIR.comb <- FTIR1_cleaned_data[FTIR2_cleaned_data, on = .(DATE.TIME), roll = "nearest"]


# Plotting using ggplot2 
ggplot(FTIR.comb, aes(x = factor(Messstelle.F1), y = CO2.F1)) +
        geom_boxplot(aes(color = "Messstelle.F1"), alpha = 0.5) +
        geom_boxplot(aes(x = factor(Messstelle.F2), y = CO2.F2, color = "Messstelle.F2"), alpha = 0.5) +
        labs(x = "Messstelle", y = "CO2 Mean") +
        scale_color_manual(values = c("Messstelle.F1" = "blue", "Messstelle.F2" = "red"), 
                           labels = c("Messstelle.F1", "Messstelle.F2")) +
        theme_minimal()

# Plotting using ggline
FTIR.comb$Messstelle.F1 <- as.factor(FTIR.comb$Messstelle.F1)
FTIR.comb$Messstelle.F2 <- as.factor(FTIR.comb$Messstelle.F2)

ggline(FTIR.comb, x = "Messstelle.F1", y = "CO2.F1",
       add = "mean_se",
       linetype = "solid",
       xlab = "Messstelle", ylab = "CO2 Mean",
       legend = "right") 

# Plotting using ggline
ggline(FTIR.comb, x = "Messstelle.F2", y = "CO2.F2",
       add = "mean_se",
       linetype = "solid",
       xlab = "Messstelle", ylab = "CO2 Mean",
       legend = "right")


############ HEAT MAP #############

# Melt data to long format for heatmap
melted_data <-melt(FTIR.comb, id.vars = c("Messstelle.F1", "Messstelle.F2"),
                   measure.vars = c("CO2.F1", "CO2.F2"),
                   variable.name = "GasType", value.name = "MeanValue")

# Plot heatmap
ggplot(melted_data, aes(x = Messstelle.F1, y = Messstelle.F2, fill = MeanValue)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high = "blue") +
        labs(x = "Messstelle.F1", y = "Messstelle.F2", fill = "Mean CO2") +
        theme_minimal()

