library(data.table)
library(lubridate)

####### Development of cleaning function ########
ftclean <- function(raw_path, clean_path, result_file_name) {
        # Load required libraries
        library(data.table)
        
        # Step 1: Read the TXT file using data.table's fread function
        cat("Reading raw data...\n")
        data <- fread(raw_path, header = TRUE, fill = TRUE)
        
        # Step 2: Filter out non-numeric rows in Messstelle column
        cat("Filtering non-numeric Messstelle values...\n")
        data <- data[grepl("^\\d+$", Messstelle)]
        
        # Step 3: Create DATE.TIME column
        cat("Creating DATE.TIME column...\n")
        data[, DATE.TIME := as.POSIXct(paste(Datum, Zeit), format = "%Y-%m-%d %H:%M:%S")]
        
        # Drop Datum and Zeit columns
        data[, c("Datum", "Zeit") := NULL]
        
        # Step 4: Select final columns
        cat("Selecting final columns...\n")
        data <- data[, .(DATE.TIME, Messstelle, CO2, NH3, CH4, H2O, N2O, Acetylen)]
        
        # Step 5: Convert columns to appropriate data types
        cat("Converting columns to appropriate data types...\n")
        data[, `:=`(Messstelle = as.factor(Messstelle),
                    CO2 = as.numeric(CO2),
                    NH3 = as.numeric(NH3),
                    CH4 = as.numeric(CH4),
                    H2O = as.numeric(H2O),
                    N2O = as.numeric(N2O),
                    Acetylen = as.numeric(Acetylen))]
        
        # Create the output file name
        output_file <- file.path(clean_path, result_file_name)
        
        # Write the processed data to the new CSV file
        cat("Writing processed data to CSV...\n")
        fwrite(data, file = output_file, row.names = FALSE)
        
        # Final message
        cat("Processed data has been saved as", output_file, "\n")
}






# Example usage:
#raw_path <- "D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/FTIR_1/2024-02-07_FTIR1.TXT"
#clean_path <- "D:/Data Analysis/Gas_data/Clean_data/FTIR_clean"
#result_file_name <- "2024-06-03_2024-06-11_FTIR1.csv"
#FTIR_cleaned_data <- ftclean(raw_path, clean_path, result_file_name)

