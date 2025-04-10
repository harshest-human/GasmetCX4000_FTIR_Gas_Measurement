ftclean <- function(input_path, output_path, result_file_name, gas, start_time = NULL, end_time = NULL) {
        # Load required libraries
        library(data.table)
        library(lubridate)
        
        # Step 1: Read the TXT file using data.table's fread function
        cat("Reading raw data...\n")
        data <- fread(input_path, header = TRUE, fill = TRUE)
        
        # Step 2: Filter out non-numeric rows in Line column
        cat("Filtering non-numeric Line values...\n")
        data <- data[grepl("^\\d+$", Line)]
        
        # Step 3: Create DATE.TIME column
        cat("Creating DATE.TIME column...\n")
        data[, DATE.TIME := as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S")]
        
        # Convert DATE.TIME to the required format ("%Y-%m-%d %H:%M:%S")
        data[, DATE.TIME := format(DATE.TIME, "%Y-%m-%d %H:%M:%S")]
        
        # Drop Date and Time columns
        data[, c("Date", "Time") := NULL]
        
        # Step 4: Select final columns (include gas columns dynamically)
        cat("Selecting final columns...\n")
        selected_columns <- c("DATE.TIME", "Line", gas)
        data <- data[, ..selected_columns]
        
        # Step 5: Convert columns to appropriate data types
        cat("Converting columns to appropriate data types...\n")
        data[, `:=`(Line = as.factor(Line))]
        
        # Convert gas columns to numeric
        for (gas_column in gas) {
                data[, (gas_column) := as.numeric(get(gas_column))]
        }
        
        # Step 6: Filter by start_time and end_time if provided
        if (!is.null(start_time) && !is.null(end_time)) {
                cat("Filtering data between:\n",
                    "Start:", start_time, "\n",
                    "End:  ", end_time, "\n")
                data <- data[DATE.TIME >= as.POSIXct(start_time) &
                                     DATE.TIME <= as.POSIXct(end_time)]
        }
        
        # Step 7: Ensure the time zone is UTC and reformat the timestamp
        cat("Ensuring DATE.TIME is in UTC...\n")
        data[, DATE.TIME := as.POSIXct(DATE.TIME, tz = "UTC")]
        
        # Create the output file name
        output_file <- file.path(output_path, result_file_name)
        
        # Write the processed data to the new CSV file
        cat("Writing processed data to CSV...\n")
        fwrite(data, file = output_file, row.names = FALSE)
        
        # Final message
        cat("Processed data has been saved as", output_file, "\n")
        
        # Return the final data (not the file path)
        return(data)
}

# Example usage:
#input_path       <- "D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/FTIR_1/2025_04_08_FTIR_1_RESULTS.TXT"
#output_path      <- "D:/Data Analysis/Gas_data/Clean_data/FTIR_clean"
#result_file_name <- "2025-04-08_FTIR1_Ring.csv"
#gas              <- c("CO2", "NH3", "CH4", "H2O")
#start_time       <- "2025-04-08 12:00:00"
#end_time         <- "2025-04-10 12:30:00"

#final_data <- ftclean(input_path, output_path, result_file_name, gas, start_time, end_time)

