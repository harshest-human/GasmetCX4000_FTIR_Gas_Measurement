ftclean <- function(input_path, output_path, result_file_name, gas, start_time = NULL, end_time = NULL) {
        # Load libraries
        library(data.table)
        library(lubridate)
        
        # Step 1: Read file
        cat("Reading raw data...\n")
        data <- fread(input_path, header = TRUE, fill = TRUE)
        
        
        # Step 2: Detect language (English or German) by checking for 'Date' or 'Datum'
        is_german <- "Datum" %in% names(data)
        cat("Detected language:", ifelse(is_german, "German", "English"), "\n")
        
        # Step 3: Column name mappings
        column_map <- list(
                "Datum" = "Date", "Zeit" = "Time",
                "Messstelle" = "Line",
                "Acetylen" = "Acetylene", "CO2" = "CO2", "CH4" = "CH4",
                "NH3" = "NH3", "H2O" = "H2O", "N2O" = "N2O"
        )
        
        # Rename known columns to English
        common_cols <- intersect(names(column_map), names(data))
        setnames(data, old = common_cols, new = unlist(column_map[common_cols]))
        
        # Step 4: Filter rows with numeric Line only
        cat("Filtering non-numeric Line values...\n")
        data <- data[grepl("^\\d+$", Line)]
        
        # Step 5: Create DATE.TIME column
        cat("Creating DATE.TIME column...\n")
        date_col <- if ("Date" %in% names(data)) "Date" else stop("Date/Datum column not found")
        time_col <- if ("Time" %in% names(data)) "Time" else stop("Time/Zeit column not found")
        data[, DATE.TIME := as.POSIXct(paste(get(date_col), get(time_col)),
                                       format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin")]
        
        # Drop raw Date/Time columns
        data[, c("Date", "Time") := NULL]
        
        # Step 6: Remove 'Unit' and 'Compensation' columns
        data <- data[, !grepl("^Unit$|^Compensation$", names(data), ignore.case = TRUE), with = FALSE]
        
        # Step 7: Keep only DATE.TIME, Line, and selected gases
        cat("Selecting gas columns...\n")
        available_gases <- gas[gas %in% names(data)]
        missing_gases <- setdiff(gas, names(data))
        if (length(missing_gases) > 0) {
                warning("Some gases not found in file and will be skipped: ", paste(missing_gases, collapse = ", "))
        }
        selected_columns <- c("DATE.TIME", "Line", available_gases)
        data <- data[, ..selected_columns]
        
        # Step 8: Type conversions
        cat("Converting types...\n")
        data[, Line := as.factor(Line)]
        for (g in available_gases) {
                data[, (g) := as.numeric(get(g))]
        }
        
        # Step 9: Optional time filtering
        if (!is.null(start_time) && !is.null(end_time)) {
                cat("Filtering between:\nStart:", start_time, "\nEnd:  ", end_time, "\n")
                data <- data[DATE.TIME >= as.POSIXct(start_time) &
                                     DATE.TIME <= as.POSIXct(end_time)]
        }
        
        # Step 10: Ensure DATE.TIME is in UTC
        cat("Ensuring DATE.TIME is in UTC...\n")
        data[, DATE.TIME := as.POSIXct(DATE.TIME, tz = "Europe/Berlin")]
        
        # Step 11: Save cleaned data
        output_file <- file.path(output_path, result_file_name)
        cat("Writing cleaned CSV to:\n", output_file, "\n")
        fwrite(data, file = output_file, row.names = FALSE)
        
        # Done
        cat("✅ Cleaned data saved.\n")
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

