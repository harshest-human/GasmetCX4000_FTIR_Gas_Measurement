round_to_interval <- function(datetime, interval_sec = 450) {
        # Convert to numeric
        dt_num <- as.numeric(datetime)
        
        # Floor to nearest interval
        rounded <- floor(dt_num / interval_sec) * interval_sec
        
        # Convert back to POSIXct
        rounded_posix <- as.POSIXct(rounded, origin = "1970-01-01", tz = tz(datetime))
        
        # Identify full hours (skip NAs)
        is_full_hour <- !is.na(rounded_posix) & format(rounded_posix, "%M:%S") == "00:00"
        
        # Subtract 1 second for full-hour timestamps
        rounded_posix[is_full_hour] <- rounded_posix[is_full_hour] - 1
        
        return(rounded_posix)
}
