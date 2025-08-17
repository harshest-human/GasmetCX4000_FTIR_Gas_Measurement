round_to_interval <- function(datetime, interval_sec = 450) {
        # Convert datetime to numeric seconds since epoch
        dt_num <- as.numeric(datetime)
        
        # Floor to nearest interval
        rounded <- floor(dt_num / interval_sec) * interval_sec
        
        # Convert back to POSIXct
        rounded_posix <- as.POSIXct(rounded, origin = "1970-01-01", tz = tz(datetime))
        
        # If exact full hour (minute=0, second=0), subtract 1 second
        is_full_hour <- format(rounded_posix, "%M:%S") == "00:00"
        rounded_posix[is_full_hour] <- rounded_posix[is_full_hour] - 1
        
        return(rounded_posix)
}
