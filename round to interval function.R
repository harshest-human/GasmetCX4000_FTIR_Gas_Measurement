round_to_interval <- function(datetime, interval_sec = 450) {
        dt_num <- as.numeric(datetime)
        
        # Round down to nearest interval
        rounded <- floor(dt_num / interval_sec) * interval_sec
        
        # If exact full hour, subtract 1 second
        is_full_hour <- format(as.POSIXct(rounded, origin = "1970-01-01", tz = tz(datetime)), "%M:%S") == "00:00"
        rounded[is_full_hour] <- rounded[is_full_hour] - 1
        
        as.POSIXct(rounded, origin = "1970-01-01", tz = tz(datetime))
}
