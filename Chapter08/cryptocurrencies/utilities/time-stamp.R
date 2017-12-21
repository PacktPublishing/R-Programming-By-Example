#
# TimeStamp (S3 Class)
#

# ---- timestamp-lubridate

library(lubridate)

# ---- timestamp-timestamp-constructor

timestamp_constructor <- function(timestamp = now.TimeStamp()) {
    class(timestamp) <- "TimeStamp"
    if (valid(timestamp)) { return(timestamp) }
    stop("Invalid timestamp (format should be: 'YYYY-MM-DD-HH-mm')")
}

# ---- timestamp-valid-timestamp

valid.TimeStamp <- function(timestamp) {
    if (gsub("-", "", gsub("[[:digit:]]", "", timestamp)) != "") {
        return(FALSE)
    }
    if (length(strsplit(timestamp, "-")[[1]]) != 5) {
        return(FALSE)
    }
    if (is.na(strptime(timestamp, "%Y-%m-%d-%H-%M"))) {
        return(FALSE)
    }
    return(TRUE)
}

# ---- timestamp-valid

valid <- function (object) {
    UseMethod("valid", object)
}

# ---- timestamp-now-timestamp

now.TimeStamp <- function() {
    timestamp <- format(Sys.time(), "%Y-%m-%d-%H-%M")
    class(timestamp) <- "TimeStamp"
    return(timestamp)
}

# ---- timestamp-time-to-timestamp-timestamp

time_to_timestamp.TimeStamp <- function(time) {
    timestamp <- format(time, "%Y-%m-%d-%H-%M")
    class(timestamp) <- "TimeStamp"
    return(timestamp)
}

# ---- timestamp-timestamp-to-time-timestamp

timestamp_to_time.TimeStamp <- function(timestamp) {
    return(strptime(timestamp, "%Y-%m-%d-%H-%M"))
}

# ---- time-to-date-string-timestamp

time_to_date_string.TimeStamp <- function(time) {
    return(format(time, "%Y-%m-%d"))
}

# ---- timestamp-to-date-string-timestamp

timestamp_to_date_string.TimeStamp <- function(timestamp) {
    return(substr(timestamp, 1, 10))
}

# ---- timestamp-subtract-timestamp

subtract.TimeStamp <- function(timestamp, interval) {
    time <- timestamp_to_time.TimeStamp(timestamp)
    if (interval == "1h") {
        time <- time - hours(1)
    } else if (interval == "1d") {
        time <- time - days(1)
    } else if (interval == "1w") {
        time <- time - weeks(1)
    } else if (interval == "1m") {
        time <- time - months(1)
    } else if (interval == "1y") {
        time <- time - years(1)
    } else {
        stop("Unknown interval")
    }
    timestamp <- time_to_timestamp.TimeStamp(time)
    return(timestamp)
}

# ---- timestamp-subtract

subtract <- function (object, interval) {
    UseMethod("subtract", object)
}

# ---- timestamp-one-year-ago-timestamp

one_year_ago.TimeStamp <- function() {
    return(subtract(now.TimeStamp(), "1y"))
}
