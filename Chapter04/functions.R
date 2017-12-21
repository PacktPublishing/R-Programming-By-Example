#
# Chapter 04 - Simulating Sales Data And Working With Databases: Functions
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

library(RMySQL)

#
# SALES
#

# ---- functions-random-sales-data

random_sales_data <- function(sales, parameters) {
    n                  <- parameters[["n"]]
    n_letters          <- parameters[["n_letters"]]
    n_digits           <- parameters[["n_digits"]]
    reduction          <- parameters[["reduction"]]
    date_start         <- parameters[["date_start"]]
    date_end           <- parameters[["date_end"]]
    quantity_lambda    <- parameters[["quantity_lambda"]]
    price_mean         <- parameters[["price_mean"]]
    price_variance     <- parameters[["price_variance"]]
    cost_mean          <- parameters[["cost_mean"]]
    cost_variance      <- parameters[["cost_variance"]]
    discount_lambda    <- parameters[["discount_lambda"]]
    protein_source_pbs <- parameters[["protein_source_probabilities"]]
    continent_pbs      <- parameters[["continent_probabilities"]]
    delivery_pbs       <- parameters[["deliver_probabilities"]]
    status_pbs         <- parameters[["status_probabilities"]]
    paid_pbs           <- parameters[["paid_probabilities"]]

    set.seed(12345)

    composition = random_composition(n)

    sales <- data.frame(
        SALE_ID        = random_strings(n, n_letters, n_digits),
        CLIENT_ID      = random_strings(n, n_letters, n_digits, reduction),
        DATE           = random_dates_in_range(n, date_start, date_end),
        QUANTITY       = random_quantities(n, quantity_lambda),
        COST           = random_values(n, cost_mean, cost_variance),
        PRICE          = random_values(n, price_mean, price_variance),
        DISCOUNT       = random_discounts(n, discount_lambda),
        PROTEIN        = composition$PROTEIN,
        CARBS          = composition$CARBS,
        FAT            = composition$FAT,
        PROTEIN_SOURCE = factor(random_levels(n, levels(sales$PROTEIN_SOURCE), protein_source_pbs),
                                levels = levels(sales$PROTEIN_SOURCE)),
        CONTINENT      = factor(random_levels(n, levels(sales$CONTINENT), continent_pbs),
                                levels = levels(sales$CONTINENT)),
        DELIVERY       = factor(random_levels(n, levels(sales$DELIVERY), delivery_pbs),
                                levels = levels(sales$DELIVERY)),
        STATUS         = factor(random_levels(n, levels(sales$STATUS), status_pbs),
                                levels = levels(sales$STATUS)),
        PAID           = factor(random_levels(n, levels(sales$PAID), paid_pbs),
                                levels = levels(sales$PAID)),
        stringsAsFactors = FALSE
    )
    sales <- skew_sales_data(sales)
    return(sales)
}

# ---- functions-random-strings

random_strings <- function(n, n_letters, n_digits, reduction = 0) {
    letters <- do.call(paste0, replicate(n_letters, sample(LETTERS, n, TRUE), FALSE))
    max_number <- as.numeric(paste(replicate(n_digits, 9), collapse = ""))
    format <- paste("%0", n_digits, "d", sep = "")
    digits <- sprintf(format, sample(max_number, n, TRUE))
    ids <- paste0(letters, digits)
    if (reduction > 0) {
        ids <- sample(ids[1:floor(reduction * length(ids))], n, TRUE)
    }
    return(ids)
}

# ---- functions-random-levels

random_levels <- function(n, levels, probabilities = NULL) {
    return(sample(levels, n, TRUE, probabilities))
}

# ---- functions-random-dates-in-range

random_dates_in_range <- function(n, start, end, increasing_prob = FALSE) {
    sequence <- seq(start, end, "day")
    if (increasing_prob) {
        probabilities <- seq(1, length(sequence))^2
        probabilities <- probabilities / sum(probabilities)
        return(sample(sequence, n, TRUE, probabilities))
    } else {
        return(sample(sequence, n, TRUE))
    }
}

# ---- functions-random-quantities

random_quantities <- function(n, lambda) {
    return(rpois(n, lambda) + 1)
}

# ---- functions-random-values

random_values <- function(n, mean, variance) {
    return(round(rnorm(n, mean, sqrt(variance)), 2))
}

# ---- functions-random-discounts

random_discounts <- function(n, lambda) {
    return(round(rexp(n, lambda), 2) * 100)
}

# ---- functions-random-composition

random_composition <- function(n) {
    matrix <- t(replicate(n, random_triple(), TRUE))
    return(data.frame(
        PROTEIN = matrix[, 1],
        CARBS = matrix[, 2],
        FAT = matrix[, 3]
    ))
}

# ---- functions-random-triple

random_triple <- function() {
    a       <- runif(1, 0, 1)
    b       <- runif(1, 0, 1)
    PROTEIN <- 1 - max(a, b)
    CARBS   <- abs(a - b)
    FAT     <- min(a, b)
    return(c(PROTEIN, CARBS, FAT))
}

# ---- functions-skew-sales-data

skew_sales_data <- function(sales) {
    sales <- skew_value_by_continent(sales, "PRICE")
    sales <- skew_value_by_protein_source(sales, "PRICE")
    sales <- skew_value_by_protein_source(sales, "COST")
    return(sales)
}

# ---- functions-skew-price-by-continent

skew_value_by_continent <- function(sales, value) {
    sales[sales$CONTINENT == "AMERICA", value] <- (
        sales[sales$CONTINENT == "AMERICA", value] * 2
    )
    sales[sales$CONTINENT == "EUROPE", value] <- (
        sales[sales$CONTINENT == "EUROPE", value] * 1.5
    )
    return(sales)
}

# ---- functions-skew-value-by-protein-source

skew_value_by_protein_source <- function(sales, value) {
    if (value == "PRICE") {
        beef_multiplier       <- 2
        fish_multiplier       <- 1.7
        chicken_multiplier    <- 1
        vegetarian_multiplier <- 1.3
    } else {
        beef_multiplier       <- 1.6
        fish_multiplier       <- 1.4
        chicken_multiplier    <- 1.2
        vegetarian_multiplier <- 1
    }
    sales[sales$PROTEIN_SOURCE == "BEEF", value] <- (
        sales[sales$PROTEIN_SOURCE == "BEEF", value] * beef_multiplier
    )
    sales[sales$PROTEIN_SOURCE == "FISH", value] <- (
        sales[sales$PROTEIN_SOURCE == "FISH", value] * fish_multiplier
    )
    sales[sales$PROTEIN_SOURCE == "CHICKEN", value] <- (
        sales[sales$PROTEIN_SOURCE == "CHICKEN", value] * chicken_multiplier
    )
    sales[sales$PROTEIN_SOURCE == "VEGETARIAN", value] <- (
        sales[sales$PROTEIN_SOURCE == "VEGETARIAN", value] * vegetarian_multiplier
    )
    return(sales)
}

# ---- section-separator

#
# CLIENTS
#

# ---- functions-random-clients-data

random_clients_data <- function(clients, client_ids, parameters) {
    n         <- length(client_ids)
    bd_start  <- parameters[["birth_date_start"]]
    bd_end    <- parameters[["birth_date_end"]]
    cs_start  <- parameters[["client_since_start"]]
    cs_end    <- parameters[["client_since_end"]]
    stars_pbs <- parameters[["stars_probabilities"]]

    set.seed(12345)

    clients <- data.frame(
        CLIENT_ID    = client_ids,
        BIRTH_DATE   = random_dates_in_range(n, bd_start, bd_end, TRUE),
        CLIENT_SINCE = random_dates_in_range(n, cs_start, cs_end, TRUE),
        GENDER       = factor(random_levels(n, levels(clients$GENDER)),
                              levels = levels(clients$GENDER)),
        STARS        = factor(random_levels(n, levels(clients$STARS), stars_pbs),
                              levels = levels(clients$STARS)),
        stringsAsFactors = FALSE
    )
}

# ---- functions-random-client-messages-data

random_client_messages_data <- function(client_messages, sales, parameters) {
    n             <- parameters[["n"]]
    date_start    <- parameters[["date_start"]]
    date_end      <- parameters[["date_end"]]
    reviews_file  <- parameters[["reviews_file"]]
    locations     <- parameters[["locations"]]

    set.seed(12345)

    reviews <- random_reviews(n, reviews_file)

    # TODO: Signature changed
    # TODO: Explain `numeric(n)`

    client_messages <- data.frame(
        SALE_ID = sample(unique(sales$SALE_ID), n, TRUE),
        DATE    = random_dates_in_range(n, date_start, date_end),
        STARS   = factor(reviews$STARS, levels = levels(client_messages$STARS)),
        SUMMARY = reviews$SUMMARY,
        MESSAGE = reviews$MESSAGE,
        LAT     = numeric(n),
        LNG     = numeric(n),
        stringsAsFactors = FALSE
    )
    client_messages <- add_coordinates(client_messages, sales, locations)
    return(client_messages)
}

# ---- functions-random-reviews

random_reviews <- function(n, reviews_file) {
    reviews <- readRDS(reviews_file)
    return(reviews[sample(1:nrow(reviews), n, FALSE), ])
}

# ---- functions-add-coordinates

add_coordinates <- function(client_messages, sales, locations) {
    sale_ids <- unique(client_messages$SALE_ID)
    for (sale_id in sale_ids) {
        continent <- sales[sales$SALE_ID == sale_id, "CONTINENT"]
        coordinates <- sample(locations[[continent]], 1)
        client_messages[client_messages$SALE_ID == sale_id, "LAT"] <- (
            coordinates[[1]][["LAT"]] + rnorm(1, 0, 2)
        )
        client_messages[client_messages$SALE_ID == sale_id, "LNG"] <- (
            coordinates[[1]][["LNG"]] + rnorm(1, 0, 2)
        )
    }
    return(client_messages)
}

# ---- section-separator

#
# RELATIONAL DATABASES
#

# ---- functions-database-setup

database_setup <- function(parameters) {
    user     <- parameters[["user"]]
    host     <- parameters[["host"]]
    database <- parameters[["database"]]

    db <- dbConnect(MySQL(), user = user, host = host)
    query <- paste("CREATE DATABASE IF NOT EXISTS ", database, ";", sep = "")
    dbSendQuery(db, query)
    dbDisconnect(db)
    db <- dbConnect(MySQL(), user = user, host = host, dbname = database)
    return(db)
}

# ---- functions-database-read-with-metadata

read_table <- function(db, table) {
    data <- dbReadTable(db, table)
    if (table == "sales") {
        return(add_sales_metadata(data))
    } else if (table == "clients") {
        return(add_clients_metadata(data))
    } else if (table == "client_messages") {
        return(add_client_messages_metadata(data))
    } else {
        return(data)
    }
}

# ---- functions-database-add-metadata

add_sales_metadata <- function(data) {
    status_levels         <- c("PENDING", "DELIVERED", "RETURNED", "CANCELLED")
    protein_source_levels <- c("BEEF", "FISH", "CHICKEN", "VEGETARIAN")
    continent_levels      <- c("AMERICA", "EUROPE", "ASIA")
    delivery_levels       <- c("IN STORE", "TO LOCATION")
    paid_levels           <- c("YES", "NO")
    data$DATE             <- as.Date(data$DATE)
    data$PROTEIN_SOURCE   <- factor(data$PROTEIN_SOURCE, levels = protein_source_levels)
    data$CONTINENT        <- factor(data$CONTINENT, levels = continent_levels)
    data$DELIVERY         <- factor(data$DELIVERY, levels = delivery_levels)
    data$STATUS           <- factor(data$STATUS, levels = status_levels)
    data$PAID             <- factor(data$PAID, levels = paid_levels)
    return(data)
}

add_clients_metadata <- function(data) {
    gender_levels     <- c("FEMALE", "MALE")
    star_levels       <- c("1", "2", "3", "4", "5")
    data$BIRTH_DATE   <- as.Date(data$BIRTH_DATE)
    data$CLIENT_SINCE <- as.Date(data$CLIENT_SINCE)
    data$GENDER       <- factor(data$GENDER, levels = gender_levels)
    data$STARS        <- factor(data$STARS, levels = star_levels)
    return(data)
}

add_client_messages_metadata <- function(data) {
    star_levels <- c("1", "2", "3", "4", "5")
    data$DATE   <- as.Date(data$DATE)
    data$STARS  <- factor(data$STARS, levels = star_levels)
    return(data)
}
