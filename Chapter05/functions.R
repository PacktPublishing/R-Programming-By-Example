#
# Chapter 05 - Communicating Sales With Visualizations: Functions
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

library(ggplot2)
library(viridis)
library(ggExtra)
library(threejs)
library(leaflet)
library(plotly)
library(rgdal)
library(plyr)
library(rgl)

#
# SALES
#

# ---- functions-save-png

save_png <- function(graph, save_to, width = 480, height = 480) {
    png(save_to, width = width, height = height)
    print(graph)
    dev.off()
}

# ---- functions-add-profits

add_profits <- function(data) {
    unprofitable <- c("RETURNED", "CANCELLED", "PENDING")
    data$PROFIT <- data$PRICE - data$COST - data$DISCOUNT
    data$PROFIT[data$STATUS %in% unprofitable] <- 0
    data$PROFIT[data$PAID == "NO"] <- 0
    data$PROFIT_RATIO <- data$PROFIT / data$COST
    return(data)
}

# ---- functions-graph-bars-without-colors

graph_bars <- function(data, x, y = NULL) {
    if (is.null(y)) {
        graph <- ggplot(data, aes_string(x)) +
            geom_bar() +
            ggtitle(paste(x, "Frequency")) +
            ylab("Frequency")
    } else {
        aggregation <- get_aggregation(y)
        graph <- ggplot(data, aes_string(x, y)) +
            geom_bar(fun.y = aggregation, stat = "summary") +
            ggtitle(paste(y, "by", x))
    }
    if (class(data[, x]) == "numeric") {
        graph <- graph +
            scale_x_continuous(breaks = seq(min(data[, x]), max(data[, x])))
    }
    return(graph)
}

# ---- functions-graph-bars-with-colors

graph_bars <- function(data, x, y = NULL, color = NULL) {
    if (is.null(y)) {
        graph <- ggplot(data, aes_string(x, fill = color)) +
            geom_bar(position = "dodge") +
            ggtitle(paste(x, "Frequency")) +
            ylab("Frequency")
    } else {
        aggregation <- get_aggregation(y)
        graph <- ggplot(data, aes_string(x, y, fill = color)) +
            geom_bar(fun.y = aggregation, stat = "summary", position = "dodge") +
            ggtitle(paste(y, "by", x))
    }
    if (class(data[, x]) == "numeric") {
        graph <- graph +
            scale_x_continuous(breaks = seq(min(data[, x]), max(data[, x])))
    }
    return(graph)
}

# ---- functions-get-aggregation

get_aggregation <- function(y) {
    if (y == "PROFIT") {
        return("sum")
    }
    return("mean")
}

# ---- functions-graph-top-n-graph-bars

graph_top_n_bars <- function(data, x, n, by_profit = FALSE) {
    if (by_profit) {
        profit_by_client <- aggregate(data$PROFIT, list(data[, x]), sum)
        top_df <- profit_by_client[order(-profit_by_client$x)[1:n], ]
        top_df[, x] <- top_df$Group.1
        top_df$y_bar <- top_df$x
        y_label <- "PROFIT"
    } else {
        frequency_by_client <- sort(table(data[, x]), decreasing = TRUE)[1:n]
        top_df <- data.frame(
            aux_name = names(frequency_by_client),
            y_bar = as.numeric(frequency_by_client)
        )
        top_df[, x] <- top_df$aux_name
        y_label <- "Frequency"
    }
    return(
        ggplot(top_df, aes_string(x = x, y = "y_bar")) +
        ggtitle(paste(y_label, "by", x, "( Top", n, ")")) +
        scale_x_discrete(limits = top_df[, x]) +
        geom_bar(stat = "identity") +
        ylab(y_label)
    )
}

# ---- functions-graph-top-n-boxplots

graph_top_n_boxplots <- function(data, x, y, n, f = "#2196F3", c = "#0D47A1") {
    data <- filter_n_top(sales, n, x)
    return(
        ggplot(data, aes_string(x, y)) +
        geom_boxplot(fill = f, color = c) +
        ggtitle(paste(y, "by", x, "( Top", n, ")"))
    )
}

# ---- functions-graph-last-n-days

graph_last_n_days <- function(data, n, y = NULL, color = NULL) {
    subset        <- filter_n_days_back(data, n)
    # TODO: Fix this `get_dates_interval()` function
    # interval      <- get_dates_interval(n)
    days_range    <- paste("(last ", n, " days)", sep = "")
    date_sequence <- seq(min(subset[, "DATE"]), max(subset[, "DATE"]), by = "day")
    if (is.null(y)) {
        graph <- ggplot(subset, aes_string(x = "DATE", color = color)) +
            ggtitle(paste("Frequency", days_range))+
            geom_point(stat = "count", size = 3) +
            geom_line(stat = "count", size = 1)
    } else {
        aggregation <- get_aggregation(y)
        graph <- ggplot(subset, aes_string(x = "DATE", y = y, color = color)) +
            ggtitle(paste(y, days_range)) +
            geom_point(fun.y = aggregation, stat = "summary", size = 3) +
            geom_line(fun.y = aggregation, stat = "summary", size = 1)
    }
    graph <- graph +
        ylab(y) +
        scale_x_date(
            breaks = date_sequence,
            # TODO: Fix up
            # date_breaks = interval,
            date_labels = "%B %d, %Y"
        )
    return(graph)
}

# ---- functions-graph-marginal-distributions

graph_marginal_distributions <- function(data, x, y, color = NULL, shape = NULL) {
    if (is.null(color)) {
        if (is.null(shape)) {
            graph <- ggplot(data, aes_string(x, y))
        } else {
            graph <- ggplot(data, aes_string(x, y, shape = shape))
        }
    } else {
        if (is.null(shape)) {
            graph <- ggplot(data, aes_string(x, y, color = color))
        } else {
            graph <- ggplot(data, aes_string(x, y, color = color, shape = shape))
        }
    }
    return(ggMarginal(graph + geom_point(), type = "histogram"))
}

# ---- functions-graph-radar

graph_radar <- function(data, by) {
    data <- tidyr::gather(data, MACRO, PERCENTAGE, PROTEIN:FAT, factor_key = TRUE)
    data$CLIENT_ID <- paste(
        data$CLIENT_ID, " ($", data$PROFIT, ")", sep = ""
    )
    return(
        ggplot(data, aes(MACRO, PERCENTAGE)) +
        geom_polygon(
            aes_string(group = by, color = by, fill = by),
            alpha = 0.4,
            size = 2
        ) +
        facet_wrap(as.formula(paste("~", by)), nrow = 1) +
        guides(color = "none", fill = "none") +
        coord_radar() +
        xlab("") +
        ylab("")
    )
}

# ---- functions-coord-radar

coord_radar <- function(theta = "x", start = 0, direction = 1) {
    # theta <- match.arg(theta, c("x", "y"))
    if (theta == "x") {
        r <- "y"
    } else {
        r <- "x"
    }
    return(ggproto("CordRadar", CoordPolar, theta = theta, r = r,
                   start = start, direction = sign(direction),
                   is_linear = function(coord) { return(TRUE) }))
}

# ---- functions-filter-data

filter_data <- function(data, n_days, n_top, aggregate_by, static = TRUE) {
    data <- filter_n_days_back(data, n_days)
    data <- filter_n_top(data, n_top, aggregate_by)
    if (static) {
        aggr_profit <- aggregate(
            data[, c("PROFIT", "PROFIT_RATIO")],
            list(data[, aggregate_by]),
            sum
        )
        aggr_profit$CLIENT_ID <- aggr_profit$Group.1

        aggr_macros <- aggregate(
            data[, c("PROTEIN", "CARBS", "FAT")],
            list(data[, aggregate_by]),
            mean
        )
        aggr_macros$CLIENT_ID <- aggr_macros$Group.1

        data <- merge(aggr_profit, aggr_macros, by = aggregate_by)
        drop_columns <- c("Group.1.x", "Group.1.y", "PROFIT_RATIO")
        data <- data[, !(names(data) %in% drop_columns)]
        data <- data[order(-data$PROFIT), ]
    }
    return(data)
}

# ---- functions-filter-n-days-back

filter_n_days_back <- function(data, n) {
    if (is.null(n)) {
        return(data)
    }
    n_days_back <- Sys.Date() - n
    return(data[data[, "DATE"] >= n_days_back, ])
}

# ---- functions-filter-n-days-back-new

filter_n_days_back <- function(data, n, from_date = NULL) {
    if (is.null(n)) {
        return(data)
    }
    if (is.null(from_date)) {
        from_date <- Sys.Date()
    } else if (is.character(from_date)) {
        from_date <- as.Date(from_date)
    }
    n_days_back <- from_date - n
    return(data[data[, "DATE"] >= n_days_back, ])
}

# ---- functions-filter-n-top

filter_n_top <- function(data, n, by) {
    aggr <- aggregate(data$PROFIT, list(data[, by]), sum)
    top  <- aggr[order(-aggr[, 2])[1:n], 1]
    data <- data[data[, by] %in% top, ]
    return(data)
}

# ---- functions-graph-top-n-interactive

# TODO: Delete unnecessary functions

graph_top_n_interactive <- function(data, by) {
    data <- tidyr::gather(data, MACRO, PERCENTAGE, PROTEIN:FAT, factor_key = TRUE)
    return(
        ggplot(data, aes(MACRO, PERCENTAGE, frame = as.character(DATE))) +
        geom_point(aes_string(
            fill = by,
            color = by,
            size = "PROFIT",
            shape = "PROTEIN_SOURCE"
        )) +
        facet_wrap(as.formula(paste("~", by)), nrow = 1) +
        xlab("") +
        ylab("")
    )
}

# ---- functions-disaggregate-dates

disaggregate_dates <- function(data) {
    data$BD_YEAR  <- as.numeric(format(data$BIRTH_DATE, "%Y"))
    data$BD_MONTH <- as.numeric(format(data$BIRTH_DATE, "%m"))
    return(data)
}

# ---- functions-graph-marginal-distributions-client-birth-dates

graph_marginal_distributions_client_birth_dates <- function(data) {
    x       <- "BD_YEAR"
    y       <- "BD_MONTH"
    x_noise <- "BD_YEAR_NOISE"
    y_noise <- "BD_MONTH_NOISE"
    data    <- disaggregate_dates(data)
    data    <- add_dates_noise(data)
    graph <- ggplot(data, aes_string(x_noise,
                                     y_noise,
                                     size = "STARS",
                                     color = "GENDER")) +
        scale_x_continuous(breaks = seq(min(data[, x]), max(data[, x]), by = 5)) +
        scale_y_continuous(breaks = seq(min(data[, y]), max(data[, y]))) +
        geom_point() +
        ylab("MONTH") +
        xlab("YEAR")
    return(ggMarginal(graph, type = "histogram"))
}

# ---- functions-add-date-noise

add_dates_noise <- function(data) {
    year_noise          <- rnorm(nrow(data), sd = 0.5)
    month_noise         <- rnorm(nrow(data), sd = 0.5)
    data$BD_YEAR_NOISE  <- data$BD_YEAR + year_noise
    data$BD_MONTH_NOISE <- data$BD_MONTH + month_noise
    return(data)
}

# ---- functions-client-messages-map-static-needs-fixing

graph_client_messages_static <- function(client_messages, sales, animate = FALSE) {
    #
    # TODO: Too slow. Fix it or change example.
    #
    client_messages$MESSAGE_DATE <- as.character(client_messages$DATE)
    client_messages <- client_messages[, !(names(client_messages) == "DATE")]
    data <- merge(client_messages, sales, "SALE_ID", all.x = TRUE, all.y = FALSE)
    world_map <- filter(map_data("world"), region != "Antarctica")
    full_data <- rbind.fill(data, world_map)

    if (animate) {
        graph <- ggplot(full_data, aes(frame = MESSAGE_DATE))
    } else {
        graph <- ggplot(full_data)
    }
    return(
        graph +
        geom_polygon(
            aes(long, lat, group = group),
            color = "grey60",
            fill = "grey50"
        ) +
        geom_point(aes(LNG, LAT, color = PRICE, size = PROFIT_RATIO)) +
        scale_color_viridis(option = "inferno") +
        ylab("Latitude") +
        xlab("Longitude") +
        coord_fixed()
    )
}

# ---- functions-client-messages-map-static

graph_client_messages_static <- function(client_messages, sales) {
    data <- merge(client_messages, sales, "SALE_ID", all.x = TRUE, all.y = FALSE)
    world_map <- filter(map_data("world"), region != "Antarctica")
    return(
        ggplot() +
        geom_polygon(
            data = world_map,
            aes(long, lat, group = group),
            color = "grey60",
            fill = "grey50"
        ) +
        geom_point(
            data = data,
            aes(LNG, LAT, color = PRICE, size = PROFIT_RATIO)
        ) +
        scale_color_viridis(option = "inferno") +
        ylab("Latitude") +
        xlab("Longitude") +
        coord_fixed()
    )
}

# ---- functions-client-messages-map-interactive

graph_client_messages_interactive <- function(client_messages, sales) {
    data <- merge(client_messages, sales, "SALE_ID", all.x = TRUE, all.y = FALSE)
    data$ICON <- awesomeIcons(
        markerColor = get_icon_color(data),
        icon = get_icon(data),
        iconColor = 'white',
        library = 'ion'
    )
    return(
        addProviderTiles(addAwesomeMarkers(
            leaflet(data),
            ~LNG, ~LAT,
            icon  = ~ICON,
            label = ~paste("Profit:", PROFIT)
        ), providers$OpenStreetMap)
    )
}

# ---- functions-get-icon-color

get_icon_color <- function(data) {
    return(sapply(
        as.numeric(data$STARS),
        function(stars) {
            if (stars >= 4) {
                return("green")
            } else {
                return("red")
            }
        }
    ))
}

# ---- functions-get-icon

get_icon <- function(data) {
    return(sapply(
        as.numeric(data$STARS),
        function(stars) {
            if (stars >= 4) {
                return("ion-android-happy")
            } else {
                return("ion-android-sad")
            }
        }
    ))
}

# ---- functions-client-messages-map-globe

graph_client_messages_in_globe <- function(client_messages, sales) {
    data <- setup_globe_data(client_messages, sales)
    world_map <- get_world_map_data()
    data <- rbind.fill(data, world_map)
    return(globejs(
        lat = data$LAT,
        long = data$LNG,
        val = data$PROFIT^1.2,
        color = data$COLOR,
        pointsize = 1,
        atmosphere = TRUE
    ))
}

# ---- functions-setup-globe-data

setup_globe_data <- function(client_messages, sales) {
    data <- merge(
        client_messages,
        sales,
        "SALE_ID",
        all.x = TRUE,
        all.y = FALSE
    )
    data$COLOR <- NA
    data[data$PROTEIN_SOURCE == "BEEF", "COLOR"]       <- "#aaff00"
    data[data$PROTEIN_SOURCE == "FISH", "COLOR"]       <- "#00ffaa"
    data[data$PROTEIN_SOURCE == "CHICKEN", "COLOR"]    <- "#00aaff"
    data[data$PROTEIN_SOURCE == "VEGETARIAN", "COLOR"] <- "#0055ff"
    return(data)
}

# ---- functions-get-world-map-data

get_world_map_data <- function() {
    cache <- tempfile()
    writeBin(readBin(url(
        "http://illposed.net/nycr2015/MOD13A2_E_NDVI_2014-05-25_rgb_360x180.TIFF",
        open = "rb"
    ), what = "raw", n = 1e6
    ), con = cache
    )

    world_map <- readGDAL(cache)
    world_map <- as.data.frame(cbind(coordinates(world_map), world_map@data[,1]))
    names(world_map) <- c("LNG", "LAT", "PROFIT")

    world_map <- world_map[world_map$PROFIT < 255,]
    world_map <- na.exclude(world_map)

    world_map$PROFIT <- 1
    world_map$COLOR  <- "#0055ff"

    return(world_map)
}
