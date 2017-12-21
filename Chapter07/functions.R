#
# Chapter 07 - Developing Automatic Sales Reports : Functions
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

source("../../chapter-05/functions.R")

library(ggrepel)

# ---- functions-constants

RED   <- "#F44336"
GREEN <- "#4CAF50"

# ---- functions-proportions-table

proportions_table <- function(data, variable) {
    return(prop.table(table(data[, variable])))
}

# ---- functions-equal-length-data

equal_length_data <- function(data_1, data_2) {
    ml <- min(length(data_1), length(data_2))
    return(list(
        names = names(data_1[1:ml]),
        data_1 = as.numeric(data_1[1:ml]),
        data_2 = as.numeric(data_2[1:ml]),
        deleted = ml != length(data_1) || ml != length(data_2)
    ))
}

# ---- functions-prepare-data

prepare_data <- function(parts) {
    data            <- data.frame("Category" = parts$names)
    data$Difference <- parts$data_2 - parts$data_1
    data$Percent    <- (parts$data_2 - parts$data_1) / parts$data_1 * 100
    data$Sign       <- ifelse(data$Difference >=  0, "Positive", "Negative")
    data$Color      <- ifelse(data$Difference >=  0, GREEN, RED)
    data$Before     <- parts$data_1
    data$After      <- parts$data_2
    return(data)
}

# ---- functions-difference-bars

difference_bars <- function(data_1, data_2, before, after) {
    parts <- equal_length_data(data_1, data_2)
    data  <- prepare_data(parts)
    p <- ggplot(data, aes(Category, Difference, fill = Sign))
    p <- p + geom_bar(stat = "identity", width = 0.5)
    p <- p + scale_fill_manual(values = c("Positive" = GREEN, "Negative" = RED))
    p <- p + theme(legend.position = "none", text = element_text(size = 14))
    p <- p + scale_y_continuous(labels = scales::percent)
    p <- p + labs(title = paste(before, "vs", after))
    p <- p + labs(x = "", y = "")
    if (parts$deleted) {
        p <- p + labs(subtitle = "(Extra categories have been deleted)")
    }
    return(p)
}

# ---- functions-change-lines

change_lines <- function(data_1, data_2, before, after, x_adjustment) {
    parts <- equal_length_data(data_1, data_2)
    data  <- prepare_data(parts)
    percent_labels <- paste(round(data$Percent, 2), "%", sep = "")
    before_labels <- paste(
        data$Category, " (", round(data$Before, 2), "%)", sep = "")
    after_labels <- paste(
        data$Category, " (", round(data$After, 2), "%)", sep = "")
    percent_y <- (
        apply(data[, c("Before", "After")], 1, min) +
        abs(data$Before - data$After) / 2
    )
    p <- ggplot(data)
    p <- p + geom_segment(
                 aes(x = 1, xend = 2, y = Before, yend = After, col = Sign),
                 show.legend = FALSE,
                 size = 1.5
             )
    p <- p + scale_color_manual(values = c("Positive" = GREEN, "Negative" = RED))
    p <- p + geom_vline(xintercept = 1, linetype = "dashed", size = 0.8)
    p <- p + geom_vline(xintercept = 2, linetype = "dashed", size = 0.8)
    p <- p + geom_text(
                 label = before,
                 x = 0.7,
                 y = 1.1 * max(data$Before, data$After),
                 size = 7
             )
    p <- p + geom_text(
                 label = after,
                 x = 2.3,
                 y = 1.1 * max(data$Before, data$After),
                 size = 7
             )
    p <- p + geom_text_repel(
                 label = before_labels,
                 x = rep(1 - x_adjustment, nrow(data)),
                 y = data$Before,
                 size = 5,
                 direction = "y",
                 segment.color = NA
             )
    p <- p + geom_text_repel(
                 label = after_labels,
                 x = rep(2 + x_adjustment, nrow(data)),
                 y = data$After,
                 size = 5,
                 direction = "y",
                 segment.color = NA
             )
    p <- p + geom_text_repel(
                 label = percent_labels,
                 x = rep(1.5, nrow(data)),
                 y = percent_y,
                 col = data$Color,
                 size = 5,
                 direction = "y",
                 segment.color = NA
             )
    p <- p + theme(
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(),
                 axis.text.y = element_blank()
             )
    p <- p + ylim(0, (1.1 * max(data$Before, data$After)))
    p <- p + labs(x = "", y = "")
    p <- p + xlim(0.5, 2.5)
    return(p)
}

# ---- functions-format-tweets

format_tweets <- function(data) {
    write(paste(
        data$screenName, " (", data$created, "): \n",
        iconv(enc2utf8(substr(data$text, 1, 65)), sub = ""),
        "(...) \n", sep = ""
    ), stdout())
}
