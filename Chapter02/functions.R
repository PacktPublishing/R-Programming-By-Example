#
# Chapter 02 - Understanding Votes With Descriptive Statistics: Functions
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

library(viridis)
library(ggplot2)
library(ggbiplot)
library(corrplot)
library(progress)

source("../chapter-01/functions.R")

prepare_data <- function(path, complete_cases = FALSE) {
    data <- read.csv(path)
    data <- clean_data(data)
    data <- transform_data(data)
    if (complete_cases) {
        data <- data[complete.cases(data), ]
    }
    return(data)
}

clean_data <- function(data) {
    data[data$Leave == -1, "Leave"] <- NA
    return(data)
}

transform_data <- function(data) {
    data$Proportion <- data$Leave / data$NVotes
    data$Vote <- ifelse(data$Proportion > 0.5, "Leave", "Remain")
    data$RegionName <- as.character(data$RegionName)
    data[data$RegionName == "London", "RegionName"]                   <- "L"
    data[data$RegionName == "North West", "RegionName"]               <- "NW"
    data[data$RegionName == "North East", "RegionName"]               <- "NE"
    data[data$RegionName == "South West", "RegionName"]               <- "SW"
    data[data$RegionName == "South East", "RegionName"]               <- "SE"
    data[data$RegionName == "East Midlands", "RegionName"]            <- "EM"
    data[data$RegionName == "West Midlands", "RegionName"]            <- "WM"
    data[data$RegionName == "East of England", "RegionName"]          <- "EE"
    data[data$RegionName == "Yorkshire and The Humber", "RegionName"] <- "Y"
    return(data)
}

get_numerical_variable_names <- function(data) {
    names <- sapply(data, is.numeric)
    names <- remove_names_that_should_not_be_included(names)
    return(names)
}

remove_names_that_should_not_be_included <- function(names) {
    names[["ID"]]     <- FALSE
    names[["Leave"]]  <- FALSE
    names[["NVotes"]] <- FALSE
    return(names)
}

full_summary <- function(data, save_to = "") {
    summary <- summary(data)
    if (not_empty(save_to)) sink(save_to)
    print(summary)
    if (not_empty(save_to)) sink()
    return(summary)
}

numerical_summary <- function(data, numerical_variables, save_to = "") {
    summary <- do.call(
        cbind,
        lapply(data[complete.cases(data), numerical_variables], summary)
    )
    print(summary)
    if (not_empty(save_to)) write.csv(summary, save_to)
    return(summary)
}

plot_percentage <- function(data, variable, save_to = "") {
    if (not_empty(save_to)) png(save_to)
    barplot(
        prop.table(table(data[, variable])),
        ylab = "Frequency",
        col = "white"
    )
    if (not_empty(save_to)) dev.off()
}

matrix_scatterplots <- function(data, numerical_variables, save_to = "") {
    if (not_empty(save_to)) png(save_to, 2000, 2000)
    pairs(as.matrix(data[, numerical_variables]))
    if (not_empty(save_to)) dev.off()
}

all_scatterplots <- function(data, numerical_variables, save_to = "") {
    create_graphs_iteratively(data, numerical_variables, plot_scatterplot, save_to)
}

create_graphs_iteratively <- function(data,
                                      numerical_variables,
                                      plot_function,
                                      save_to = "") {
    numerical_variables[["Proportion"]] <- FALSE
    variables <- names(numerical_variables[numerical_variables == TRUE])

    n_variables <- (length(variables) - 1)
    progress_bar <- progress_bar$new(
        format = "Progress [:bar] :percent ETA: :eta",
        total = n_variables
    )
    for (i in 1:n_variables) {
        progress_bar$tick()
        for (j in (i + 1):length(variables)) {
            image_name <- paste(
                save_to,
                variables[i], "_",
                variables[j], ".png",
                sep = ""
            )
            plot_function(
                data,
                var_x = variables[i],
                var_y = variables[j],
                save_to = image_name,
                regression = TRUE
            )
        }
    }
}

plot_scatterplot <- function(data,
                             var_x,
                             var_y,
                             var_color = "Proportion",
                             regression = FALSE,
                             save_to = "") {
    if (var_color != "") {
        plot <- ggplot(data, aes_string(x = var_x, y = var_y, color = var_color))
    } else {
        plot <- ggplot(data, aes_string(x = var_x, y = var_y))
    }
    # TODO: Fix this: discrete value provided to continuous scale
    # plot <- plot + scale_color_viridis()
    plot <- plot + geom_point()
    if (regression) {
        plot <- plot + stat_smooth(method = "lm", col = "grey", se = FALSE)
    }
    if (not_empty(save_to)) png(save_to)
    print(plot)
    if (not_empty(save_to)) dev.off()
}

correlations_plot <- function(data, numerical_variables, save_to = "") {
    if (not_empty(save_to)) png(save_to, 1280, 1280)
    corrplot(cor(data[, numerical_variables]), tl.col = "black", tl.cex = 0.6)
    if (not_empty(save_to)) dev.off()
}

principal_components <- function(data, numerical_variables, save_to = "") {
    numerical_variables[["Proportion"]] <- FALSE

    pca <- prcomp(data[, numerical_variables], center = TRUE, scale. = TRUE)
    biplot <- ggbiplot(pca, groups = data$Vote)
    biplot <- biplot + scale_color_discrete(name = "")
    biplot <- biplot + theme(legend.position = "top",
                             legend.direction = "horizontal")

    if (not_empty(save_to)) sink(extend(save_to, "_results.txt"))
    print(pca)
    if (not_empty(save_to)) sink()

    if (not_empty(save_to)) sink(extend(save_to, "_summary.txt"))
    print(summary(pca))
    if (not_empty(save_to)) sink()

    if (not_empty(save_to)) png(extend(save_to, ".png"), 500, 500)
    plot(pca, type = "l", main = "Principal Components' Variances" )
    if (not_empty(save_to)) dev.off()

    if (not_empty(save_to)) png(extend(save_to, "_biplot.png"), 800, 800)
    print(biplot)
    if (not_empty(save_to)) dev.off()

    return(pca)
}

adjust_data <- function(data) {
    data <- add_relevant_variables(data)
    data <- remove_non_relevant_variables(data)
    return(data)
}

add_relevant_variables <- function(data) {
    data <- add_relevant_age_variables(data)
    data <- add_relevant_race_variables(data)
    data <- add_relevant_education_variables(data)
    return(data)
}

add_relevant_age_variables <- function(data) {
    data$Age_18to44 <- (
        data$Age_18to19 +
        data$Age_20to24 +
        data$Age_25to29 +
        data$Age_30to44
    )
    data$Age_45plus <- (
        data$Age_45to59 +
        data$Age_60to64 +
        data$Age_65to74 +
        data$Age_75to84 +
        data$Age_85to89 +
        data$Age_90plus
    )
    return(data)
}

add_relevant_race_variables <- function(data) {
    data$NonWhite <- (
        data$Black +
        data$Asian +
        data$Indian +
        data$Pakistani
    )
    return(data)
}

add_relevant_education_variables <- function(data) {
    data$HighEducationLevel <- data$L4Quals_plus
    data$LowEducationLevel  <- data$NoQuals
    return(data)
}

remove_non_relevant_variables <- function(data) {
    column_names <- colnames(data)
    names <- !logical(length(column_names))
    names <- setNames(names, column_names)
    names <- remove_unnecessary_age_variables(names)
    names <- remove_unnecessary_race_variables(names)
    names <- remove_unnecessary_education_variables(names)
    data  <- data[, names]
    return(data)
}

remove_unnecessary_age_variables <- function(names) {
    column_names <- names(names)
    age_variables <- sapply(column_names, contains_age_string)
    names[age_variables] <- FALSE
    names[["Age_18to44"]] <- TRUE
    names[["Age_45plus"]] <- TRUE
    return(names)
}

contains_age_string <- function(name) {
    return(grepl("Age", name))
}

remove_unnecessary_race_variables <- function(names) {
    names[["Black"]]     <- FALSE
    names[["Asian"]]     <- FALSE
    names[["Indian"]]    <- FALSE
    names[["Pakistani"]] <- FALSE
    return(names)
}

remove_unnecessary_education_variables <- function(names) {
    names[["NoQuals"]]      <- FALSE
    names[["L4Quals_plus"]] <- FALSE
    return(names)
}
