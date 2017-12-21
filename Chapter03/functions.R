#
# Chapter 03 - Predicting Votes With Linear Models: Functions
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

library(progress)

source("../chapter-01/functions.R")
source("../chapter-02/functions.R")

variable_histogram <- function(data, variable, save_to = "") {
    save_png(data, variable, save_to, histogram)
}

variable_qqplot <- function(data, variable, save_to = "") {
    save_png(data, variable, save_to, quantile_quantile)
}

save_png <- function(data, variable, save_to, function_to_create_image) {
    if (not_empty(save_to)) png(save_to)
    function_to_create_image(data, variable)
    if (not_empty(save_to)) dev.off()
}

histogram <- function(data, variable) {
    hist(data[, variable], main = "Histogram", xlab = "Proportion")
}

quantile_quantile <- function(data, variable) {
    qqnorm(data[, variable], main = "Normal QQ-Plot for Proportion")
    qqline(data[, variable])
}

generate_combinations_unvectorized <- function(variables,
                                               min_percentage,
                                               max_percentage) {
    variables[["Proportion"]] <- FALSE
    variables                 <- names(variables[variables == TRUE])
    n                         <- length(variables)
    n_min                     <- floor(n * min_percentage)
    n_max                     <- ceiling(n * max_percentage)
    all_combinations          <- NULL

    progress_bar <- progress_bar$new(
        format = "Progress [:bar] :percent ETA: :eta",
        total = length(n_min:n_max)
    )

    for (k in n_min:n_max) {
        progress_bar$tick()
        combinations <- combn(variables, k)
        for (column in 1:ncol(combinations)) {
            new_list <- list(combinations[, column])
            all_combinations <- c(all_combinations, list(new_list))
        }
    }
    return(unlist(all_combinations, recursive = FALSE))
}

generate_combinations_vectorized <- function(variables,
                                             min_percentage,
                                             max_percentage) {
    variables[["Proportion"]] <- FALSE
    variables <- names(variables[variables == TRUE])

    n       <- length(variables)
    n_min   <- floor(n * min_percentage)
    n_max   <- ceiling(n * max_percentage)

    progress_bar <- progress_bar$new(
        format = "Progress [:bar] :percent ETA: :eta",
        total = length(n_min:n_max)
    )

    all_combinations <- unlist(
        lapply(lapply(n_min:n_max, function(k) {
            progress_bar$tick()
            return(combn(variables, k))
        }),
        function(y) unlist(apply(y, 2, list), recursive = FALSE)),
        recursive = FALSE
    )
    return(all_combinations)
}

are_all_combinations_the_same_unvectorized <- function(a, b) {
    for (i in 1:length(a)) {
        if (any(a[[i]] != b[[i]])) {
            return(FALSE)
        }
    }
    return(TRUE)
}

are_all_combinations_the_same_vectorized <- function(a, b) {
    return(all(unlist(lapply(
        1:length(a),
        function(i) all(a[[i]] == b[[i]])
    ))))
}

find_best_fit <- function(type, measure, data_train, data_test, combinations) {
    n_cases <- length(combinations)
    progress_bar <- progress_bar$new(
        format = "Progress [:bar] :percent ETA: :eta",
        total = n_cases
    )
    scores <- lapply(1:n_cases, function(i) {
        progress_bar$tick()
        results <- compute_model_and_fit(type, combinations[[i]], data_train)
        score <- compute_score(measure, results[["fit"]], data_test)
        return(score)
    })
    i <- ifelse(measure == "Proportion", which.min(scores), which.max(scores))
    best_results <- compute_model_and_fit(type, combinations[[i]], data_train)
    best_score <- compute_score(measure, best_results[["fit"]], data_test)
    print_best_model_info(i, best_results[["model"]], best_score, measure)
    return(best_results[["fit"]])
}

compute_model_and_fit <- function(type, combination, data_train) {
    model <- generate_model(combination)
    if (type == "lm") {
        fit <- lm(model, data_train)
    } else {
        fit <- glm(model, quasibinomial, data_train)
    }
    return(list(model = model, fit = fit))
}

generate_model <- function(combination) {
    sum <- paste(combination, collapse = " + ")
    return(formula(paste("Proportion", "~", sum)))
}

compute_score <- function(measure, fit, data_test) {
    if (measure == "Proportion") {
        score <- score_proportions
    } else {
        score <- score_votes
    }
    predictions <- predict(fit, data_test, type = "response", se.fit = TRUE)
    return(score(data_test, predictions))
}

score_proportions <- function(data_test, predictions) {
    # se := standard errors
    se <- predictions$se.fit
    real <- data_test$Proportion
    predicted <- predictions$fit
    return(sum((real - predicted)^2 / se^2) / nrow(data))
}

score_votes <- function(data_test, predictions) {
    real <- data_test$Vote
    predicted <- ifelse(predictions$fit > 0.5, "Leave", "Remain")
    return(sum(real == predicted))
}

fit_plot <- function(fit, save_to = "") {
    if (not_empty(save_to)) png(save_to)
    par(mfrow = c(2, 2))
    plot(fit)
    if (not_empty(save_to)) dev.off()
}

fit_summary <- function(fit, save_to = "") {
    if (not_empty(save_to)) sink(save_to)
    print(summary(fit))
    if (not_empty(save_to)) sink()
}

print_best_model_info <- function(i, model, best_score, measure){
    print("*************************************")
    print(paste("Best model number:", i))
    print(paste("Best score:       ", best_score))
    print(paste("Score measure:    ", measure))
    print("Best model:")
    print(strsplit(toString(model), "\\+"))
    print("*************************************")
}
