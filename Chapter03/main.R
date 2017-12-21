#
# Chapter 03 - Predicting Votes With Linear Models: Main
#
# This chapter uses the data from *Chapter 02*.
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

source("./functions.R")

empty_directories("./results/")

print_section("Loading data...")

data <- adjust_data(prepare_data("../chapter-02/data_brexit_referendum.csv"))

data_incomplete     <- data[!complete.cases(data), ]
data                <- data[ complete.cases(data), ]
numerical_variables <- get_numerical_variable_names(data)

print_section("Creating sample...")

set.seed(12345)

n          <- nrow(data)
sample     <- sample(1:n, size = round(0.7 * n), replace = FALSE)
data_train <- data[ sample, ]
data_test  <- data[-sample, ]

print_section("Creating plots...")

variable_qqplot(
    data = data,
    variable = "Proportion",
    save_to = "./results/proportion_qqplot.png"
)
variable_histogram(
    data = data,
    variable = "Proportion",
    save_to = "./results/proportion_histogram.png"
)
plot_scatterplot(
    data = data,
    var_x = "Students",
    var_y = "Proportion",
    var_color = FALSE,
    regression = TRUE,
    save_to = "./results/scatterplot_students.png"
)
plot_scatterplot(
    data = data,
    var_x = "Age_18to44",
    var_y = "Proportion",
    var_color = FALSE,
    regression = TRUE,
    save_to = "./results/scatterplot_age18to44.png"
)

print_section("Generating combinations...")

combinations <- generate_combinations_vectorized(numerical_variables, 0.9, 1.0)

print_section("Finding best fits for `Proportion` measure...")

best_lm_fit_by_proportions <- find_best_fit(
    type = "lm",
    measure = "Proportion",
    data_train = data_train,
    data_test = data_test,
    combinations = combinations
)
best_glm_fit_by_proportions <- find_best_fit(
    type = "glm",
    measure = "Proportion",
    data_train = data_train,
    data_test = data_test,
    combinations = combinations
)

print_section("Finding best fits for `Votes` measure...")

best_lm_fit_by_votes <- find_best_fit(
    type = "lm",
    measure = "Vote",
    data_train = data_train,
    data_test = data_test,
    combinations = combinations
)
best_glm_fit_by_votes <- find_best_fit(
    type = "glm",
    measure = "Vote",
    data_train = data_train,
    data_test = data_test,
    combinations = combinations
)

print_section("Saving results...")

saveRDS(
    object = best_lm_fit_by_votes,
    file = "./results/best_lm_fit_by_votes.rds"
)
saveRDS(
    object = best_glm_fit_by_votes,
    file = "./results/best_glm_fit_by_votes.rds"
)
saveRDS(
    object = best_lm_fit_by_proportions,
    file = "./results/best_lm_fit_by_proportions.rds"
)
saveRDS(
    object = best_glm_fit_by_proportions,
    file = "./results/best_glm_fit_by_proportions.rds"
)

print_section("Saving result plots for linear models...")

fit_plot(
    fit = best_lm_fit_by_votes,
    save_to = "./results/best_lm_fit_by_votes.png"
)
fit_plot(
    fit = best_lm_fit_by_proportions,
    save_to = "./results/best_lm_fit_by_proportions.png"
)

print_section("Saving summaries...")

fit_summary(
    fit = best_lm_fit_by_votes,
    save_to = "./results/best_lm_summary_by_votes.txt"
)
fit_summary(
    fit = best_glm_fit_by_votes,
    save_to = "./results/best_glm_summary_by_votes.txt"
)
fit_summary(
    fit = best_lm_fit_by_proportions,
    save_to = "./results/best_lm_summary_by_proportions.txt"
)
fit_summary(
    fit = best_glm_fit_by_proportions,
    save_to = "./results/best_glm_summary_by_proportions.txt"
)

print_section("Done.")
