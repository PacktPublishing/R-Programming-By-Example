#
# Chapter 02 - Understanding Votes With Descriptive Statistics: Main
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

start_time <- proc.time()

source("./functions.R")

empty_directories(c(
    "./results/original/",
    "./results/adjusted/",
    "./results/original/scatterplots/"
))

data <- prepare_data("./data_brexit_referendum.csv", complete_cases = TRUE)

data_adjusted           <- adjust_data(data)
numerical_variables     <- get_numerical_variable_names(data)
numerical_variables_adj <- get_numerical_variable_names(data_adjusted)

print_section("Working on summaries...")

full_summary(
    data,
    save_to = "./results/original/summary_text.txt"
)
numerical_summary(
    data,
    numerical_variables,
    save_to = "./results/original/summary_numerical.csv"
)

print_section("Working on histograms...")

plot_percentage(
    data,
    variable = "RegionName",
    save_to = "./results/original/vote_percentage_by_region.png"
)

print_section("Working on matrix scatterplots...")

matrix_scatterplots(
    data_adjusted,
    numerical_variables_adj,
    save_to = "./results/adjusted/matrix_scatterplots.png"
)

print_section("Working on scatterplots...")

plot_scatterplot(
    data,
    var_x = "RegionName",
    var_y = "Proportion",
    var_color = "White",
    regression = TRUE,
    save_to = "./results/original/regionname_vs_proportion_vs_white.png"
)
all_scatterplots(
    data,
    numerical_variables,
    save_to = "./results/original/scatterplots/"
)

print_section("Working on correlations...")

correlations_plot(
    data,
    numerical_variables,
    save_to = "./results/original/correlations.png"
)

print_section("Working on principal components...")

principal_components(
    data_adjusted,
    numerical_variables_adj,
    save_to = "./results/adjusted/principal_components"
)

end_time <- proc.time()
time_taken <- end_time - start_time
print(paste("Time taken:", time_taken[1]))

print_section("Done.")
