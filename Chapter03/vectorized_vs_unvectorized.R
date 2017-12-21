#
# Chapter 03 - Predicting Votes With Linear Models: Performance Test
#
# Objective: Test the speed of vectorized vs unvectorized functions.
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

source("./functions.R")

#
# Feel free to experiment with different parameters:
# (they should be between 0.0 and 1.0)
#
MIN_PERCENTAGE <- 0.6
MAX_PERCENTAGE <- 1.0

print_section("Loading data...")

data <- adjust_data(prepare_data(
    "../chapter-02/data_brexit_referendum.csv",
    complete_cases = TRUE
))
numerical_variables <- get_numerical_variable_names(data)

print_section("Generating combinations (unvectorized version)...")

start_time <- proc.time()

c_unvectorized <- generate_combinations_unvectorized(
    variables = numerical_variables,
    min_percentage = MIN_PERCENTAGE,
    max_percentage = MAX_PERCENTAGE
)

end_time <- proc.time()
time_taken <- end_time - start_time
print(paste("Time taken:", time_taken[1]))

print_section("Generating combinations (vectorized version)...")

start_time <- proc.time()

c_vectorized   <- generate_combinations_vectorized(
    variables = numerical_variables,
    min_percentage = MIN_PERCENTAGE,
    max_percentage = MAX_PERCENTAGE
)

end_time <- proc.time()
time_taken <- end_time - start_time
print(paste("Time taken:", time_taken[1]))

print_section("Checking combinations (unvectorized version)...")

start_time <- proc.time()

are_all_combinations_the_same_unvectorized(
    a = c_vectorized, b = c_unvectorized
)

end_time <- proc.time()
time_taken <- end_time - start_time
print(paste("Time taken:", time_taken[1]))

print_section("Checking combinations (vectorized version)...")

start_time <- proc.time()

are_all_combinations_the_same_vectorized(
    a = c_vectorized, b = c_unvectorized
)

end_time <- proc.time()
time_taken <- end_time - start_time
print(paste("Time taken:", time_taken[1]))
