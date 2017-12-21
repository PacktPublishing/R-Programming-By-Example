#
# Chapter 04 - Simulating Sales Data And Working With Databases: Setup Reviews
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

# ---- setup-reviews

data <- read.csv("./data/original_reviews.csv")

data$STARS   <- data$Score
data$SUMMARY <- data$Summary
data$MESSAGE <- data$Text

columns_to_drop <- c(
    "Id",
    "Text",
    "Time",
    "Score",
    "UserId",
    "Summary",
    "ProductId",
    "ProfileName",
    "HelpfulnessNumerator",
    "HelpfulnessDenominator"
)

data <- data[, !(names(data) %in% columns_to_drop)]

saveRDS(data, "./data/reviews.rds")
