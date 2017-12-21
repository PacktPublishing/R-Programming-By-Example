#
# Chapter 01 - Introduction to R: Shared Functions
#
# Omar Trejo Navarro
# Mexico City
# May, 2017
#

empty_directories <- function(directories) {
    for (directory in directories) {
        unlink(directory, recursive = TRUE)
        dir.create(directory, showWarnings = FALSE)
    }
}

not_empty <- function(file) {
    return(file != "")
}

print_section <- function(string) {
    separator <- paste(rep("*", nchar(string)), collapse = "")
    print(separator)
    print(string)
    print(separator)
}
