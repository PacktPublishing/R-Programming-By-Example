#
# Chapter 08 - An Object-Oriented System To Track Cryptocurrency Markets:
#              S3 Square And Rectangles Example
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

# ---- s3-constructors

color_constructor <- function(color) {
    class(color) <- "S3Color"
    return(color)
}

rectangle_constructor <- function(a, b, color) {
    rectangle <- list(a = a, b = b, color = color_constructor(color))
    class(rectangle) <- "S3Rectangle"
    return(rectangle)
}

# ---- s3-constructor-demo

S3_rectangle <- rectangle_constructor(2, 3, "blue")
class(S3_rectangle)
str(S3_rectangle)

# ---- s3-area-rectangle

S3area.S3Rectangle <- function(rectangle) {
    return(rectangle$a * rectangle$b)
}

# ---- s3-area-rectangle-demo-1

S3area(S3_rectangle)

# ---- s3-area-rectangle-area

S3area <- function(object) {
    UseMethod("S3area")
}

# ---- s3-area-rectangle-demo-2

S3area(S3_rectangle)

# ---- s3-color-rectangle

S3color.S3Rectangle <- function(rectangle) {
    return(rectangle$color)
}

S3color <- function(object) {
    UseMethod("S3color")
}

# ---- s3-print-demo-1

print(S3_rectangle)

# ---- s3-print-rectangle

print.S3Rectangle <- function(rectangle) {
    print(paste(
        S3color(rectangle), "rectangle:",
        rectangle$a, "x", rectangle$b, "==", S3area(rectangle)
    ))
}

# ---- s3-print-demo-2

print(S3_rectangle)

# ---- s3-encapsulation-demo-1

print(S3_rectangle$a)
S3_rectangle$a <- 1
print(S3_rectangle$a)

# ---- s3-set-color-rectangle

set_color.S3Rectangle <- function(rectangle, new_color) {
    rectangle$color <- new_color
    return(rectangle)
}

set_color <- function(object, new_color) {
    UseMethod("set_color")
}

# ---- s3-encapsulation-demo-2

print(S3color(S3_rectangle))
set_color(S3_rectangle, "black")
print(S3color(S3_rectangle))

# ---- s3-encapsulatino-demo-3

print(S3color(S3_rectangle))
S3_rectangle <- set_color(S3_rectangle, "black")
print(S3color(S3_rectangle))

# ---- s3-square-constructor

square_constructor <- function(a, color) {
    square <- rectangle_constructor(a, a, color)
    class(square) <- c("S3Square", class(square))
    return(square)
}

# ---- s3-square-constructor-demo

S3_square <- square_constructor(4, "red")
class(S3_square)
print(S3_square)

# ---- s3-print-square

print.S3Square <- function(square) {
    print(paste(
        S3color(square), "square:",
        square$a, "x", square$b, "==", S3area(square)
    ))
}

# ---- s3-print-square-demo

print(S3_square)
