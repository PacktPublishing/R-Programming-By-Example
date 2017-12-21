#
# Chapter 08 - An Object-Oriented System To Track Cryptocurrency Markets:
#              S4 Square And Rectangles Example
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

# ---- s4-constructors

library(methods)

setClass(
    Class = "S4Color",
    representation = representation(
        color = "character"
    )
)
setClass(
    Class = "S4Rectangle",
    representation = representation(
        a = "numeric",
        b = "numeric",
        color = "S4Color"
    )
)

# ---- s4-constructor-demo

S4_rectangle <- new(
    "S4Rectangle",
    a = 2,
    b = 3,
    color = new("S4Color", color = "blue")
)
class(S4_rectangle)
str(S4_rectangle)

# ---- s4-methods-and-polymorphism

setGeneric("S4area", function(self) {
    standardGeneric("S4area")
})
setMethod("S4area", "S4Rectangle", function(self) {
    return(self@a * self@b)
})

S4area(S4_rectangle)

setGeneric("S4color", function(self) {
    standardGeneric("S4color")
})
setMethod("S4color", "S4Rectangle", function(self) {
    return(self@color@color)
})

# ---- s4-print-demo-1

print(S4_rectangle)

# ---- s4-print-rectangle

setGeneric("S4print", function(self) {
    standardGeneric("S4print")
})
setMethod("S4print", "S4Rectangle", function(self) {
    print(paste(
        S4color(self), "rectangle:",
        self@a, "x", self@b, "==", S4area(self)
    ))
})

# ---- s4-print-demo-2

S4print(S4_rectangle)

# ---- s4-encapsulation-demo-1

print(S4_rectangle@a)
S4_rectangle@a <- 1
print(S4_rectangle@a)

print(S4color(S4_rectangle))
S4color(S4_rectangle) <- "black"
print(S4color(S4_rectangle))

# ---- s4-set-color-rectangle

setGeneric("S4color<-", function(self, value) {
    standardGeneric("S4color<-")
})
setReplaceMethod("S4color", "S4Rectangle", function(self, value) {
    self@color <- new("S4Color", color = value)
    return(self)
})

# ---- s4-encapsulation-demo-2

print(S4color(S4_rectangle))
S4color(S4_rectangle) <- "black"
print(S4color(S4_rectangle))

# ---- s4-square-constructor

setClass("S4Square", contains = "S4Rectangle")

# ---- s4-square-constructor-demo

S4_square <- new("S4Square", a = 4, b = 4, color = new("S4Color", color = "red"))
class(S4_square)
S4print(S4_square)

# ---- s4-print-square

setMethod("S4print", "S4Square", function(self) {
    print(paste(
        S4color(self), "square:",
        self@a, "x", self@b, "==", S4area(self)
    ))
})

S4print(S4_square)
