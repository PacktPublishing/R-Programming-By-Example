#
# Chapter 08 - An Object-Oriented System To Track Cryptocurrency Markets:
#              R6 Square And Rectangles Example
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

# ---- r6-constructors

library(R6)

R6Rectangle <- R6Class(
    "R6Rectangle",
    public = list(
        initialize = function(a, b, color) {
            private$a <- a
            private$b <- b
            private$own_color <- color
        },
        area = function() {
            private$a * private$b
        },
        color = function() {
            private$own_color
        },
        set_color = function(new_color) {
            private$own_color <- new_color
        },
        own_print = function() {
            print(paste(
                self$color(), "rectangle:",
                private$a, "x", private$b, " == ", self$area()
            ))
        }
    ),
    private = list(
        a = NULL,
        b = NULL,
        own_color = NULL
    )
)

# ---- r6-constructor-demo

R6_rectangle <- R6Rectangle$new(2, 3, "blue")
class(R6_rectangle)
print(R6_rectangle)

# ---- r6-area-rectangle-demo-1

R6_rectangle$own_print()
R6_rectangle$area()
R6_rectangle$color()

# ---- r6-encapsulation-demo

R6_rectangle$a
R6_rectangle$own_print()
R6_rectangle$a <- 1
R6_rectangle$own_print()
R6_rectangle$set_color("black")
R6_rectangle$own_print()

# ---- r6-square-constructor

R6Square <- R6Class(
    "R6Square",
    inherit = R6Rectangle,
    public = list(
        initialize = function(a, color) {
            super$initialize(a, a, color)
        },
        print = function() {
            print(paste(
                self$color(), "square:",
                private$a, "x", private$b, " == ", self$area()
            ))
        }
    )
)

# ---- r6-square-constructor-demo

R6_square <- R6Square$new(4, "red")
class(R6_square)
print(R6_square)
