#
# Chapter 10 - Adding Interactivity With Dashboards: Functions
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

# ---- functions-full

library(ggthemr)

ggthemr('flat dark', type = 'outer')

sma_graph <- function(data, sma) {
    g <- ggplot(data, aes(time, price_usd))
    g <- g + geom_point()
    g <- g + geom_line(group = 1)
    g <- g + geom_line(aes_string(y = sma),
                       group = 1, color = "white", size = 1)
    return(g)
}
