
library(knitr)

set.seed(12345)
options(digits = 4)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  out.width = "100%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,  # 1 / Phi
  fig.show = "hold"
)

options(dplyr.print_min = 6, dplyr.print_max = 6)

hook_output <- knit_hooks$get("output")
knit_hooks$set(output = function(x, options) {
    lines <- options$output.lines
    if (is.null(lines)) {
        return(hook_output(x, options))
    }
    x <- unlist(strsplit(x, "\n"))
    more <- "(Truncated output)"
    if (length(lines) == 1) {
        if (length(x) > lines) {
            x <- c(head(x, lines), more)
        }
    } else {
        x <- c(more, x[lines], more)
    }
    x <- paste(c(x, ""), collapse = "\n")
    hook_output(x, options)
})
