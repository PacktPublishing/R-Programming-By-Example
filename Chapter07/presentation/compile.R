
library(rmarkdown)

types <- c(
    # "html_document",
    # "pdf_document",
    # "word_document",
    "ioslides_presentation"
    # "slidy_presentation",
    # "beamer_presentation"
)
render("presentation.Rmd", types)
