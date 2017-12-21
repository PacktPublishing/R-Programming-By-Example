
library(devtools)
install_github("ifellows/wordcloud")
install_github("topepo/caret/pkg/caret")

install.packages(c(
    "lsa",
    "e1071",
    "irlba",
    "ggplot2",
    "twitteR",
    "quanteda",
    "sentimentr",
    "doParallel",
    "randomForest"
), dependencies = TRUE)
