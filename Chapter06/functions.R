#
# Chapter 06 - Understanding Product Reviews With Text Analysis: Functions
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

library(lsa)
library(irlba)
library(caret)
library(ggplot2)
library(twitteR)
library(quanteda)
library(sentimentr)
# library(doParalell)
library(paralell)
library(randomForest)

clean_client_messages <- function(client_messages) {
    client_messages$MESSAGE <- as.character(client_messages$MESSAGE)
    client_messages$SUMMARY <- as.character(client_messages$SUMMARY)
    client_messages$STARS <- as.numeric(client_messages$STARS)
    client_messages$MULT_PURCHASES <- as.factor(client_messages$STARS >= 5)
    indexes <- createDataPartition(
        client_messages$MULT_PURCHASES,
        list = FALSE,
        times = 1,
        p = 0.3
    )
    return(client_messages[indexes, ])
}

# ---- section-separator

#
# Document Frequency Matrix
#

# ---- functions-build-tokens

build_tokens <- function(data, n_grams = 1) {
    tokens <- tokens(
        data,
        remove_punct = TRUE,
        remove_numbers = TRUE,
        remove_symbols = TRUE,
        remove_hyphens = TRUE
    )
    tokens <- tokens_tolower(tokens)
    tokens <- tokens_select(tokens, stopwords(), selection = "remove")
    tokens <- tokens_wordstem(tokens, language = "english")
    tokens <- tokens_ngrams(tokens, n = 1:n_grams)
    return(tokens)
}

# ---- functions-build-dfm

build_dfm <- function(data, n_grams = 1) {
    tokens <- build_tokens(data, n_grams)
    return(dfm(tokens))
}

# ---- functions-build-dfm-df

build_dfm_df <- function(data, dfm) {
    df <- cbind(MULT_PURCHASES = data$MULT_PURCHASES, data.frame(dfm))
    names(df) <- make.names(names(df))
    return(df)
}

# ---- section-separator

#
# Training
#

# ---- functions-train-model

train_model <- function(data, cv.control) {
    cluster <- makeCluster(detectCores())
    start.time <- Sys.time()
    model <- train(
        MULT_PURCHASES ~ .,
        data = data,
        method = "rf",
        trControl = cv.control,
        tuneLength = 5
    )
    print(Sys.time() - start.time)
    stopCluster(cluster)
    return(model)
}

# ---- section-separator

#
# TF-IDF
#

# ---- functions-term-frequency

term_frequency <- function(row) {
    return(row / sum(row))
}

inverse_document_frequency <- function(col) {
    corpus_size <- length(col)
    doc_count <- length(which(col > 0))
    return(log10(corpus_size / doc_count))
}

tf_idf <- function(tf, idf) {
    return(tf * idf)
}

# ---- functions-build-tf-idf

build_tf_idf <- function(dfm, idf = NULL) {
    tf <- apply(as.matrix(dfm), 1, term_frequency)
    if (is.null(idf)) {
        idf <- apply(as.matrix(dfm), 2, inverse_document_frequency)
    }
    tfidf <- t(apply(tf, 2, tf_idf, idf = idf))
    incomplete_cases <- which(!complete.cases(tfidf))
    tfidf[incomplete_cases, ] <- rep(0.0, ncol(tfidf))
    return(tfidf)
}

# ---- section-separator

#
# SVD
#

# ---- functions-build-svd

build_svd <- function(dfm) {
    dfm <- t(dfm)
    start.time <- Sys.time()
    svd <- irlba(dfm, nv = min(nrow(dfm), ncol(dfm)) / 4)
    print(Sys.time() - start.time)
    return(svd)
}

# ---- section-separator

#
# Cosine similarity
#

# ---- functions-cosine-similarities

cosine_similarities <- function(df) {
    return(cosine(t(as.matrix(df[, -c(1)]))))
}

# ---- functions-mean-cosine-similarities

mean_cosine_similarities <- function(df) {
    similarities <- cosine_similarities(df)
    indexes <- which(df$MULT_PURCHASES == TRUE)
    df$MULT_PURCHASES_SIMILARITY <- rep(0.0, nrow(df))
    for (i in 1:nrow(df)) {
        df$MULT_PURCHASES_SIMILARITY[i] <- mean(similarities[i, indexes])
    }
    return(df)
}

# ---- section-separator

#
# TWITTER
#

# ---- functions-get-twitter-data

get_twitter_data <- function(keyword, n) {
    return(twListToDF(searchTwitter(keyword, n, lang = "en")))
}
