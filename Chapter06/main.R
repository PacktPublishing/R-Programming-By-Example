#
# Chapter 06 - Understanding Product Reviews With Text Analysis: Main
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

source("./functions.R")

# ---- main-set-seed

set.seed(12345)

# ---- main-load-data

client_messages <- readRDS("../chapter-04/results/client_messages.rds")

# ---- remove-this

# TODO: Remove this into simulation in chapter 4
client_messages <- clean_client_messages(client_messages)
str(client_messages)
prop.table(table(client_messages$MULT_PURCHASES))

# ---- main-create-training-and-testing-sets

indexes <- createDataPartition(
    client_messages$MULT_PURCHASES,
    list = FALSE,
    times = 1,
    p = 0.7
)
train <- client_messages[ indexes, ]
test  <- client_messages[-indexes, ]

# ---- section-separator

#
# Model 1
#

# ---- main-build-dfm

train.dfm <- build_dfm(train$MESSAGE)

# ---- main-build-dfm-df

train.dfm.df <- build_dfm_df(train, train.dfm)

# ---- main-cross-validation

cv.control <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 2
)
# ---- main-cross-validation-note

# To use parallelization with `parallel` package, use this:

cv.control <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 2,
    allowParallel = TRUE
)

# ---- main-model-1

model.1 <- train_model(train.dfm.df, cv.control)

# ---- main-model-1-confusion-matrix

confusionMatrix(model.1$finalModel$predicted, train$MULT_PURCHASES)

# ---- section-separator

#
# Model 2
#

# ---- main-tf-idf

train.tfidf <- build_tf_idf(train.dfm)
train.tfidf.df <- build_dfm_df(train, train.tfidf)

# ---- main-model-2

model.2 <- train_model(train.tfidf.df, cv.control)

# ---- section-separator

#
# Model 3
#

# ---- main-add-bigrams

train.bigrams.dfm <- build_dfm(train$MESSAGE, n_grams = 2)
train.bigrams.tfidf <- build_tf_idf(train.bigrams.dfm)
train.bigrams.tfidf.df <- build_dfm_df(train, train.bigrams.tfidf)

# ---- main-model-3

model.3 <- train_model(train.bigrams.tfidf.df, cv.control)

# ---- section-separator

#
# Model 4
#

# ---- main-model-4

train.bigrams.svd <- build_svd(train.bigrams.tfidf)
train.bigrams.svd.df <- build_dfm_df(train, train.bigrams.svd$v)
model.4 <- train_model(train.bigrams.svd.df, cv.control)

# ---- section-separator

#
# Model 5
#

# ---- main-model-5

train.bigrams.svd.sim.df <- mean_cosine_similarities(train.bigrams.svd.df)
model.5 <- train_model(train.bigrams.svd.sim.df, cv.control)

# ---- section-separator

#
# Model 6
#

# ---- main-model-6

train.sentiment <- sentiment_by(train$MESSAGE)
train.sentiments.df <- cbind(
    train.tfidf.df,
    WORD_COUNT = train.sentiment$word_count,
    SENTIMENT = train.sentiment$ave_sentiment
)
model.6 <- train_model(train.sentiments.df, cv.control)

# ---- section-separator

#
# Test
#

# ---- main-test-build-dfm

test.dfm <- build_dfm(test)
test.dfm <- dfm_select(test.dfm, pattern = train.dfm, selection = "keep")

# ---- main-test-build-tf-idf

train.idf <- apply(as.matrix(train.dfm), 2, inverse_document_frequency)
test.tfidf <- build_tf_idf(test.dfm, idf = train.idf)
test.tfidf.df <- build_dfm_df(test, test.tfidf)

# ---- main-test-sentiment

test.sentiment <- sentiment_by(test$MESSAGE)
test.sentiments.df <- cbind(
    test.tfidf.df,
    WORD_COUNT = test.sentiment$word_count,
    SENTIMENT = test.sentiment$ave_sentiment
)

# ---- main-test-sentiment-predictions

predictions <- predict(model.6, test.sentiments.df)

# ---- main-test-bigrams

test.bigrams.dfm <- build_dfm(test, n_grams = 2)
test.bigrams.dfm <- dfm_select(
    test.bigrams.dfm,
    pattern = train.bigrams.dfm,
    selection = "keep"
)

# ---- main-test-bigrams-tf-idf

train.bigrams.idf <- apply(as.matrix(train.bigrams.dfm), 2, inverse_document_frequency)
test.bigrams.tfidf <- build_tf_idf(test.bigrams.dfm, idf = train.bigrams.idf)

# ---- main-test-bigrams-tf-idf-svd

sigma.inverse       <- 1 / train.bigrams.svd$d
u.transpose         <- t(train.bigrams.svd$u)
test.bigrams.svd    <- t(sigma.inverse * u.transpose %*% t(test.bigrams.tfidf))
test.bigrams.svd.df <- build_dfm_df(test, test.bigrams.svd)

# ---- section-separator

#
# Twitter
#

# ---- main-twitter-login

consumer_key    <- "b9SGfRpz4b1rnHFtN2HtiQ9xl"
consumer_secret <- "YMifSUmCJ4dlgB8RVxKRNcTLQw7Y4IBwDwBRkdz2Va1vcQjOP0"
access_token    <- "171370802-RTl4RBpMDaSFdVf5q9xrSWQKxtae4Wi3y76Ka4Lz"
access_secret   <- "dHfbMtmpeA2QdOH5cYPXO5b4hF8Nj6LjxELfOMSwHoUB8"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# ---- main-get-twitter-data

food_data <- get_twitter_data("The Food Factory", 250)
