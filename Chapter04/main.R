#
# Chapter 04 - Simulating Sales Data And Working With Databases: Main
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

source("./functions.R")

#
# SALES
#

# ---- main-sales-table

status_levels         <- c("PENDING", "DELIVERED", "RETURNED", "CANCELLED")
protein_source_levels <- c("BEEF", "FISH", "CHICKEN", "VEGETARIAN")
continent_levels      <- c("AMERICA", "EUROPE", "ASIA")
delivery_levels       <- c("IN STORE", "TO LOCATION")
paid_levels           <- c("YES", "NO")

sales <- data.frame(
    SALE_ID        = character(),
    CLIENT_ID      = character(),
    DATE           = as.Date(character()),
    QUANTITY       = integer(),
    COST           = numeric(),
    PRICE          = numeric(),
    DISCOUNT       = numeric(),
    PROTEIN        = numeric(),
    CARBS          = numeric(),
    FAT            = numeric(),
    PROTEIN_SOURCE = factor(levels = protein_source_levels),
    CONTINENT      = factor(levels = continent_levels),
    DELIVERY       = factor(levels = delivery_levels),
    STATUS         = factor(levels = status_levels),
    PAID           = factor(levels = paid_levels)
)

# ---- main-sales-usage

parameters <- list(
    n                            = 10000,
    n_letters                    = 5,
    n_digits                     = 5,
    reduction                    = 0.25,
    date_start                   = as.Date("2015-01-01"),
    date_end                     = Sys.Date(),
    quantity_lambda              = 2,
    cost_mean                    = 12,
    cost_variance                = 1,
    price_mean                   = 15,
    price_variance               = 2,
    discount_lambda              = 100,
    protein_source_probabilities = c(0.6, 0.2, 0.1, 0.1),
    continent_probabilities      = c(0.5, 0.3, 0.2),
    delivery_probabilities       = c(0.7, 0.3),
    status_probabilities         = c(0.2, 0.6, 0.1, 0.1),
    paid_probabilities           = c(0.9, 0.1)
)

sales <- random_sales_data(sales, parameters)

# ---- section-separator

#
# CLIENTS
#

# ---- main-clients-table

gender_levels <- c("FEMALE", "MALE")
star_levels   <- c("1", "2", "3", "4", "5")

clients <- data.frame(
    CLIENT_ID    = character(),
    BIRTH_DATE   = as.Date(character()),
    CLIENT_SINCE = as.Date(character()),
    GENDER       = factor(levels = gender_levels),
    STARS        = factor(levels = star_levels)
)

# ---- main-clients-usage

parameters <- list(
    birth_date_start    = as.Date("1950-01-01"),
    birth_date_end      = as.Date("1997-01-01"),
    client_since_start  = as.Date("2015-01-01"),
    client_since_end    = Sys.Date(),
    stars_probabilities = c(0.05, 0.1, 0.15, 0.2, 0.5)
)

clients <- random_clients_data(clients, unique(sales$CLIENT_ID), parameters)

# ---- section-separator

#
# CLIENT MESSAGES
#

# ---- main-client-messages-table

client_messages <- data.frame(
    SALE_ID = character(),
    DATE    = as.Date(character()),
    STARS   = factor(levels = star_levels),
    SUMMARY = character(),
    MESSAGE = character(),
    LAT     = numeric(),
    LNG     = numeric()
)

# ---- main-client-messages-usage

parameters <- list(
    n            = 1000,
    date_start   = as.Date("2015-01-01"),
    date_end     = Sys.Date(),
    reviews_file = "./reviews/data/reviews.rds",
    locations    = list(
        "AMERICA" = list(
            list(LAT = 35.982915, LNG = -119.028006),
            list(LAT = 29.023053, LNG = -81.762383),
            list(LAT = 41.726658, LNG = -74.731133),
            list(LAT = 19.256493, LNG = -99.292577),
            list(LAT = -22.472499, LNG = -43.348329)
        ),
        "EUROPE" = list(
            list(LAT = 40.436888, LNG = -3.863850),
            list(LAT = 48.716026, LNG = 2.350955),
            list(LAT = 52.348010, LNG = 13.351161),
            list(LAT = 42.025875, LNG = 12.418940),
            list(LAT = 51.504122, LNG = -0.364277)
        ),
        "ASIA" = list(
            list(LAT = 31.074426, LNG = 121.125328),
            list(LAT = 22.535733, LNG = 113.830406),
            list(LAT = 37.618251, LNG = 127.135865),
            list(LAT = 35.713791, LNG = 139.489820),
            list(LAT = 19.134907, LNG = 73.000993)
        )
    )
)

client_messages <- random_client_messages_data(client_messages, sales, parameters)

# ---- main-save-simulated-data

saveRDS(sales, "./results/sales.rds")
saveRDS(clients, "./results/clients.rds")
saveRDS(client_messages, "./results/client_messages.rds")

# ---- section-separator

# #
# # RELATIONAL DATABASES
# #

# # ---- main-database-setup

# # TODO: Remove user:

# parameters <- list(
#     user     = "otrenav",
#     host     = "localhost",
#     database = "sales"
# )
# db <- database_setup(parameters)

# # ---- main-database-write

# dbWriteTable(db, "sales", sales, overwrite = TRUE)
# dbWriteTable(db, "clients", clients, overwrite = TRUE)
# dbWriteTable(db, "client_messages", client_messages, overwrite = TRUE)

# # ---- main-database-read-without-metadata

# sales_from_db <- dbReadTable(db, "sales")
# str(sales_from_db)
# str(sales)

# # ---- main-database-read-with-metadata

# sales_from_db <- read_table(db, "sales")
# str(sales_from_db)
# str(sales)

# # ---- main-database-read-all

# sales           <- read_table(db, "sales")
# clients         <- read_table(db, "clients")
# client_messages <- read_table(db, "client_messages")

# # ---- main-database-explore-database

# dbListTables(db)
# dbListFields(db, "sales")
# dbListFields(db, "clients")
# dbListFields(db, "client_messages")

# # ---- main-database-disconnect

# dbDisconnect(db)
