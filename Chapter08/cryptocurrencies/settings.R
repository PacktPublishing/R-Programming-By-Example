
# ---- settings-all

SETTINGS <- list(
    "debug" = TRUE,
    "storage" = list(
        "read" = list(
            "name" = "CSVFiles",
            "environment" = "production"
        ),
        "write" = list(
            list(
                "name" = "CSVFiles",
                "environment" = "production"
            )
        ),
        "table_names" = list(
            "production" = list(
                "assets" = "production_assets",
                "markets" = "production_markets",
                "users" = "production_users",
                "wallets" = "production_wallets"
            ),
            "development" = list(
                "assets" = "development_assets",
                "markets" = "development_markets",
                "users" = "development_users",
                "wallets" = "development_wallets"
            )
        )
    ),
    "batch_data_collection" = list(
        "assets" = list(
            "minutes" = 60
        ),
        "markets" = list(
            "minutes" = 60
        )
    )
)
