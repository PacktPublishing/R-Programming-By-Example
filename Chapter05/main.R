#
# Chapter 05 - Communicating Sales With Visualizations: Main
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

source("./functions.R")

sales           <- readRDS("../chapter-04/results/sales.rds")
clients         <- readRDS("../chapter-04/results/clients.rds")
client_messages <- readRDS("../chapter-04/results/client_messages.rds")

# ---- main-sales-add-profit

sales <- add_profits(sales)

# ---- section-separator

#
# Bar graphs
#

# ---- main-sales-frequency-by-quantity-without-function

graph <- ggplot(sales, aes(QUANTITY)) +
    geom_bar() +
    ggtitle("QUANTITY Frequency") +
    scale_x_continuous(breaks = seq(min(sales[, "QUANTITY"]),
                                    max(sales[, "QUANTITY"])))

# ---- main-sales-frequency-by-quantity-with-function

graph <- graph_bars(sales, "QUANTITY")
save_png(graph, "./results/sales_frequency_by_quantity.png", 300, 300)

# ---- main-sales-frequency-by-continent

graph <- graph_bars(sales, "CONTINENT")
save_png(graph, "./results/sales_frequency_by_continent.png", 300, 300)

# ---- main-sales-profit-by-continent

graph <- graph_bars(sales, "CONTINENT", "PROFIT")
save_png(graph, "./results/sales_profit_by_continent.png", 300, 300)

# ---- main-sales-profit-ratio-by-continent

graph <- graph_bars(sales, "CONTINENT", "PROFIT_RATIO")
save_png(graph, "./results/sales_profit_ratio_by_continent.png", 300, 300)

# ---- main-sales-frequency-by-protein-source

graph <- graph_bars(sales, "PROTEIN_SOURCE")
save_png(graph, "./results/sales_frequency_by_protein_source.png", 300, 300)

# ---- main-sales-profit-by-protein-source

graph <- graph_bars(sales, "PROTEIN_SOURCE", "PROFIT")
save_png(graph, "./results/sales_profit_by_protein_source.png", 300, 300)

# ---- main-sales-profit-ratio-by-protein-source

graph <- graph_bars(sales, "PROTEIN_SOURCE", "PROFIT_RATIO")
save_png(graph, "./results/sales_profit_ratio_by_protein_source.png", 300, 300)

# ---- main-sales-frequency-by-quantity-and-continent

graph <- graph_bars(sales, "QUANTITY", color = "PROTEIN_SOURCE")
save_png(graph, "./results/sales_frequency_by_quantity_and_continent.png", 900, 300)

# ---- main-sales-profit-by-continent-and-protein-source

graph <- graph_bars(sales, "CONTINENT", "PROFIT", "PROTEIN_SOURCE")
save_png(graph, "./results/sales_profit_by_continent_and_protein_source.png", 450, 300)

# ---- main-sales-profit-ratio-by-continent-and-protein-source

graph <- graph_bars(sales, "CONTINENT", "PROFIT_RATIO", "PROTEIN_SOURCE")
save_png(graph, "./results/sales_profit_ratio_by_continent_and_protein_source.png", 450, 300)

# ---- section-separator

#
# Top n bar graphs
#

# ---- main-sales-frequency-by-top-clients-bars

graph <- graph_top_n_bars(sales, "CLIENT_ID", 10)
save_png(graph, "./results/sales_frequency_by_top_clients.png", 900, 300)

# ---- main-sales-profit-by-top-clients-bars

graph <- graph_top_n_bars(sales, "CLIENT_ID", 10, TRUE)
save_png(graph, "./results/sales_profit_by_top_clients.png", 900, 300)

#
# Top n boxplots
#

# ---- main-sales-profit-by-top-clients-boxplot

graph <- graph_top_n_boxplots(sales, "CLIENT_ID", "PROFIT", 10)
save_png(graph, "./results/sales_profit_by_top_clients_boxplot.png", 900, 300)

# ---- section-separator

#
# Marginal distributions
#

# ---- main-sales-cost-vs-price-by-protein-source-and-continent

graph <- graph_marginal_distributions(sales, "COST", "PRICE", "PROTEIN_SOURCE", "CONTINENT")
save_png(graph, "./results/sales_cost_vs_price_by_protein_source_and_continent.png", 450, 450)

# ---- main-sales-cost-vs-price-by-protein-status-and-paid

graph <- graph_marginal_distributions(sales, "COST", "PRICE", "STATUS", "PAID")
save_png(graph, "./results/sales_cost_vs_price_by_status_and_paid.png", 450, 450)

# ---- mains-sales-price-vs-profit-ratio-by-protein-source-and-continent

graph <- graph_marginal_distributions(sales, "PRICE", "PROFIT_RATIO", "PROTEIN_SOURCE", "CONTINENT")
save_png(graph, "./results/sales_price_vs_profit_ratio_by_protein_source_and_continent.png", 450, 450)

# ---- main-clients-birth-dates-by-gender-and-stars

graph <- graph_marginal_distributions_client_birth_dates(clients)
save_png(graph, "./results/client_birth_dates_by_gender_and_stars.png", 900, 450)

# ---- section-separator

#
# Radar graphs
#

# ---- main-sales-top-clients-average-macros-in-last-30-days

subset <- filter_data(sales, 30, 5, "CLIENT_ID")
graph  <- graph_radar(subset, "CLIENT_ID")
save_png(graph, "./results/sales_top_clients_average_macros_in_last_30_days.png", 900, 300)

# ---- section-separator

#
# 3D interactive graphs
#

# ---- main-sales-3d-macronutrients

# plot3d(sales$PROTEIN, sales$CARBS, sales$FAT)

# ---- main-sales-3d-profit-ratio

# plot3d(sales$PROFIT_RATIO, sales$PRICE, sales$QUANTITY)

# ---- section-separator

#
# Time series (last n days)
#

# ---- main-sales-frequency-in-last-30-days

graph <- graph_last_n_days(sales, 30)
save_png(graph, "./results/sales_frequency_in_last_30_days.png", 900, 300)

# ---- main-sales-profit-in-last-30-days

graph <- graph_last_n_days(sales, 30, "PROFIT")
save_png(graph, "./results/sales_profit_in_last_30_days.png", 900, 300)

# ---- main-sales-profit-ratio-in-last-30-days

graph <- graph_last_n_days(sales, 30, "PROFIT_RATIO")
save_png(graph, "./results/sales_profit_ratio_in_last_30_days.png", 900, 300)

# ---- main-sales-frequency-by-protein-source-in-30-last-days

graph <- graph_last_n_days(sales, 30, color = "PROTEIN_SOURCE")
save_png(graph, "./results/sales_frequency_by_protein_source_in_last_30_days.png", 900, 300)

# ---- main-sales-profit-by-protein-source-in-30-last-days

graph <- graph_last_n_days(sales, 30, "PROFIT", "PROTEIN_SOURCE")
save_png(graph, "./results/sales_profit_by_protein_source_in_last_30_days.png", 900, 300)

# ---- main-sales-profit-ratio-by-protein-source-in-30-last-days

graph <- graph_last_n_days(sales, 30, "PROFIT_RATIO", "PROTEIN_SOURCE")
save_png(graph, "./results/sales_profit_ratio_by_protein_source_in_last_30_days.png", 900, 300)

# ---- main-client-messages-stars-in-30-last-days

aux <- client_messages
aux$STARS <- as.numeric(aux$STARS)
graph <- graph_last_n_days(aux, 30, "STARS")
save_png(graph, "./results/client_messages_stars_in_last_30_days.png", 900, 300)

#
# TODO: DELETE
#

# ---- main-sales-top-clients-macros-evolution

# subset <- filter_data(sales, NULL, 5, "CLIENT_ID", FALSE)
# graph  <- graph_top_n_interactive(subset, "CLIENT_ID")
# save_png(graph, "./results/sales_top_clients_macros_evolution_static.png", 800, 400)
# ggplotly(graph + theme(legend.position="none"))

# ---- main-sales-continents-macros-evolution

# subset <- filter_data(sales, NULL, 5, "CONTINENT", FALSE)
# graph  <- graph_top_n_interactive(subset, "CONTINENT")
# save_png(graph, "./results/sales_continents_macros_evolution_static.png", 800, 400)
# ggplotly(graph + theme(legend.position="none"))

# ---- section-separator

#
# Map animations
#

# ---- main-client-messages-map-static

graph <- graph_client_messages_static(client_messages, sales)
save_png(graph, "./results/client_messages_map_static.png", 900, 600)

# ---- main-client-messages-map-animation

# graph <- graph_client_messages_static(client_messages, sales, TRUE)
# ggplotly(graph)

# ---- section-separator

#
# Interactive maps
#

# ---- main-client-messages-map-interactive

graph <- graph_client_messages_interactive(client_messages, sales)
print(graph)

# ---- section-separator

#
# Globe
#

# ---- main-client-messages-globe

# graph <- graph_client_messages_in_globe(client_messages, sales)
# print(graph)

# ---- section-separator

print("Done.")
