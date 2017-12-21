#
# Chapter 07 - Developing Automatic Sales Reports : Presentation
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

source("../functions.R")

# ---- pres-load-data

all_time  <- readRDS("../../chapter-04/results/sales.rds")

# ---- pres-filter-n-days-back

max_date  <- max(all_time$DATE)
this_week <- filter_n_days_back(all_time, 7, max_date)
last_week <- filter_n_days_back(all_time, 7, max_date - 7)

# ---- pres-proportions-tables

quantity_all   <- proportions_table(all_time, "QUANTITY")
continent_all  <- proportions_table(all_time, "CONTINENT")
protein_all    <- proportions_table(all_time, "PROTEIN_SOURCE")

quantity_last  <- proportions_table(last_week, "QUANTITY")
continent_last <- proportions_table(last_week, "CONTINENT")
protein_last   <- proportions_table(last_week, "PROTEIN_SOURCE")

quantity_this  <- proportions_table(this_week, "QUANTITY")
continent_this <- proportions_table(this_week, "CONTINENT")
protein_this   <- proportions_table(this_week, "PROTEIN_SOURCE")

# ---- pres-all-vs-this

difference_bars(quantity_all, quantity_this, "This week", "All-time")
difference_bars(continent_all, continent_this, "This week", "All-time")
difference_bars(protein_all, protein_this, "This week", "All-time")

# ---- pres-last-vs-this

difference_bars(quantity_last, quantity_this, "This week", "Last week")
difference_bars(continent_last, continent_this, "This week", "Last week")
difference_bars(protein_last, protein_this, "This week", "Last week")

# ---- pres-change-all-vs-this

change_lines(quantity_all, quantity_this, "This week", "All-time", 0.2)
change_lines(continent_all, continent_this, "This week", "All-time", 0.3)
change_lines(protein_all, protein_this, "This week", "All-time", 0.5)

# ---- pres-change-last-vs-this

change_lines(quantity_last, quantity_this, "This week", "Last week", 0.2)
change_lines(continent_last, continent_this, "This week", "Last week", 0.3)
change_lines(protein_last, protein_this, "This week", "Last week", 0.5)
