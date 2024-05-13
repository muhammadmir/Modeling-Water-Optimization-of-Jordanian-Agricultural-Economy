# This is the analysis file, it's designed to assess results from Run.R
# Run.R should be manually ran up to to but not including "Running Optimization"
# section so all necessary variables are computed and ready to use here.

library(tidyverse)
library(mco)

# Helpers
source("./Helpers/Optimization Helpers.R")
source("./Helpers/Visualization Helpers.R")

# Visualizing Results
# ==============================================================================
filename <- "./50000G - Scenario B.RData"
load(filename)

# To Beat
c(max_water_use, min_revenue)

# Switch signs of second obj (revenue)
res$value[, 2] <- res$value[, 2] * -1

# Whether or not to filter by Pareto optimal solutions
only_pareto_optimal <- FALSE

# Extract output values as a tibble nicely
out_df <- (res[[2]] %>%
    as_tibble(.name_repair = ~ paste0("f_", seq_along(.))) %>%
    filter(res[[3]] == only_pareto_optimal)) / 1E6

# Objective Function Space
print_objective_space_diagram(
    out_df,
    "Water Usage (MCM)",
    "Net Revenue (M USD)",
    max_water_use, # Optional if want to plot baseline value
    min_revenue, # Optional, if want to plot baseline value
    only_pareto_optimal # Optional, whether or not out_df is Pareto Front
)

# Extract input values as tibble nice
in_df <- (res[[1]] %>%
    as_tibble(.name_repair = ~ paste0("x_", seq_along(.))) %>%
    filter(res[[3]] == only_pareto_optimal)) / 1E6

unflatten_input_row <- function(row) {
    x <- row %>% unname() %>% convert_to_tibbles(items, .)
    return(x)
}

# Convert input values (design variables) back to matrix (tibble) format
in_formatted_df <- do.call(rbind, apply(in_df, 1, unflatten_input_row))

# Extract information of random solution (Random Design Point r)
r <- 1
r_pq_df <- pluck(in_formatted_df[r, ], 1, 1)
r_iq_df <- pluck(in_formatted_df[r, ], 2, 1)
r_eq_df <- pluck(in_formatted_df[r, ], 3, 1)

# Water Use of Random Solution
f_1(r_pq_df, r_iq_df, r_eq_df) / 1E6

# Revenue of Random Solution
f_2(r_pq_df, r_iq_df, r_eq_df) * -1 / 1E6

# Quantity Dist
print_pie_donut_diagram(r_pq_df, r_iq_df, r_eq_df)

# Revenue Dist
print_pie_donut_diagram(r_pq_df * pr_df, r_iq_df * ir_df, r_eq_df * er_df)

# Calculating Percent Change of Random Design Point From Baseline
r_pq_df <- r_pq_df %>% round(0)
r_iq_df <- r_iq_df %>% round(0)
r_eq_df <- r_eq_df %>% round(0)

agg_pq_df <- ((pq_df) %>% colSums %>% round(3))
agg_r_pq_df <- ((r_pq_df) %>% colSums %>% round(3))

(((agg_r_pq_df - agg_pq_df) / agg_pq_df) * 100) %>% round(3)

agg_iq_df <- ((iq_df) %>% colSums %>% round(3))
agg_r_iq_df <- ((r_iq_df) %>% colSums %>% round(3))

(((agg_r_iq_df - agg_iq_df) / agg_iq_df) * 100) %>% round(3)

agg_eq_df <- ((eq_df) %>% colSums %>% round(3))
agg_r_eq_df <- ((r_eq_df) %>% colSums %>% round(3))

(((agg_r_eq_df - agg_eq_df) / agg_eq_df) * 100) %>% round(3)
# ==============================================================================