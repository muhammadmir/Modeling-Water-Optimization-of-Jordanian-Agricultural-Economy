library(tidyverse)
library(mco)

# Helpers
source("./Helpers/Optimization Helpers.R")
source("./Helpers/Visualization Helpers.R")

TARGET <- "Country T"
YEAR <- 2019

# Defining Objectives and Constraints
# ==============================================================================
# Minimzing water use
f_1 <- function(pq_df, iq_df, eq_df) {
    wf <- crop_df$WF_green + crop_df$WF_blue

    # Water Use by Commodity (Produdction, Import, Export)
    # Sum total quantity of each type and multiply by WF.
    pw_df <- colSums(pq_df) * wf
    iw_df <- colSums(iq_df) * wf
    ew_df <- colSums(eq_df) * wf

    # Net Water Use (By Commodity)
    nwu <- (ew_df - iw_df) + pw_df

    # Aggregate Water Use (Sum of All Commodity)
    awu <- unname(sum(nwu))

    return(awu)
}

# Maximize net revenue
f_2 <- function(pq_df, iq_df, eq_df) {
    # Total Revenue by Commodity (Proudction, Import, Export)
    pt_df <- colSums(pq_df * pr_df)
    it_df <- colSums(iq_df * ir_df)
    et_df <- colSums(eq_df * er_df)

    # Net Total Revenue (By Commodity)
    nt <- (et_df - it_df) + pt_df

    # Aggregate Net Total Revenue
    ant <- unname(sum(nt))

    return(ant * - 1)
}

# Reliable Supply: Net qty greater than demand
g_1 <- function(pq_df, iq_df, eq_df) {
    # Net Quantity (By Commodity)
    nq_df <- colSums(iq_df - eq_df) + colSums(pq_df)

    return(sum(nq_df - dom_demand_df))
}

# Net revenue greater than baseline
g_2 <- function(pq_df, iq_df, eq_df) {
    # Net Total Revenue (Reverse Condition)
    ant <- -1 * f_2(pq_df, iq_df, eq_df)

    return(ant - min_revenue)
}

# Security of internal production
g_3 <- function(pq_df) {
    return(sum(pq_df - (dom_demand_df / 2)))
}

# Water use does not exceed baseline
g_4 <- function(pq_df, iq_df, eq_df) {
    awu <- f_1(pq_df, iq_df, eq_df)

    return((awu - max_water_use) * -1)
}
# ==============================================================================

# Example Data
# ==============================================================================
items <- c("Apple", "Banana", "Orange")
countries <- c("Country A", "Country B", "Country C")

# Quantity Tibbles (Tonne)
pq_df <- tibble(
    !!items[1] := 100,
    !!items[2] := 400,
    !!items[3] := 500
)

iq_df <- tibble(
    !!items[1] := c(50, 80, 50),
    !!items[2] := c(10, 40, 60),
    !!items[3] := c(14, 29, 43)
)

eq_df <- tibble(
    !!items[1] := c(10, 30, 50),
    !!items[2] := c(20, 70, 60),
    !!items[3] := c(20, 100, 80)
)

# Contains WF_blue and WF_green of crop (m ^ 3 / Tonne)
crop_df <- tibble(
    WF_blue = c(10000, 900, 200),
    WF_green = c(0, 0, 0),
    WF_total = WF_blue + WF_green
)

# Baseline Domestic Demand (Tonne)
dom_demand_df <- tibble(
    !!items[1] := pq_df$Apple + sum(iq_df$Apple) - sum(eq_df$Apple),
    !!items[2] := pq_df$Banana + sum(iq_df$Banana) - sum(eq_df$Banana),
    !!items[3] := pq_df$Orange + sum(iq_df$Orange) - sum(eq_df$Orange)
)

# Monteary Rate Tibbles (USD / Tonne)
pr_df <- tibble(
    !!items[1] := 350,
    !!items[2] := 230,
    !!items[3] := 90
)

ir_df <- tibble(
    !!items[1] := c(300, 250, 200),
    !!items[2] := c(230, 100, 240),
    !!items[3] := c(100, 140, 180)
)

er_df <- tibble(
    !!items[1] := c(200, 90, 190),
    !!items[2] := c(310, 230, 160),
    !!items[3] := c(90, 150, 20)
)

# Baseline maximum water use
max_water_use <- f_1(pq_df, iq_df, eq_df) # m ^ 3

# Baseline minimum net revenue
min_revenue <- f_2(pq_df, iq_df, eq_df) * -1 # USD

# Visuals

# Quantity Dist
print_pie_donut_diagram(pq_df, iq_df, eq_df)

# Revenue Dist
print_pie_donut_diagram(
    pq_df * pr_df,
    iq_df * ir_df,
    eq_df * er_df
)

wf <- crop_df$WF_green + crop_df$WF_blue

pw_df <- colSums(pq_df) * wf
iw_df <- colSums(iq_df) * wf
ew_df <- colSums(eq_df) * wf

nwu <- (ew_df - iw_df) + pw_df

water_df <- tibble(
    "Group" = names(nwu),
    "Value" = unname(nwu),
    "Percent" = (Value / sum(Value)) * 100
)

ggplot(water_df, aes(x = "", y = Value, fill = Group)) +
    geom_bar(stat = "identity", color = "white", width = 1.1) +
    geom_label(
        aes(label = prettyNum(Value, big.mark = ",", scientific = FALSE)),
        color = "white",
        position = position_stack(vjust = 0.5),
        show.legend = FALSE
    ) +
    coord_polar("y", start = 0) +
    annotate("text", x = 0, y = 0, label = "Pie Chart Title", size = 5, color = "black") +
    theme_void()
# ==============================================================================

# Running Optimization
# ==============================================================================
fitness_fn <- function(x) {
    y <- numeric(2)

    dfs <- converter(items, x)

    pq_df <- dfs$Production[[1]]
    iq_df <- dfs$Import[[1]]
    eq_df <- dfs$Export[[1]]

    y[1] <- f_1(pq_df, iq_df, eq_df)
    y[2] <- f_2(pq_df, iq_df, eq_df)

    return(y)
}

constraint_fn <- function(x) {
    y <- numeric(4)

    dfs <- converter(items, x)

    pq_df <- dfs$Production[[1]]
    iq_df <- dfs$Import[[1]]
    eq_df <- dfs$Export[[1]]

    y[1] <- g_1(pq_df, iq_df, eq_df)
    y[2] <- g_2(pq_df, iq_df, eq_df)
    y[3] <- g_3(pq_df)
    y[4] <- g_4(pq_df, iq_df, eq_df)

    return(y)
}

run_nsga2 <- function(gensize, popsize) {
    x_dim <- length(pq_df) + 2 * nrow(iq_df) * ncol(iq_df)
    upper_limit <- unconverter(pq_df, iq_df, eq_df)
    upper_limit[upper_limit == 0] <- 0.0000001 # 0 cannot be upper limit.

    start <- Sys.time()
    res <- nsga2(
        fitness_fn, idim = x_dim, odim = 2,
        constraints = constraint_fn, cdim = 4,
        generations = gensize, popsize = popsize,
        lower.bounds = rep(0, x_dim),
        upper.bounds = upper_limit
    )
    end <- Sys.time()

    print(end - start) # Total elapsed time
    return(res)
}

gensize <- 1000
popsize <- 100
specifics <- "Example Problem"
print(paste("Running Optimization", gensize, popsize, specifics, sep = " | "))

res <- run_nsga2(gensize, popsize)
save(
    res,
    file = paste0(gensize, "G", " - ", popsize, "P", " - ", specifics, ".RData")
)
# ==============================================================================

# Visualizing Results
# ==============================================================================
filename <- "./Solutions/1000G - 100P - Example Problem.RData"
load(filename)

# To Beat
c(max_water_use, min_revenue)

# Switch Signs
res$value[, 2] <- res$value[, 2] * -1

plot(res,
    xlab = "Max Water Usage (m^3)",
    ylab = "Minimum Revenue (USD)",
    main = "Objective Space"
)

# Outputs
out_df <- res[[2]] %>%
    as_tibble(.name_repair = ~ paste0("f_", seq_along(.))) %>%
    filter(res[[3]])


# Objective Function Space
print_objective_space_diagram(
    out_df,
    "Water Usage (m^3)",
    "Net Revenue (USD)",
    max_water_use,
    min_revenue
)
ggsave("Example Objective Function Space.png", path = "./Images/")

# Parameters
in_df <- res[[1]] %>%
    as_tibble(.name_repair = ~ paste0("x_", seq_along(.))) %>%
    filter(res[[3]]) %>%
    round(2)

unflatten_input_row <- function(row) {
    x <- row %>% unname() %>% converter(items, .)
    return(x)
}

items <- c("Apple", "Banana", "Orange")
countries <- c("Country A", "Country B", "Country C")

in_formatted_df <- do.call(rbind, apply(in_df, 1, unflatten_input_row))

# Visualze a random parameter set of index r
r <- 20
r_pq_df <- pluck(in_formatted_df[r, ], 1, 1)
r_iq_df <- pluck(in_formatted_df[r, ], 2, 1)
r_eq_df <- pluck(in_formatted_df[r, ], 3, 1)

# Quantity Dist (Solution)
print_pie_donut_diagram(r_pq_df, r_iq_df, r_eq_df)

# Revenue Dist (Solution)
print_pie_donut_diagram(
    r_pq_df * pr_df,
    r_iq_df * ir_df,
    r_eq_df * er_df
)

out_df[r, ]

# ==============================================================================