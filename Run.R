library(tidyverse)
library(maps)
library(riem)
library(weathermetrics)
library(SPEI)
library(FAOSTAT)
library(mco)
library(webr)

# Helpers
source("./Helpers/FAO Helpers.R")
source("./Helpers/Kc Functions.R")
source("./Helpers/METAR Helpers.R")
source("./Helpers/Optimization Helpers.R")
source("./Helpers/Visualization Helpers.R")

YEAR <- 2019
YEARS_BACK <- 1

# Calculating ETo and Peff from METAR Weather Data
# ==============================================================================
network_code <- "JO__ASOS"
start_date <- ymd(paste0((YEAR - YEARS_BACK), "-01-01"))
end_date <- ymd(paste0(YEAR, "-12-31"))

# Get monthly average information
monthly_df <- get_monthly_df(network_code, start_date, end_date) %>%
    arrange(Station)

# Load JMD data
precip_df <- read.csv("./Data/JMD Precip 2018-2019.csv") %>% as_tibble()
sunshine_df <- read.csv("./Data/JMD Sunshine 2018-2019.csv") %>% as_tibble()

# Helpful function for pulling data from JMD data.
pull_data <- function(station, df) {
    values <- df %>%
        filter(Station == station) %>%
        select(-c(Station, Element, YEAR)) %>%
        pivot_longer(cols = everything()) %>%
        pull(value)

    return(values)
}

# Append Sunshine Data
monthly_df["Sunshine"] <- c(
    pull_data("Q.A.I.Airport", sunshine_df),
    pull_data("Amman Airport", sunshine_df),
    pull_data("King Hussien International Airport", sunshine_df)
)

# Append Precip Data
monthly_df["Precip"] <- c(
    pull_data("Q.A.I.Airport", precip_df),
    pull_data("Amman Airport", precip_df),
    pull_data("King Hussien International Airport", precip_df)
)

# Calculate ETo and Peff
monthly_df <- calc_ETo_and_Peff(monthly_df)

# Question: Sub average monthly Peff from spreadsheet instead?

# Average ETo and Peff (temporally)
monthly_df <- monthly_df %>%
    group_by(Date) %>%
    summarize(ETo = mean(ETo), Peff = mean(Peff))
# ==============================================================================

# Selecting Crops and Calculating their ETc
# ==============================================================================

# General Notes
# ETo is lowest typically in November - March
# Strategy should be to plant in November and harvest in March
# Most of the growth period should fall between those months
# Help: https://www.researchgate.net/publication/265013042_Optimization_of_the_Cropping_Pattern_in_Northern_and_Southern_part_of_Jordan_Valley_under_drought_conditions_and_limited_water_availability

# Assumptions
# L: Low alittude
# Kc: No ground cover, no frost
apricot_crop <- tibble(
    crop = "Apricot",
    plant_date = as.Date("2019-03-01"),
    growing_stage = list(tibble(init = 20, dev = 70, mid = 120, late = 60)),
    Kc_value = list(tibble(init = 0.55, mid = 0.90, end = 0.65))
)

# Assumptions
# L: November planting date
barley_crop <- tibble(
    crop = "Barley",
    plant_date = as.Date("2018-11-01"),
    growing_stage = list(tibble(init = 40, dev = 60, mid = 60, late = 40)),
    Kc_value = list(tibble(init = 0.3, mid = 1.15, end = 0.25))
)

# Assumptions
# L: Arid region, November is cooler
# Kc: Fresh market
cucumber_crop <- tibble(
    crop = "Cucumber",
    plant_date = as.Date("2018-11-01"),
    growing_stage = list(tibble(init = 25, dev = 35, mid = 50, late = 20)),
    Kc_value = list(tibble(init = 0.6, mid = 1.00, end = 0.75))
)

# Assumptions
# L: Arid region, December is cooler
# Kc: Harvested after grain is dried
maize_crop <- tibble(
    crop = "Maize",
    plant_date = as.Date("2018-12-01"),
    growing_stage = list(tibble(init = 25, dev = 40, mid = 45, late = 30)),
    Kc_value = list(tibble(init = 0.3, mid = 1.20, end = 0.35))
)

# Assumptions
# L: Low altitude
# Kc: No ground cover, no frost
peach_crop <- tibble(
    crop = "Peach",
    plant_date = as.Date("2019-03-01"),
    growing_stage = list(tibble(init = 20, dev = 70, mid = 120, late = 60)),
    Kc_value = list(tibble(init = 0.55, mid = 0.90, end = 0.65))
)

# Assumptions
# L: Arid climate, November is cooler
tomato_crop <- tibble(
    crop = "Tomato",
    plant_date = as.Date("2018-11-01"),
    growing_stage = list(tibble(init = 35, dev = 45, mid = 70, late = 30)),
    Kc_value = list(tibble(init = 0.60, mid = 1.15, end = 0.70))
)

# Assumptions
# L: Winter wheat, November is cooler
# Kc: Non-frozen soil, hand harvested
wheat_crop <- tibble(
    crop = "Wheat",
    plant_date = as.Date("2018-11-01"),
    growing_stage = list(tibble(init = 30, dev = 140, mid = 40, late = 30)),
    Kc_value = list(tibble(init = 0.70, mid = 1.15, end = 0.40))
)

# Selected crops
crops <- list(
    wheat_crop, maize_crop, barley_crop, # Imports
    tomato_crop, apricot_crop, peach_crop, cucumber_crop # Exports
)

calculate_ETc <- function(crop) {
    # Daily time series
    Kc_curve <- calculate_Kc_curve(crop)

    # Monthly time series
    Kc_curve_monthly <- mean_monthly_Kc(Kc_curve, crop)

    # Determining monthly ETc
    # We calculate x values of monthly_ETc and y values of Kc_curve_monthly,
    # where x and y are multiples of 12. In the event x != y, we must get
    # the last (x - y) results of Kc_curve_monthly as they will
    # chronologically compare with Kc_curve_monthly.
    #
    # Ex: If monthly_ETc is calculated for years 2019 and 2020 but
    # Kc_curve_monthly only returns values for 2020, the last 12 results of
    # monthly_ETc will be for 2020.
    monthly_ETc <- monthly_df %>%
        tail(length(Kc_curve_monthly)) %>%
        pull(ETo) * Kc_curve_monthly

    # Saving as a df
    crop_df <- monthly_df %>%
        tail(length(monthly_ETc)) %>%
        mutate(
            ETc = monthly_ETc,
            Peff = ifelse(Peff != 0, Peff, 0)
        ) %>%
        select(Date, ETo, ETc, Peff)

    return(crop_df)
}

calc_stats <- function(crop) {
    crop_df <- calculate_ETc(crop)

    crop_df <- tibble(
        crop = crop$crop,
        CWR = crop_df %>% pull(ETc) %>% sum(), # Units: mm
        Peff = crop_df %>% pull(Peff) %>% sum(), # Units: mm
        ET_blue = max(0, CWR - Peff), # Units: mm
        ET_green = min(CWR, Peff), # Units: mm
        CWU_blue = 10 * ET_blue, # Units: m^3 / ha
        CWU_green = 10 * ET_green # Units: m^3 / ha
    )

    return(list(crop_df))
}

crop_df <- bind_rows(sapply(crops, calc_stats)) %>% arrange(crop)
# ==============================================================================

# Loading FAO Data
# ==============================================================================
country_code <- 112
item_codes <- c(
    526, # Apricot
    44, # Barley
    397, # Cucumber
    56, # Maize
    534, # Peach
    388, # Tomato
    15 # Wheat
)

# Only run once.
# save_prod_df(country_code, c(), YEAR)

prod_df <- read.csv("./Data/FAO Prod Filtered.csv") %>%
    as_tibble() %>%
    filter(Code %in% item_codes) %>%
    select(-Code) %>%
    mutate(
        Item = case_when(
            Item == "Apricots" ~ "Apricot",
            Item == "Cucumbers and gherkins" ~ "Cucumber",
            Item == "Maize (corn)" ~ "Maize",
            Item == "Peaches and nectarines" ~ "Peach",
            Item == "Tomatoes" ~ "Tomato",
            .default = Item
        )
    )

# Adding Maize missing Yield
prod_df <- prod_df %>%
    add_row( # Maize Yield (1)
        Item = "Maize",
        Element = "yield",
        Year = 2019,
        Unit = "tonne/ha",
        Value = 1,
        Flag = "M"
    ) %>%
    arrange(Item)

# Fixed Values (WF_blue / WF_green): Used later in optimization.
crop_df <- set_wf(crop_df, prod_df)

# Only run once.
# save_tm_df(country_code, c(), YEAR)

tm_df <- read.csv("./Data/FAO TM Filtered.csv") %>%
    as_tibble() %>%
    filter(Code %in% item_codes) %>%
    select(-Code) %>%
    mutate(
        Item = case_when(
            Item == "Apricots" ~ "Apricot",
            Item == "Cucumbers and gherkins" ~ "Cucumber",
            Item == "Maize (corn)" ~ "Maize",
            Item == "Peaches and nectarines" ~ "Peach",
            Item == "Tomatoes" ~ "Tomato",
            .default = Item
        )
    )

# All countries involved in trade
countries <- tm_df %>%
    distinct(Country) %>%
    pull() %>%
    sort()

# All items involved in trade
items <- tm_df %>%
    distinct(Item) %>%
    pull() %>%
    sort()

# I/E rate and I/E quantity dataframes
tm_df <- get_full_tm(tm_df, countries, items)
ie_rq_dfs <- get_rate_and_qty_tm(tm_df, countries, items)

# Fixed Values: Used later in optimization.
ir_df <- ie_rq_dfs[[1]]
er_df <- ie_rq_dfs[[2]]
# ==============================================================================

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

# Calculating Baseline Values
# ==============================================================================
# From Production Data
dom_prod_qty <- prod_df %>%
    filter(Element == "production") %>%
    pull(Value) # Units: Tonne

pq_df <- tibble(items, dom_prod_qty) %>% spread(items, dom_prod_qty)

# From Trade Data
iq_df <- ie_rq_dfs[[3]]
eq_df <- ie_rq_dfs[[4]]

net_demand <- dom_prod_qty + unname(colSums(iq_df - eq_df)) # Units: Tonne
dom_demand_df <- tibble(items, net_demand) %>% spread(items, net_demand)

# From FAO Directly (USD / Tonne)
dom_rate <- c(
    852, # Apricot
    492.3, # Barley
    324.1, # Cucumber
    286.1, # Maize
    676.3, # Peach
    172.5, # Tomato
    563.2 # Wheat
)
pr_df <- tibble(items, dom_rate) %>% spread(items, dom_rate)

# Divide by Million
pq_df <- (pq_df) %>% as_tibble()
iq_df <- (iq_df) %>% as_tibble()
eq_df <- (eq_df) %>% as_tibble()
dom_demand_df <- dom_demand_df

# Baseline maximum water use
max_water_use <- f_1(pq_df, iq_df, eq_df) # Units: m^3

# Baseline minimum net revenue
min_revenue <- f_2(pq_df, iq_df, eq_df) * -1 # Units: USD
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

gensize <- 200000
popsize <- 100
specifics <- "Half Food"
print(paste("Running Optimization", gensize, popsize, specifics, sep = " | "))

res <- run_nsga2(gensize, popsize)
save(
    res,
    file = paste0(gensize, "G", " - ", popsize, "P", " - ", specifics, ".RData")
)
