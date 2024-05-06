library(tidyverse)
library(treemapify)
library(FAOSTAT)
library(RColorBrewer)
library(webr)
library(networkD3)

source("./Helpers/Kc Functions.R")
source("./Helpers/FAO Helpers.R")
source("./Helpers/Visualization Helpers.R")

country_code <- 112
year_value <- 2019
TARGET <- "Jordan"

# Climate Data
# ===========================================================================
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

# Make map?
jordan_map <- map_data(map = "world", region = "Jordan")
ggplot() +
    geom_polygon(
        data = jordan_map,
        aes(x = long, y = lat, group = group),
        color = "black",
        fill = "white"
    ) +
    theme_classic(base_size = 16)

# Calculate ETo and Peff
monthly_df <- calc_ETo_and_Peff(monthly_df)

# Average ETo and Peff (temporally)
average_monthly_df <- monthly_df %>%
    group_by(Date) %>%
    summarize(ETo = mean(ETo), Peff = mean(Peff))

ggplot() +
    geom_line(
        data = monthly_df,
        aes(x = Date, y = ETo, color = Station)
    ) +
    geom_line(
        data = average_monthly_df,
        aes(x = Date, y = ETo, color = "Average")
    ) +
    scale_color_manual(
        name = "Station",
        values = c("black", "red", "green", "blue")) +
    labs(x = "Date", y = "ETo (mm)") +
    scale_x_date(date_breaks = "5 months") +
    theme_classic(base_size = 16)
ggsave("ETo vs. Time.png", path = "./Images/")

ggplot() +
    geom_line(
        data = monthly_df,
        aes(x = Date, y = Peff, color = Station)
    ) +
    geom_line(
        data = average_monthly_df,
        aes(x = Date, y = Peff, color = "Average")
    ) +
    scale_color_manual(
        name = "Station",
        values = c("black", "red", "green", "blue")) +
    labs(x = "Date", y = "Peff (mm)") +
    scale_x_date(date_breaks = "5 months") +
    theme_classic(base_size = 16)
ggsave("Peff vs. Time.png", path = "./Images/")
# ===========================================================================

# Crop Data
# ===========================================================================

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

min_plant_date <- min(sapply(crops, function(x) {return(x$plant_date)}))
max_harvest_date <- max(sapply(crops, function(x) {
    growing_stage <- x$growing_stage[[1]]
    crop_end_date <- x$plant_date + sum(growing_stage)
    return(crop_end_date)
}))

min_plant_year <- year(as.Date(min_plant_date, origin = "1970-01-01"))
max_plant_year <- year(as.Date(max_harvest_date, origin = "1970-01-01"))

date_by_day <- seq(
    ymd(paste(min_plant_year, "01", "01", sep = "-")),
    ymd(paste(max_plant_year, "12", "31", sep = "-")),
    by = "1 day"
)
date_by_month <- seq(
    ymd(paste(min_plant_year, "01", "01", sep = "-")),
    ymd(paste(max_plant_year, "12", "31", sep = "-")),
    by = "1 month"
)

master_Kc_curve <- tibble(Date = date_by_day)

append_to_master_Kc_curve <- function(crop) {
    Kc_curve <- calculate_Kc_curve(crop) %>% pull(Kc)

    Kc_curve <- c(
        Kc_curve,
        rep(0, length(master_Kc_curve$Date) - length(Kc_curve))
    )

    return(tibble(!!crop$crop := Kc_curve))
}

crop_cols <- bind_cols(lapply(crops, append_to_master_Kc_curve))

master_Kc_curve <- master_Kc_curve %>%
    bind_cols(crop_cols) %>%
    pivot_longer(cols = -Date, names_to = "Crop", values_to = "Kc")

ggplot(master_Kc_curve, aes(x = Date, y = Kc, color = Crop)) +
labs(x = "Date", y = "Kc Value") +
scale_x_date(date_breaks = "5 months") +
geom_line(size = 1) +
scale_color_brewer(palette = "Set1") +
theme_classic(base_size = 16)
ggsave("Expanded Kc Curves.png", path = "./Images/")

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

master_ETc_curve <- tibble(Date = date_by_month)

fn <- function(crop) {
    ETc_curve <- calculate_ETc(crop) %>% pull(ETc)

    ETc_curve <- c(
        ETc_curve,
        rep(0, length(master_ETc_curve$Date) - length(ETc_curve))
    )

    return(tibble(!!crop$crop := ETc_curve))
}

crop_cols <- bind_cols(sapply(crops, fn))

master_ETc_curve <- master_ETc_curve %>%
    bind_cols(crop_cols) %>%
    pivot_longer(cols = -Date, names_to = "Crop", values_to = "Kc")

ggplot(master_ETc_curve, aes(x = Date, y = Kc, color = Crop)) +
    labs(x = "Date", y = "ETc (mm)") +
    scale_x_date(date_breaks = "5 months") +
    geom_line(size = 1) +
    scale_color_brewer(palette = "Set1") +
    theme_classic(base_size = 16)
ggsave("ETc Curves.png", path = "./Images/")
# ===========================================================================

# Domestic Production Data
# ===========================================================================
# https://unstats.un.org/unsd/classifications/unsdclassifications/cpcv21.pdf
# Section 0, Division 01
# Products of agriculture, horticulture and market gardening
# Note: Doesn't include milled rice!
# Milled Rice is Section 2, Division 23, Class 2316
valid_groups <- "011|012|013|013|014|015|016|017|018|019"

prod_df <- get_raw_prod_df()

# Get relevant products and format df
prod_df <- prod_df %>%
    filter(
        area_code == country_code,
        year == year_value,
        grepl(valid_groups, substr(item_code__cpc_, 2, 4))
    ) %>%
    select(item_code, item, element, year, unit, value, flag) %>%
    rename(
        "Code" = item_code,
        "Item" = item,
        "Element" = element,
        "Year" = year,
        "Unit" = unit,
        "Value" = value,
        "Flag" = flag
    ) %>%
    rowwise() %>%
    mutate(
        Value = Value * correct_value(Unit),
        Unit = correct_unit(Unit)
    ) %>%
    arrange(Item)

# Get products that were domestically produced
prod_df <- prod_df %>%
    arrange(desc(Value)) %>%
    filter(Element == "production", Value > 0)

ggplot(
    prod_df,
    aes(
        area = Value,
        fill = Value,
        label = paste(Item, "\n", round(100 * Value / sum(Value), 2), "%")
    )
) +
geom_treemap(color = "black", size = 2) +
geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
scale_fill_gradient(low = "#D8B5FF", high = "#1EAE98") +
theme(legend.position = "none")
ggsave("Dom Prod Agri 2019.png", path = "./Images/")
# ===========================================================================

# Trade Data
# ===========================================================================
tm_df <- get_raw_tm_df()

tm_df <- tm_df %>%
    filter(
        reporter_country_code == country_code,
        year == year_value,
        grepl(valid_groups, substr(item_code__cpc_, 2, 4))
    ) %>%
    select(partner_countries, item_code, item, element, unit, value, flag) %>%
    rename(
        "Country" = partner_countries,
        "Code" = item_code,
        "Item" = item,
        "Element" = element,
        "Unit" = unit,
        "Value" = value,
        "Flag" = flag
    ) %>%
    rowwise() %>%
    mutate(
        Value = Value * correct_value(Unit),
        Unit = correct_unit(Unit)
    )

# Don't really need to do the whole pivot_wider, but why not?
tm_import_df <- tm_df %>%
    filter(grepl("import_quantity|import_value", Element)) %>%
    group_by(Item, Unit) %>%
    summarize(Value = sum(Value)) %>%
    pivot_wider(names_from = Unit, values_from = Value)

tm_export_df <- tm_df %>%
    filter(grepl("export_quantity|export_value", Element)) %>%
    group_by(Item, Unit) %>%
    summarize(Value = sum(Value)) %>%
    arrange(desc(Value)) %>%
    pivot_wider(names_from = Unit, values_from = Value)

# Imports
ggplot(
    tm_import_df,
    aes(
        area = USD,
        fill = USD,
        label = paste0(
            Item, "\n",
            prettyNum(USD, big.mark = ",", scientific = FALSE), " USD\n",
            round(100 * USD / sum(USD), 2), "%"
        )
    )
) +
geom_treemap(color = "black", size = 2) +
geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
scale_fill_gradient(low = "#D8B5FF", high = "#1EAE98") +
theme(legend.position = "none")
ggsave("Import Agri 2019.png", path = "./Images/")

# Exports
ggplot(
    tm_export_df,
    aes(
        area = USD,
        fill = USD,
        label = paste0(
            Item, "\n",
            prettyNum(USD, big.mark = ",", scientific = FALSE), " USD\n",
            round(100 * USD / sum(USD), 2), "%"
        )
    )
) +
geom_treemap(color = "black", size = 2) +
geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
scale_fill_gradient(low = "#D8B5FF", high = "#1EAE98") +
theme(legend.position = "none")
ggsave("Export Agri 2019.png", path = "./Images/")

items <- c(
    "Tomatoes", "Peaches and nectarines", "Cucumbers and gherkins",
    "Apricots", "Barley", "Wheat", "Maize (corn)"
)

tm_import_df %>% ungroup() %>%
filter(Item %in% items) %>% summarise(sum(USD)) / sum(tm_import_df$USD)
# ===========================================================================

# Case Study Data
# ===========================================================================
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

wf_df <- crop_df %>%
    mutate(WF_total = WF_blue + WF_green) %>%
    select(crop, WF_total) %>%
    pivot_wider(names_from = crop, values_from = WF_total)

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

# From Production Data (Units: Tonne)
dom_prod_qty <- prod_df %>% filter(Element == "production") %>% pull(Value)

pq_df <- tibble(items, dom_prod_qty) %>% spread(items, dom_prod_qty)

# From Trade Data (Units: Tonne)
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
pq_df <- (pq_df / 1E6) %>% as_tibble()
iq_df <- (iq_df / 1E6) %>% as_tibble()
eq_df <- (eq_df / 1E6) %>% as_tibble()
dom_demand_df <- (dom_demand_df / 1E6) %>% as_tibble()

# Baseline maximum water use
max_water_use <- f_1(pq_df, iq_df, eq_df) # Units: MCM

# Baseline minimum net revenue
min_revenue <- f_2(pq_df, iq_df, eq_df) * -1 # Units: M USD

# Quantity Distribution
print_pie_donut_diagram(pq_df, iq_df, eq_df)

# WF Distribution
prod_wf <- (pq_df * wf_df) %>% as_tibble()
import_wf <- ((iq_df %>% colSums) * wf_df) %>% as_tibble()
export_wf <- ((eq_df %>% colSums) * wf_df) %>% as_tibble()


# Revenue Distribution
print_pie_donut_diagram(pq_df * pr_df, iq_df * ir_df, eq_df * er_df)

# ===========================================================================


# Trace Plot
# ===========================================================================

# Defining Constant
max_water_use <- -9517.037
min_revenue <- -103.9162


# Loading Solutions
out_df <- tibble()
file_df <- tibble(
    location = c(
        "./Solutions/50000G - 100P - 3 Import 4 Export Reverse F1.RData",
        "./Solutions/100000G - 100P - 3 Import 4 Export.RData",
        "./Solutions/200000G - 100P - 3 Import 4 Export.RData"
        ),
    name = c("50,000 Generations", "100,000 Generations", "200,000 Generations")
)

for (index in seq_len(length(filenames))) {
    load(file_df[index, ]$location)
    res$value[, 2] <- res$value[, 2] * -1

    temp_df <- res[[2]] %>%
        as_tibble(.name_repair = ~ paste0("f_", seq_along(.))) %>%
        mutate(solution_type = file_df[index, ]$name)

    print((temp_df %>% summarize(f_1_mean = mean(f_1), f_2_mean = mean(f_2))))

    out_df <- out_df %>% bind_rows(temp_df)
}


print_objective_space_diagram(
    (out_df %>% select(-solution_type)),
    "Water Usage (MCM)",
    "Net Revenue (M USD)",
    max_water_use,
    min_revenue,
    FALSE
)

ggplot(out_df)
