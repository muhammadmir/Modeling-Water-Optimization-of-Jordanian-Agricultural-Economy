library(FAOSTAT)

#' Correct FAO unit value to a standard unit value.
#'
#' This function takes an FAO unit and determines factor to multiply by to
#' convert to a standard unit. This function and correct_unit are correlated.
#'
#' @param unit A string value.
#'
#' @return The numeric factor to multiply by.
#'
correct_value <- function(unit) {
    factor <- 0

    if (unit == "ha") factor <- 1
    else if (unit == "hg/ha" || unit == "100 g/ha") factor <- 0.0001
    else if (unit == "tonnes" || unit == "t") factor <- 1
    else if (unit == "1000 US$" || unit == "1000 USD") factor <- 1000
    else if (unit == "An") factor <- 1
    else print(paste0("Unknown Unit: ", unit))

    return(factor)
}

#' Correct FAO unit name to a standard unit name.
#'
#' This function takes an FAO unit and corrects it to a standard unit. This
#' function and correct_value are correlated.
#'
#' @param unit A string value.
#'
#' @return A string value with correct standard unit.
#'
correct_unit <- function(unit) {
    correct_unit <- NA

    if (unit == "ha") correct_unit <- "ha"
    else if (unit == "hg/ha" || unit == "100 g/ha") correct_unit <- "tonne/ha"
    else if (unit == "tonnes" || unit == "t") correct_unit <- "tonne"
    else if (unit == "1000 US$" || unit == "1000 USD") correct_unit <- "USD"
    else if (unit == "An") correct_unit <- "Animal"

    else print(paste0("Unknown Unit: ", unit))

    return(correct_unit)
}

#' Sets water footprint (blue and green) of the crops based on FAO data to
#' crop_df.
#'
#' This function takes the crops and FAO domestic production data to calculate
#' the blue and green water footprints of each crop.
#'
#' @param crop_df A tibble.
#' @param prod_df A tibble.
#'
#' @return Modified crop_df with WF_blue and WF_green as new columns.
#'
set_wf <- function(crop_df, prod_df) {
    yield <- prod_df %>%
        filter(Element == "yield") %>%
        pull(Value) # Units: tonne / ha

    crop_df <- crop_df %>%
        mutate(
            WF_blue = CWU_blue / yield,
            WF_green = CWU_green / yield
        )

    return(crop_df)
}

#' An internal function utilized by get_full_tm.
#'
#' This function takes the trade matrix and the cartesian product of the
#' Countries and Items to add in the missing rows of Countries who one or more
#' Items are imported from but one or more Items are not exported to, and
#' vice-versa.
#'
#' @param tm_df A tibble.
#' @param combinations A tibble.
#'
#' @return The full trade matrix as a tibble.
#'
add_missing_rows <- function(tm_df, combinations) {
    for (prefix in c("import", "export")) {
        # All Countries by Item that do have current prefix
        prefix_countries <- tm_df %>%
            group_by(Country, Item) %>%
            filter(grepl(prefix, Element)) %>%
            select(Country, Item) %>%
            distinct()

        # All Countries by Item that do NOT have current prefix. Creates two
        # of each row (one for import_quantity, other for import_value)
        non_prefix_countries <- combinations %>%
            anti_join(prefix_countries, by = join_by(Country, Item)) %>%
            slice(rep(row_number(), each = 2))

        # Fill in the rows
        expanded <- non_prefix_countries %>%
            mutate(
                Element = ifelse(
                    row_number() %% 2 == 0,
                    paste0(prefix, "_quantity"),
                    paste0(prefix, "_value")
                ),
                Unit = ifelse(row_number() %% 2 == 0, "tonne", "USD"),
                Value = 0,
                Flag = "A"
            )

        # Append
        tm_df <- bind_rows(tm_df, expanded)
    }

    return(tm_df)
}

#' Gets complete trade matrix.
#'
#' This function takes the trade matrix and adds in the missing rows of
#' Countries who one or more Items are imported from but one or more Items are
#' not exported to, and vice-versa.
#'
#' @param tm_df A tibble.
#' @param countries A vector.
#' @param items A vector.
#'
#' @return The full trade matrix as a tibble, arranged by Country.
#'
get_full_tm <- function(tm_df, countries, items) {
    # Cartesian product of both
    combinations <- expand.grid("Country" = countries, "Item" = items) %>%
        as_tibble()

    # Sorting alphabetically
    tm_df <- add_missing_rows(tm_df, combinations) %>%
        arrange(Country)

    return(tm_df)
}

#' Gets the I/E rate and quantity matrices.
#'
#' This function takes the full trade matrix and calculates the exchange rate
#' cost in USD per tonne. The output is a list. The first two items are the
#' I/E rate matrices as tibbles, respectively. The last two items are the
#' I/E quantity matrices as tibbles, respectively.
#'
#' The row number can be mapped to the list of countries as they are sorted
#' alphabetically.
#'
#' @param tm_full_df A tibble.
#' @param countries A vector.
#' @param items A vector.
#'
#' @return A list containing the I/E rate matrices and quantities as tibbles.
#'
get_rate_and_qty_tm <- function(tm_full_df, countries, items) {
    # Import and export rate tibbles
    ir_df <- tibble(.rows = length(countries))
    er_df <- tibble(.rows = length(countries))

    # Import and export quantity tibbles
    iq_df <- tibble(.rows = length(countries))
    eq_df <- tibble(.rows = length(countries))

    for (item in seq_along(items)) {
        # Imports
        iv <- tm_full_df %>%
            filter(
                Item == items[item],
                Element == "import_value"
            ) %>%
            arrange(Country) %>%
            pull(Value)

        iq <- tm_full_df %>%
            filter(
                Item == items[item],
                Element == "import_quantity"
            ) %>%
            arrange(Country) %>%
            pull(Value)

        # Exports
        ev <- tm_full_df %>%
            filter(
                Item == items[item],
                Element == "export_value"
            ) %>%
            arrange(Country) %>%
            pull(Value)

        eq <- tm_full_df %>%
            filter(
                Item == items[item],
                Element == "export_quantity"
            ) %>%
            arrange(Country) %>%
            pull(Value)

        # Net Import / Export Revenue Rates
        ni <- (iv / iq) %>% replace(is.nan(.), 0) # Units: USD / tonne
        ne <- (ev / eq) %>% replace(is.nan(.), 0) # Units: USD / tonne

        ir_df <- ir_df %>% mutate(!!items[item] := ni)
        er_df <- er_df %>% mutate(!!items[item] := ne)

        iq_df <- iq_df %>% mutate(!!items[item] := iq) # Units: tonne
        eq_df <- eq_df %>% mutate(!!items[item] := eq) # Units: tonne
    }

    return(list(ir_df, er_df, iq_df, eq_df))
}

#' Either saves or returns the FAO production information from the QCL database
#' for the given parameters in a CSV file with the name "FAO Prod Filtered.csv"
#' in the Data folder.
#'
#' The country_code and item_codes information can be found on the FAOSTAT
#' website under the definitions and standards:
#' https://www.fao.org/faostat/en/#data/QCL
#'
#' @param country_codes A vector of character.
#' @param item_codes A vector of numeric.
#' @param years A vector of numeric.
#' @param save A logical. Defaults to TRUE.
#'
save_prod_df <- function(country_codes, item_codes, years, save=TRUE) {
    prod_df <- get_raw_prod_df()

    # To allow functionality for empty vectors.
    if (length(years) == 0) {
        years <- prod_df %>% pull(year_code)
    }

    if (length(country_codes) == 0) {
        country_codes <- prod_df %>% pull(country_codes)
    }

    if (length(item_codes) == 0) {
        item_codes <- prod_df %>% pull(item_code)
    }

    prod_df <- prod_df %>%
        filter(
            area_code %in% country_codes,
            year %in% years,
            item_code %in% item_codes
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

    if (!save) return(prod_df)

    write.table(
        prod_df,
        file = paste("./Data/FAO Prod Filtered.csv"),
        sep = ",",
        row.names = FALSE
    )
}

#' Either saves or returns the FAO trade matrix information from the TM
#' database for the given parameters in a CSV file with the name
#' "FAO TM Filtered.csv" in the Data folder.
#'
#' The country_code and item_codes information can be found on the FAOSTAT
#' website under the definitions and standards:
#' https://www.fao.org/faostat/en/#data/TM
#'
#' @param country_codes A vector of characters.
#' @param item_codes A vector of numeric.
#' @param year A vector of numeric.
#' @param save A logical. Defaults to TRUE.
#'
save_tm_df <- function(country_codes, item_codes, years, save=TRUE) {
    tm_df <- get_raw_tm_df()

    # To allow functionality for empty vectors.
    if (length(years) == 0) {
        years <- tm_df %>% pull(year_code)
    }

    if (length(country_codes) == 0) {
        country_codes <- tm_df %>% pull(reporter_country_code)
    }

    if (length(item_codes) == 0) {
        item_codes <- tm_df %>% pull(item_code)
    }

    tm_df <- tm_df %>%
        filter(
            reporter_country_code %in% country_codes,
            year %in% years,
            item_code %in% item_codes
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

    if (!save) return(tm_df)

    write.table(
        tm_df,
        file = "./Data/FAO TM Filtered.csv",
        sep = ",",
        row.names = FALSE
    )
}