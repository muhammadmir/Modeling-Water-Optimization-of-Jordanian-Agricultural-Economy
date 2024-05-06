#' An internal function for METAR Helpers.
#'
#' Computes the monthly aggregates.
#'
#' Note: Only computes aggregates for Temp, Min, Max, Dew, Humidity,
#' and Wind. Precip is summed.
#'
#' @param input_df A dataframe.
#'
#' @return A dataframe containing monthly aggregates.
#'
aggregate_monthly <- function(input_df) {
    output_df <- input_df %>%
        group_by(Month = format(Date, "%m"), Year = format(Date, "%Y")) %>%
        summarise(
            Temp = mean(Temp),
            Min = min(Temp),
            Max = max(Temp),
            Dew = mean(Dew),
            Humidity = mean(Humidity),
            Wind = mean(Wind),
            Precip = sum(Precip),
            .groups = "drop"
        )

    # Create the Date column correctly.
    output_df <- output_df %>%
        rowwise() %>%
        mutate(
            Date = as.Date(paste(c(Year, Month, "01"), collapse = "-")),
            .before = "Temp"
        ) %>%
        select(-c(Month, Year))

    # Append these columns to the front.
    output_df <- output_df %>%
        add_column(Station = input_df$Station[1], .before = "Date") %>%
        add_column(Latitude = input_df$Latitude[1], .before = "Date") %>%
        add_column(Longitude = input_df$Longitude[1], .before = "Date") %>%
        add_column(Altitude = input_df$Altitude[1], .before = "Date") %>%
        arrange(Date)

    return(output_df)
}

#' An internal function for METAR Helpers.
#'
#' Computes the daily aggregates.
#'
#' Note: Only computes aggregates for Temp, Min, Max, Dew, Humidity,
#' and Wind. Precip is summed.
#'
#' @param input_df A dataframe.
#'
#' @return A dataframe containing daily aggregates.
#'
aggregate_daily <- function(input_df) {
    output_df <- input_df %>%
        group_by(Date = as.Date(Date)) %>%
        summarise(
            Temp = mean(Temp),
            Min = min(Temp),
            Max = max(Temp),
            Dew = mean(Dew),
            Humidity = mean(Humidity),
            Wind = mean(Wind),
            Precip = sum(Precip)
        )

    # Append these columns to the front.
    output_df <- output_df %>%
        add_column(Station = input_df$Station[1], .before = "Date") %>%
        add_column(Latitude = input_df$Latitude[1], .before = "Date") %>%
        add_column(Longitude = input_df$Longitude[1], .before = "Date") %>%
        add_column(Altitude = input_df$Altitude[1], .before = "Date")

    return(output_df)
}

#' An internal function for METAR Helpers.
#'
#' Renames all the columns into something more understandable. Recodes
#' NA values as 0. Additionally, converts all units to standard units:
#'
#' Temp and Dew from °F to °C.
#' Wind from knots to meters per second (mps).
#' Precip from inches to milimeters (mm).
#'
#' @param input_df A dataframe.
#'
#' @return A dataframe containing correct columns and units.
#'
clean <- function(input_df) {
    output_df <- input_df %>% mutate(
        Station = station,
        Date = valid,
        Latitude = lat,
        Longitude = lon,
        Altitude = alti,
        Temp = fahrenheit.to.celsius(tmpf),
        Dew = fahrenheit.to.celsius(dwpf),
        Humidity = relh,
        Wind = knots_to_speed(sknt, unit = "mps"),
        Precip = inches_to_metric(p01i, unit = "mm")
    ) %>%
    select(
        Station, Date,
        Latitude, Longitude, Altitude,
        Temp, Dew, Humidity, Wind, Precip
    )

    output_df <- output_df %>% replace(is.na(.), 0)

    return(output_df)
}

#' Gets a dataframe containing the monthly aggregates of selected weather
#' parameters during a given period from all available stations for a given
#' METAR network.
#'
#' The dataframe returned contains the following columns: Station, Latitude,
#' Longitude, Altitude, Month, Year, Temp, Min, Max, Dew, Humidity, Wind, and
#' Precip.
#'
#' Note: Only Temp, Min, Max, Dew, Humidity, and Wind are calculated for
#' aggregates will Precip is summed.
#'
#' @param network_code A character.
#' @param start_date A character.
#' @param end_date A character.
#'
#' @return A dataframe containing the monthly aggregates.
#'
get_monthly_df <- function(network_code, start_date, end_date) {
    stations_df <- riem_stations(network = network_code)

    # Get data of all stations as a list of dataframes.
    grouped_df <- lapply(
        stations_df$id,
        riem_measures,
        date_start = start_date,
        date_end = end_date
    )

    clean_df <- lapply(grouped_df, clean)
    daily_df <- lapply(clean_df, aggregate_daily)
    monthly_df <- lapply(daily_df, aggregate_monthly)

    # Bind and arrange by Date.
    master_df <- do.call(rbind, monthly_df) %>%
        group_by(Station) %>%
        arrange(Date) %>%
        ungroup()

    return(master_df)
}

#' An internal function for METAR Helpers.
#'
#' Calculates the monthly ETo for the input dataframe.
#'
#' @param input_df A dataframe.
#'
#' @return A dataframe containing the ETo column.
#'
calculate_monthly_ETo <- function(input_df) {
    input_df <- input_df %>%
        mutate(
            ETo = penman(
                Tmin = Min,
                Tmax = Max,
                U2 = Wind,
                RH = Humidity,
                Tdew = Dew,
                tsun = Sunshine,
                lat = Latitude[1],
                z = Altitude[1],
                method = "FAO",
                na.rm = TRUE
            )
        ) %>%
        select(Station, Latitude, Longitude, Altitude, Date, ETo, Precip)

    return(input_df)
}

#' An internal function for METAR Helpers.
#'
#' Calculates the monthly Peff for the input dataframe.
#'
#' @param input_df A dataframe.
#'
#' @return A dataframe containing the Peff column.
#'
calculate_monthly_Peff <- function(input_df) {
    input_df <- input_df %>%
        rowwise() %>%
        mutate(
            Peff = ifelse(
                Precip > 250,
                125 + 0.1 * Precip,
                (Precip * (125 - 0.2 * Precip)) / 125
            )
        ) %>%
        select(-Precip)

    return(input_df)
}

#' Calculates the monthly ETo and effective precipitation (Peff).
#'
#' The following assumptions are made in order to calculate the ETo:
#' 1) There are at least 3 months (or rows) of data. This is because the soil
#' heat flux (G) is based on temperatures from other months.
#' 2) The daily average hours of sunshine is encoded in the Sunshine column.
#' 3) Assumption #3
#'
#' Additionally,The [X] method is used to calculate Peff.
#'
#' @param monthly_df A dataframe.
#'
#' @return A dataframe containing the ETo and Peff columns.
#'
calc_ETo_and_Peff <- function(monthly_df) {
    # Split by each station's attributes again for ETo calculation.
    split_df <- monthly_df %>%
        group_by(Station, Latitude, Longitude, Altitude) %>%
        group_split()

    split_df <- lapply(split_df, calculate_monthly_ETo)

    # Re-group into single dataframe as Peff calculation isn't
    # station-dependent.
    master_df <- do.call(rbind, split_df)

    master_df <- calculate_monthly_Peff(master_df)

    return(master_df)
}