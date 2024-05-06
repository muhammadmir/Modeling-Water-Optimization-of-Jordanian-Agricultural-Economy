#' An internal function utilized by calculate_Kc_curve.
#'
#' This function updates the Kc values on the Kc curve dataframe.
#'
#' @param Kc_curve A dataframe with the first column being the day and the
#' second column containing the Kc value at that day.
#' @param days A vector containing the days whose Kc value needs to be
#' updated.
#' @param Kc A vector containing the Kc values that need to be updated.
#'
#' @return The updated version of the Kc_curve.
#'
update_Kc_curve <- function(Kc_curve, days, Kc) {
    Kc_curve$Kc[match(days, Kc_curve$Day)] <- Kc
    return(Kc_curve)
}

#' Calculate the expanded Kc curve.
#'
#' Typical Kc curves have four stages of growth: Initial Period, Crop Dev.
#' Period, Middle Season Period, and Late Season Period. The following three Kc
#' values characterize the Kc values across the entire stages of growth: Kc
#' Initial, Kc Mid, and Kc End. Additionally, the planting date needs to be
#' provided to calculate the curve.
#'
#' This function uses the planting date, length of the growth stages, and their
#' corresponding Kc values to calculate the Kc curve, as described the FAO
#' methodology.
#'
#' Additionally, this curve is expanded to include all the number of days
#' in a year and their corresponding Kc values, which are trivially 0. This
#' expanded Kc curve allows a more accurate calculation of the monthly average
#' Kc value, which does not assume every month has 30 days like the FAO does.
#'
#' @param crop A dataframe containing the crop growing stages, Kc values,
#' plant date.
#'
#' @return A dataframe with the first column being the day and the second
#' column containing the Kc value at that day.
#'
calculate_Kc_curve <- function(crop) {
    # Unpacking
    growing_stage <- crop$growing_stage[[1]]
    Kc_value <- crop$Kc_value[[1]]

    # Date when crop cycle is fully completed.
    crop_end_date <- crop$plant_date + sum(growing_stage)

    # Used to determine number of calendar years.
    # Ex: If crop planted on 2010-10-01 for a total of 180 days, loop count
    # will be 2 as crop is still planted in 2011. Most of the time will be 1.
    loop_count <- year(crop_end_date) - year(crop$plant_date) + 1

    days_each_year <- rep(NA, loop_count)

    # Determine number of days for each year. Assume 365 days in 1 year.
    for (i in seq_len(loop_count)) {
        current_date <- crop$plant_date + years(i - 1)
        days_each_year[i] <- 365 + leap_year(current_date)
    }

    divisor <- sum(days_each_year) + 1

    Kc_curve <- tibble(
        Day = seq(sum(days_each_year)),
        Kc = rep(NA, sum(days_each_year))
    )

    # Init
    new_days <- (
        seq(yday(crop$plant_date),
        length.out = growing_stage$init) - 1
    ) %% divisor
    Kc <- Kc_value$init

    Kc_curve <- update_Kc_curve(Kc_curve, new_days, Kc)

    # Dev
    previous_day <- tail(new_days, n = 1)
    previous_Kc <- Kc_curve[previous_day, ]$Kc
    new_days <- (
        seq(previous_day + 1,
        length.out = growing_stage$dev) - 1
    ) %% divisor
    Kc <- approx(
        x = c(previous_day + 1, tail(new_days, n = 1)),
        y = c(previous_Kc, Kc_value$mid),
        n = length(new_days)
    )$y

    Kc_curve <- update_Kc_curve(Kc_curve, new_days, Kc)

    # Mid
    previous_day <- tail(new_days, n = 1)
    previous_Kc <- Kc_curve[previous_day, ]$Kc
    new_days <- (
        seq(previous_day + 1,
        length.out = growing_stage$mid) - 1
    ) %% divisor
    Kc <- Kc_value$mid

    Kc_curve <- update_Kc_curve(Kc_curve, new_days, Kc)

    # Late
    previous_day <- tail(new_days, n = 1)
    previous_Kc <- Kc_curve[previous_day, ]$Kc
    new_days <- (
        seq(previous_day + 1,
        length.out = growing_stage$late) - 1
    ) %% divisor
    Kc <- approx(
        x = c(previous_day + 1, tail(new_days, n = 1)),
        y = c(previous_Kc, Kc_value$end),
        n = length(new_days)
    )$y

    Kc_curve <- update_Kc_curve(Kc_curve, new_days, Kc)

    # Replace all NA with 0
    Kc_curve$Kc[is.na(Kc_curve$Kc)] <- 0

    return(Kc_curve)
}

#' Calculate average monthly Kc value for a crop.
#'
#' This function calculated the monthly average Kc values for a crop. For
#' example, if a crop is planted in 2019-01-01 and has a total growth period of
#' 180 days (meaning it stops growing the same year it was planted), then the
#' monthly average Kc value will be calculated for the months of 2019.
#'
#' However, if a crop is planted in 2019-10-01 and has a total growth period of
#' 130 days (meaning its stops growing in 2020), then the monthly average Kc
#' value will be calculated for all the months of 2019 and 2020.
#'
#' Two things should be noted:
#' 1) When using these values to calculate ETc, the number of months should be
#' equal. In the first example above, monthly ETo values from 2019-01-01 to
#' 2019-12-31 should be calcuted. For the second example, montly ETo values from
#' 2019-01-01 to 2020-12-31 should be calculated.
#'
#' 2) Traditional FAO methodolgoy suggests the assumption of 30 day averages for
#' for every month. However, this function accounts for the actual number of
#' days for every month.
#'
#' @param Kc_curve A dataframe with the first column being the day and the
#' second column containing the Kc value at that day.
#' @param crop A dataframe containing the crop growing stages, Kc values,
#' plant date.
#'
#' @return A vector of monthly average Kc values of length
#'
mean_monthly_Kc <- function(Kc_curve, crop) {
    year_start <- year(crop$plant_date)
    year_end <- year(crop$plant_date + sum(crop$growing_stage[[1]]))

    months <- seq(
        ymd(paste0(year_start, "-01-01")),
        ymd(paste0(year_end, "-12-31")),
        by = "1 month"
    )

    days_per_month <- days_in_month(months)

    # Goal is to increment by 1 day across by row and leave NA everywhere
    # where the corresponding row translated to a month (i.e. 3 -> March)
    # doesn't have an actual calendar day. 
    month_matrix <- matrix(nrow = 12 * (year_end - year_start + 1), ncol = 31)

    # Define matrix rows
    for (m in seq(days_per_month)) {
        # Starting day of month
        start <- ifelse(m == 1, 0, tail(na.omit(month_matrix[m - 1, ]), n = 1))

        days <- seq(start + 1, start + days_per_month[m])

        # Matrix wants # of entries per row to be the same (31)
        while (length(days) - 31 < 0) days <- c(days, NA)

        month_matrix[m, ] <- days
    }

    monthly_mean_Kc_values <- rep(0, nrow(month_matrix))

    for (row in seq_len(nrow(month_matrix))) {
        Kc_values_of_month <- Kc_curve %>%
            filter(Day %in% month_matrix[row, ]) %>%
            pull(Kc)

        monthly_mean_Kc_values[row] <- mean(Kc_values_of_month)
    }

    return(monthly_mean_Kc_values)
}
