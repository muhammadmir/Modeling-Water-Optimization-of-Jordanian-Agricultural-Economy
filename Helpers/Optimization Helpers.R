#' Creates an empty production quantity dataframe where each column name
#' is the item in items.
#'
#' The dataframe is 1xN, where N is the number of items. The entire dataframe
#' is filled with 0s.
#'
#' @param items A vector.
#'
#' @return An empty production quantity dataframe.
#'
create_blank_pq_df <- function(items) {
    df <- tibble(.rows = 1)

    for (item in seq_along(items)) {
        df <- df %>% mutate(!!items[item] := 0)
    }

    return(df)
}

#' Creates an empty I/E quantity dataframe where each column name is the item
#' in items.
#'
#' The dataframe is MxN, where M is the number of countries and N is the number
#' of items. The entire dataframe is filled with 0s.
#'
#' @param countries A vector.
#' @param items A vector.
#'
#' @return An empty I/E quantity dataframe.
#'
create_blank_ie_df <- function(countries, items) {
    df <- tibble(.rows = length(countries))
    for (item in seq_along(items)) {
        df <- df %>% mutate(
            !!items[item] := rep(0, length(countries))
        )
    }

    return(df)
}

#' This function unconverts the production, import, and export quantity
#' dataframes into a single vector. Effectively un-does the covnert function.
#'
#' The single vector is of length K, where K = P + I + E. P is the number
#' of columns in pq_df, I is the number of columns and rows in iq_df, and
#' eq_df is the number of columns and rows in eq_df.
#'
#' Note that the information is extracted and appended to the single vector
#' in rowwise fashion and from the following dataframes in the following
#' order: pq_df, iq_df, eq_df.
#'
#' @param pq_df A dataframe.
#' @param iq_df A dataframe.
#' @param eq_df A dataframe.
#'
#' @return A vector containing the rowwise-extracted values from pq_df, iq_df,
#' and eq_df.
#'
unconverter <- function(pq_df, iq_df, eq_df) {
    items <- colnames(pq_df)
    x <- c()

    # Extract data from the production quantity dataframe
    for (i in seq_along(items)) {
        x <- c(x, pq_df[[items[i]]])
    }

    # Extract data from the import quantity dataframe
    for (i in seq_along(items)) {
        x <- c(x, iq_df[[items[i]]])
    }

    # Extract data from export_quantity dataframe
    for (i in seq_along(items)) {
        x <- c(x, eq_df[[items[i]]])
    }

    return(x)
}

#' This function convert the single vector output from the unconverter function
#' back into the three dataframes with the column names being items.
#'
#' The single vector is of length K, where K = P + I + E. P is the number
#' of columns in pq_df, I is the number of columns and rows in iq_df, and
#' eq_df is the number of columns and rows in eq_df. The information is appended
#' rowwise in the following order: pq_df, iq_df, eq_df.
#'
#' The output is a dataframe with the columns being the following: Production,
#' Import, and Export. Each column contains the respective dataframes wrapped
#' as a list.
#'
#' @param items A vector.
#' @param x A vector.
#'
#' @return A dataframe.
#'
converter <- function(items, x) {
    pq_df <- create_blank_pq_df(items)
    for (i in seq_along(items)) pq_df[items[i]] <- x[i]

    # Initialize indicies
    start <- 1
    end <- length(items)

    # Import
    iq_df <- create_blank_ie_df(countries, items)
    for (i in seq_along(items)) {
        start <- ifelse(end == length(items), start + length(items), end + 1)
        end <- end + length(countries)
        iq_df <- iq_df %>% mutate(!!items[i] := x[start:end])
    }

    # Export
    eq_df <- create_blank_ie_df(countries, items)
    for (i in seq_along(items)) {
        start <- end + 1
        end <- end + length(countries)
        eq_df <- eq_df %>% mutate(!!items[i] := x[start:end])
    }

    dfs <- tibble(
        "Production" = list(pq_df),
        "Import" = list(iq_df),
        "Export" = list(eq_df)
    )

    return(dfs)
}
