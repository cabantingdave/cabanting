#' Filter UNESCO Institute of Statistics Dataset
#'
#' @description
#' `uis_filter_indicator` filters UIS dataset using a single Indicator ID.
#'
#' This dataset must be downloaded from the
#' \href{http://data.uis.unesco.org/}{UNESCO Institute of Statistics (UIS) Data Browser}
#'  as a `.csv` file in default (,) format.
#'
#' @param df The `data.frame` to be filtered. Leave the UIS dataset as is for it should have the Indicator ID as its first column, regardless of its column name, and it should contain the following columns:
#' * `Indicator` : Indicator names;
#' * `LOCATION`: Three-letter ISO country codes;
#' * `Country` : Official country names;
#' * `TIME` : Year; and
#' * `Value` : Numerical values.
#' @param years The years to filter. It should be a numerical vector. Defaults to all available years in the dataset.
#' @param indicator_id The ID of the indicator to be filtered.
#' Try `df[match(unique(df[,1]),df[,1]),c(1,2)]` to know
#' more about the indicator IDs.
#' @param countries The countries to be filtered. It should be a character vector in the country code format.
#' Defaults to all countries (including the UIS Regional summaries, if available).
#'
#' @examples
#'
#' uis_filter_indicator(df,
#'                      "FOSGP_5T8_F500")
#' uis_filter_indicator(df,
#'                      "FOSGP_5T8_F500",
#'                       countries=c("PHL","USA","CHN"))
#' uis_filter_indicator(df,
#'                       "FOSGP_5T8_F500",
#'                        years=seq(2010,2013),
#'                        countries=c("PHL","USA","CHN"))
#'
#' @name uis_filter_indicator
#' @rdname uis_filter
#' @md

uis_filter_indicator <- function(df,indicator_id,years = NULL,countries = NULL){
  library(dplyr)
  colnames(df)[1] = "INDICATOR_ID"
  if (is.null(countries)) {
    datasubset <- df %>% filter(INDICATOR_ID == indicator_id)
    countries <- sort(unique(datasubset$LOCATION))
  } else {
    datasubset <- df %>% filter(INDICATOR_ID == indicator_id  & LOCATION %in% countries)
  }
  if (is.null(years)) {
    years <- sort(unique(datasubset$TIME))
  }
  duplicate_country_removed <- datasubset[!duplicated(datasubset$LOCATION),]
  countries_names <- duplicate_country_removed[match(countries,duplicate_country_removed$LOCATION),]$Country
  columnnames <- c("COUNTRY","INDICATOR")
  new_df <- setNames(data.frame(matrix(ncol=2,nrow=length(countries)),row.names=countries),columnnames)
  new_df$COUNTRY <- countries_names
  new_df$INDICATOR <- datasubset$Indicator[1]

  options(scipen = 999)
  for (year in years) {
    current_year_data <- datasubset %>% filter(TIME == year)
    rownames(current_year_data) <- current_year_data$LOCATION
    new_df <- cbind(new_df,year = as.numeric(current_year_data$Value[match(rownames(new_df), rownames(current_year_data))]))
  }
  colnames(new_df) <- c(columnnames,years)
  new_df
}
