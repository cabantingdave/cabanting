#' Filter UNESCO Institute of Statistics Dataset
#'
#' @description
#' `uis_filter_country` filters UIS Dataset using a single country code.
#'
#' @param df The `data.frame` to be filtered. Leave the UIS dataset as is for it should have the Indicator ID as its first column, regardless of its column name, and it should contain the following columns:
#' * `Indicator` : Indicator names;
#' * `LOCATION`: Three-letter ISO country codes;
#' * `Country` : Official country names;
#' * `TIME` : Year; and
#' * `Value` : Numerical values.
#' @param years The years to filter. It should be a numerical vector. Defaults to all available years in the dataset.
#' @param country_code The three-letter ISO code of the country to be filtered. It should be a character object with length 1.
#' @param indicator_ids The IDs of the indicator to be filtered as character vector. Defaults to all indicators in the asked  country.
#'
#' @examples
#' df <- read.csv("NATMON_DS_04012021100059822.csv")
#'
#' uis_filter_country(df,
#'                    "PHL")
#' uis_filter_country(df,
#'                    "PHL",
#'                     indicator_ids=c("FOSGP_5T8_F900","FOSGP_5T8_F300","FOSGP_5T8_FUK"))
#' uis_filter_countr(df,
#'                    "PHL",
#'                    years=seq(2010,2013),
#'                    indicator_ids=c("FOSGP_5T8_F900","FOSGP_5T8_F300","FOSGP_5T8_FUK"))
#' @name uis_filter_country
#' @rdname uis_filter
#' @md
#'
uis_filter_country <- function(df,country_code,years = NULL,indicator_ids = NULL){
  library(dplyr)
  colnames(df)[1] = "INDICATOR_ID"
  if (is.null(indicator_ids)) {
    datasubset <- df %>% filter(LOCATION == country_code)
    indicator_ids <- sort(unique(datasubset$INDICATOR_ID))
  } else {
    datasubset <- df %>% filter(LOCATION == country_code  & INDICATOR_ID %in% indicator_ids)
  }
  if (is.null(years)) {
    years <- sort(unique(datasubset$TIME))
  }
  duplicate_indicator_removed <- datasubset[!duplicated(datasubset$Indicator),]
  indicator_names <- duplicate_indicator_removed[match(indicator_ids,duplicate_indicator_removed$INDICATOR_ID),]$Indicator
  columnnames <- c("COUNTRY","INDICATOR")
  new_df <- setNames(data.frame(matrix(ncol=2,nrow=length(indicator_ids)),row.names=indicator_ids),columnnames)
  new_df$COUNTRY <-  datasubset$Country[1]
  new_df$INDICATOR <- indicator_names
  options(scipen = 999)
  for (year in years) {
    current_year_data <- datasubset %>% filter(TIME == year)
    rownames(current_year_data) <- current_year_data$INDICATOR_ID
    new_df <- cbind(new_df,year = as.numeric(current_year_data$Value[match(rownames(new_df), rownames(current_year_data))]))
  }
  colnames(new_df) <- c(columnnames,years)
  new_df
}
