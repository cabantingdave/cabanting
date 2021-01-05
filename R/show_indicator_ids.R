#' Show UIS Indicators
#'
#' @description
#' `show_indicator_ids` shows the indicators and their corresponding indicator IDs on the current dataframe
#'
#' @param df The `data.frame` to be filtered. Leave the UIS dataset as is
#' @name show_indicator_ids
#' @rdname show_indicator_ids
#' @md
#' @export


show_indicator_ids <- function(df){
  df <- df[match(unique(df[,1]),df[,1]),c(1,2)]
  colnames(df) <- c("INDICATOR_ID","INDICATOR")
  indicators <- array(data.frame(setNames(df[,2],df[,1])))
  indicators
  
}
