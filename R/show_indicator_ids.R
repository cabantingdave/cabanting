show_indicator_ids <- function(df){
  df <- df[match(unique(df[,1]),df[,1]),c(1,2)]
  colnames(df) <- c("INDICATOR_ID","INDICATOR")
  indicators <- setNames(df[,2],df[,1])
  indicators
}
