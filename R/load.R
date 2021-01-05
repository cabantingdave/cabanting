library(tidyverse)
library(ggplot2)
library(ggrepel)
require(dplyr)

remove_regions_func <- function(df) {
  df <- df %>% filter(is.na(as.numeric(row.names(df))))
  df
}

replace_short_names <- function() {

}

add_world_stat <- function(df,orig_df=orig_df,world_stat = world_stat){
  temp_df <- remove_regions_func(orig_df)
  #print(temp_df)
  if (world_stat == "mean") {
    if (dim(temp_df)[2] == 3) {
      world_mean <- as.numeric(mean(temp_df[,3],na.rm=TRUE))
    } else {
      world_mean <- as.numeric(apply(temp_df[,seq(3,dim(temp_df)[2])],2,mean,na.rm=TRUE))
    }
    new_row <- c("World Mean",orig_df$INDICATOR[1],world_mean)
    #print("mean")
  } else if (world_stat == "median") {
    if (dim(temp_df)[2] == 3) {
      world_median <- as.numeric(median(temp_df[,3],na.rm=TRUE))
    } else {
      world_median <- as.numeric(apply(temp_df[,seq(3,dim(temp_df)[2])],2,median,na.rm=TRUE))
    }
    new_row <- c("World Median",orig_df$INDICATOR[1],world_median)

    #new_row <- c("World Median",orig_df$INDICATOR[1],as.numeric(apply(temp_df[,seq(3,dim(temp_df)[2])],2,median,na.rm=TRUE)))
  }
  df["World",] = new_row
  df[,seq(3,dim(df)[2])] <- sapply(df[,seq(3,dim(df)[2])],as.numeric)
  df
}

make_stat_column <- function(df,stat) {
  if (dim(df)[2] == 3) {
      df$STAT = df[,3]
  } else {
      if (stat == "mean") {
        df$MEAN <- apply(df[,seq(3,dim(df)[2])],1,mean,na.rm=TRUE)
      } else if (stat == "max") {
        df$MAX <- apply(df[,seq(3,dim(df)[2])],1,max,na.rm=TRUE)
        df[sapply(df, is.infinite)] <- 0
      } else if (stat == "min") {
        df$MIN <- apply(df[,seq(3,dim(df)[2])],1,min,na.rm=TRUE)
        df[sapply(df, is.infinite)] <- 0
      } else if (stat == "median") {
        df$MEDIAN <- apply(df[,seq(3,dim(df)[2])],1,median,na.rm=TRUE)
      } else if (stat == "sd") {
        df$SD <- apply(df[,seq(3,dim(df)[2])],1,sd,na.rm=TRUE)
      } else {
        warning("the value for stat is unknown")
      }
    colnames(df)[dim(df)[2]] <- "STAT"
    }
  df
}

sort_stat <- function(df,desc,top,countries=NULL) {
  df <- df %>% arrange(STAT) %>% drop_na(STAT)
  #print(df)

  if (length(top) == 1) {
    if (is.null(top)) {
      df <- df %>% filter(rownames(df) %in% countries)
      #print(df)
    } else {
      if (top!=0) {
        if (top > 0) {
          df <- df[seq(dim(df)[1],dim(df)[1]-top),]
        } else if (top < 0) {
          df <- df[seq(-1*top,1),]
        }
      }
    }
  } else {
    df <- df[dim(df)[1]-top+1,]
  }

  df <- df %>% arrange(STAT)

  if (desc==FALSE) {
    df <- df %>% arrange(desc(row_number()))
  }
  ### MAKE SURE THE ORDER REMAINS
  df$COUNTRY <- factor(df$COUNTRY,levels=df$COUNTRY)
  df
}

add_labels <- function(df,round_places=round_places,use_code_in_label=use_code_in_label) {
  if (use_code_in_label == FALSE){
    #print("error")
    df$LABEL <- paste(df$COUNTRY," (",parse_guess(format(round(as.numeric(df$STAT),round_places),big.mark = ",")),")",sep="")
    #print("error 2")
  } else {
    df$LABEL <- paste(rownames(df)," (",parse_guess(format(round(as.numeric(df$STAT),round_places),big.mark = ",")),")",sep="")
  }
  df
}
