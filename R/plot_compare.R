#' Plot Comparison of Nations in UIS Dataset
#'
#' @description
#' `plot_compare` plots a ggplot comparing the statistics of
#' nations with the dataset made using the function `uis_filter_indicator`
#'
#' @param df Dataframe.
#' The `data.frame` to be filtered.
#' This should be created using the `uis_filter_indicator` function.
#'
#' @param countries Character Vector.
#' The three-letter codes of countries to compare.
#' The function will highlight and label these countries.
#'
#' @param top Numerical vector or numerical variable.
#' This will filter the top countries.
#' Negative numbers will filter the bottom countries.
#' Defaults to all countries.
#' See examples for more details.
#'
#' @param stat `'mean'`, `'median'`, `'sd'`, `'min'`, and `'max'`.
#' Since the output of `uis_filter_indicator` contains multiple years,
#' this is the statistic that will be calculated
#' per country for all the available years.
#'
#' @param graph `'bar'` or `'segment'`.
#' Type of graph to be plotted. Defaults to `'bar'`.
#'
#'
#' @param include_world_stat Logical. Defaults to `TRUE`.
#' If `FALSE`, it will exclude the world statistic to be highlighted and labeled.
#'
#' @param world_stat `'mean'` or `'median'`.
#' The world statistics to calculate. Defaults to `'median'`.
#' Will be ignored if `include_world_stat` is `FALSE`.
#'
#' @param remove_regions Logical.
#' This will remove the regional summaries in the UIS dataset.
#' Defaults to `TRUE`.
#'
#' @param desc Logical. Sorts the plot into descending order. Defaults to `TRUE`.
#'
#' @param axis `0` or `1`. Orientation of plot.
#'
#' @param round_places Integer. The number of places to round in calculations.
#' Defaults to 0.
#'
#' @param labs Logical. Determines whether to show the labels in the plot.
#' Determines
#'
#' @param use_code_in_axis Determines whether to show the
#' code on the axis instead of the whole name of the country. Defaults to TRUE.
#'
#' @param title Character. The title to show on top.
#'
#' @param use_code_in_label Determines whether to show the
#' code on the labels instead of the whole name of the country. Defaults to TRUE.
#'
#' @param color_palette RColorBrewer palettes. EXPERIMENTAL.
#' Sets the color of the highlighted bars/segments.
#'
#' @examples
#' df_source <- read.csv("NATMON_DS_04012021100059822.csv")
#' df <- uis_filter_indicator(df_source,"FOSGP_5T8_F900")
#'
#' plot_compare(df)
#' plot_compare(df1,
#'              countries = c("PHL"),
#'              round_places = 3) +
#'    theme(axis.text.y = element_blank())
#'
#' plot_compare(df,
#'            countries = c("PHL","MMR","ITA"),
#'            stat = "median",
#'            graph = "segment",
#'            top = 30,
#'            axis=1,
#'            round_places = 3)
#'
#'
#' #' plot_compare(df,
#'            countries = c("PHL","MMR","ITA"),
#'            stat = "median",
#'            world_stat = "mean",
#'            top = seq(20,40),
#'            axis=1,
#'            round_places = 3)
#'
#'
#' plot_compare(df,
#'            countries = c("PHL","MMR","ITA"),
#'            stat = "median",
#'            graph = "segment",
#'            axis=1,
#'            round_places = 3,
#'            top = -20
#'            include_world_stat = FALSE,
#'            use_code_in_label = FALSE,
#'            color_palette = "BuGn"
#'            )
#'
#'
#' @name plot_compare
#' @seealso
#' @md
#'
#'
plot_compare <- function(df,
                         countries=NULL,
                         top=0,
                         stat="mean",
                         graph="bar",
                         include_world_stat = TRUE,
                         world_stat = "median",
                         remove_regions = TRUE,
                         desc=TRUE,
                         axis=0,
                         round_places = 0,
                         labs = TRUE,
                         use_code_in_axis = TRUE,
                         title = NULL,
                         use_code_in_label = TRUE,
                         color_palette=NULL){
  require(dplyr)
  require(tidyverse)
  require(ggplot2)
  require(cabanting)
  require(ggrepel)

  ### PRESERVE THE ORIGINAL TITLE
  orig_df <- df

  ### SET TITLE
  if (is.null(title)) {
    title <- df$INDICATOR[1]
  }

  #### PRESERVE COUNTRIES
  if (!(is.null(countries))) {
    countries_df <- df %>% filter(rownames(df) %in% countries)
    if (include_world_stat == TRUE) {
      countries_df <- add_world_stat(countries_df,
                                     orig_df=orig_df,
                                     world_stat = world_stat)
      countries <- c(countries,"World")
    }
    countries_df <- make_stat_column(countries_df,stat)
    countries_df <- add_labels(countries_df,
                               round_places = round_places,
                               use_code_in_label=use_code_in_label)
  }

  #### REMOVE REGIONAL SUMMARIES
  if (remove_regions == TRUE) {
    df <- remove_regions_func(df)
  }

  #### ADD WORLD IF COMPARED AGAINST WORLD
  if (include_world_stat == TRUE) {
    df <- add_world_stat(df,orig_df=orig_df,world_stat = world_stat)
  }

  #### Solve for the stat (default = "mean")
  #print(names(df))
  df <- make_stat_column(df,stat)

  #### sort and add labels
  df <- sort_stat(df,desc=desc,top=top,countries=countries)
  df <- add_labels(df,round_places = round_places,
                   use_code_in_label=use_code_in_label)
  #print(df)

  #### BIND COUNTRIES AND DF
  if (!(is.null(countries))) {
    #print(countries_df)
    duplicaterows <- rownames(countries_df) %in% rownames(df)
    df <- rbind(df,countries_df[!duplicaterows,])
    df <- sort_stat(df,desc=desc,top=0)
    df[!(rownames(df) %in% countries),"LABEL"] <- ""
    #print(df)
  }

 #print(df["World",])

  #print(max(df$STAT)*1.3)

  ### PLOT MAIN
  g <- ggplot(df,aes(x=COUNTRY,y=STAT))
  if (graph=="bar") {
    g <- g + geom_bar(stat="identity",width=0.85)
  } else if (graph=="segment"){
    g <- g + geom_segment(aes(x=COUNTRY,xend=COUNTRY,y=0,yend=STAT)) +
      geom_point(size=0.85)
  }

  ## HIGHLIGHT COUNTRIES
  if (!(is.null(countries))) {
    if (graph == "segment") {
      g <- g + geom_segment(data = countries_df,
                            aes(x=COUNTRY,xend=COUNTRY,y=0,yend=STAT,color=STAT),
                            size=1) +
        geom_point(data = countries_df,
                   aes(color=STAT,fill=STAT),
                   fill=countries_df$STAT,
                   color=countries_df$STAT,
                   size=1)
      if (labs==TRUE) {
        g <- g + geom_label_repel(data = countries_df,
                                  aes(x=COUNTRY,y=STAT,label = LABEL),
                                  nudge_y=max(df$STAT*0.1),
                                  size=3)
      }
    } else if  (graph == "bar"){
      g <- g + geom_bar(data = countries_df,stat="identity",aes(fill=STAT))
      if (labs == TRUE) {
        g <- g + geom_label_repel(data = countries_df,
                                  aes(x=COUNTRY,y=STAT,label = LABEL),
                                  nudge_y=max(df$STAT*0.1),size=3)
      }
    }
  }

  if (is.null(color_palette)) {
    g <- g +  scale_color_gradient(low="orange",high="orange") +
      scale_fill_gradient(low="orange",high="orange")
  } else {
    g <- g + scale_color_fermenter(palette=color_palette) +
      scale_fill_fermenter(palette=color_palette)
  }

  ### THEMES
  g <- g +
    theme_classic() +
    ggtitle(title) +
    theme(
      plot.title = element_text(hjust=0.5)
    )

  ## REPLACE X LABELS
  if (use_code_in_axis == TRUE) {
      g <- g +
      scale_x_discrete(labels = rownames(df))
    }

  ## COORDINATE FLIP
  if (axis==0) {
    g <- g +
      coord_flip(expand = TRUE) +
      scale_y_continuous(limits = c(0, max(df$STAT)*1.2))
    g <- g +
      theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  } else if (axis==1) {
    g <- g +
      scale_y_continuous(limits = c(0, max(df$STAT)*1.2)) +
      theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }


  g
}
