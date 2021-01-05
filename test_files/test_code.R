library(cabanting)
devtools::document("cabanting")

df <- read.csv("./cabanting/test_files/test_dataset_2.csv")

df1 <- uis_filter_indicator(df,"FOSGP_5T8_F500")

ASEAN = c("PHL","THA",
         "BRN","VNM",
         "MMR","KHM",
         "IDN","LAO",
         "MYS","SGP")

plot_compare(df1,
            countries = c("MDV"),
            stat = "median",
            graph = "bar",
            axis=1,
            round_places = 3,
            top = -30,
            use_code_in_label = FALSE,
            color_palette = "OrRd"
            )

#?plot_compare
