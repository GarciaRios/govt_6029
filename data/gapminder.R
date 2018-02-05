#' Gapminder Data
#'
#' Data from the gapminder package downloaded to a csv file
#'
library("gapminder")
data("gapminder", package = "gapminder")
write.csv(gapminder, file = "gapminder.csv", row.names = FALSE)
