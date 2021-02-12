library("dplyr")

countries <-
  c("2" = "US",
    "20" = "Canada",
    "200" = "UK",
    "210" = "Netherlands",
    "211" = "Belgium",
    "220" = "France",
    "260" = "Germany",
    "305" = "Austria",
    "325" = "Italy",
    "375" = "Finland",
    "380" = "Sweden",
    "385" = "Norway ",
    "390" = "Denmark",
    "740" = "Japan")

garrett1998 <- read.delim("garrett1998.tab", stringsAsFactors = FALSE) %>%
  select(- matches("per"), - matches("Icc_"), - clint) %>%
  mutate(countryname = countries[as.character(country)])

write.csv(garrett1998, file = "garrett1998.csv",
          na = "", row.names = FALSE)
