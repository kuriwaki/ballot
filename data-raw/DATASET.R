library(devtools)
library(dplyr)

# devtools::install_version("noncensus", "0.1")
data("counties", package = "noncensus")

counties <- counties |>
  select(-population)

usethis::use_data(counties, overwrite = TRUE)
