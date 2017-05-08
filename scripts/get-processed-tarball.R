

library(package = "jsonlite")
library(package = "httr")
library(package = "tidyverse")

# Set request parameters
prerelease <- scan(file = "lastest-prerelease", what = "character")

releases <- GET(
  paste0(
    "https://api.github.com/repos/odeleongt/gt-population/releases/tags/",
    prerelease
  )
)

# Get asset uri
asset <- releases %>%
  content(as = "text") %>%
  fromJSON() %>%
  map(~ifelse(is.list(.x), as.data.frame(list(.x)), .x)) %>%
  flatten_df() %>%
  pull(assets) %>%
  GET %>%
  content(as = "text") %>%
  fromJSON() %>%
  getElement("browser_download_url")

# Download file
download.file(asset, "data/processed/gt_2000_2020_municipality_population.RData")

