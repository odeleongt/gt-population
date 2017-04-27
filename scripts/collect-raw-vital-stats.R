#------------------------------------------------------------------------------*
# Collect raw vital statistics at the department level
#------------------------------------------------------------------------------*

#------------------------------------------------------------------------------*
# Prepare environment ----
#------------------------------------------------------------------------------*

# Load used packages
library(package = "lubridate")
library(package = "haven")
library(package = "tidyverse")

# Set metadata
data_path <- "data/raw/vital-statistics/"




#------------------------------------------------------------------------------*
# Load births data ----
#------------------------------------------------------------------------------*

# Read labeled data
births <- list.files(path = data_path, pattern = "births") %>%
  paste0(data_path, .) %>%
  set_names(gsub("[^0-9]", "", .)) %>%
  map(read_spss) %>%
  map(~set_names(.x, tolower(iconv(names(.x), to = "ASCII//TRANSLIT"))))

# Read labels from imported data
birth_labels <- births %>%
  map(~map(.x, ~attr(.x, "labels")))

# Bind imported datasets
births <- births %>%
  map(~map_df(.x, zap_labels)) %>%
  map(~map_df(.x, as.character)) %>%
  bind_rows(.id = "file_year")

# Prepare birth "events" dataset
births <- births %>%
  mutate(
    record_date = ymd(
      paste(
        stringr::str_pad(anoreg, width = 2, side = "left", pad = "0"),
        mesreg, "01", sep = "-"
      )
    ),
    anoocu = ifelse(is.na(anoocu), file_year, anoocu),
    event_date = ymd(
      paste(
        stringr::str_pad(anoocu, width = 2, side = "left", pad = "0"),
        mesocu, diaocu, sep = "-"
      )
    )
  ) %>%
  select(
    # Record metadata
    record_date, record_department = depreg, record_municipality = mupreg,
    # Event data
    event_date, event_department = depocu, event_municipality = mupocu,
    # Mother residency location
    mother_department = deprem, mother_municipality = muprem
  )




# End of script
