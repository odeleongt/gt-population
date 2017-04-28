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
    ),
    event_year = year(event_date)
  ) %>%
  select(
    # Event data
    event_year, event_date, event_department = depocu, event_municipality = mupocu,
    # Record metadata
    record_date, record_department = depreg, record_municipality = mupreg,
    # Mother residency location
    mother_department = deprem, mother_municipality = muprem
  )




#------------------------------------------------------------------------------*
# Calculate mid-year counts ----
#------------------------------------------------------------------------------*

# Mid year dates
mid_years <- births %>%
  pull(event_date) %>%
  year() %>%
  unique() %>%
  paste0("-07-01") %>%
  ymd() %>%
  data_frame(mid_year = .) %>%
  group_by(mid_year) %>%
  do({
    data_frame(
      event_year = seq(min(year(births$event_date)), year(.$mid_year))
    )
  }) %>%
  ungroup()

# Calculate mid year counts
mid_year_counts <- mid_years %>%
  # Relevant births for each year
  left_join(births) %>%
  filter(event_date < mid_year) %>%
  # Calculate age at mid year
  mutate(
    age_days = as.integer(mid_year - event_date)
  )




#------------------------------------------------------------------------------*
# Load deaths data ----
#------------------------------------------------------------------------------*

# Read labeled data
deaths <- list.files(path = data_path, pattern = "deaths") %>%
  paste0(data_path, .) %>%
  set_names(gsub("[^0-9]", "", .)) %>%
  map(read_spss) %>%
  map(~set_names(.x, tolower(iconv(names(.x), to = "ASCII//TRANSLIT"))))

# Read labels from imported data
death_labels <- deaths %>%
  map(~map(.x, ~attr(.x, "labels")))

# Bind imported datasets
deaths <- deaths %>%
  map(~map_df(.x, zap_labels)) %>%
  map(~map_df(.x, as.character)) %>%
  bind_rows(.id = "file_year")

# Prepare birth "events" dataset
deaths <- deaths %>%
  # Ignore unknown ages
  filter(
    as.integer(edadif) < 999,
    perdif != "9"
  ) %>%
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
    ),
    event_year = year(event_date),
    age_unit = recode(
      perdif,
      "1" = "days",
      "2" = "months",
      "3" = "years",
      .default = NA_character_,
      .missing = NA_character_
    ),
    birth_date = event_date - period(edadif, units = age_unit),
    age_days = as.integer(event_date - birth_date)
  ) %>%
  select(
    # Event data
    event_year, event_date, event_department = depocu, event_municipality = mupocu,
    # Record metadata
    record_date, record_department = depreg, record_municipality = mupreg,
    # Deceased data
    birth_date, age_days
  )




# End of script
