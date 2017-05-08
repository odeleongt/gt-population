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
# Define age groups ----
#------------------------------------------------------------------------------*

# Order age groups for output
age_groups <- c(
  "0-27 days", "28 days-<3 month", "3-5 months", "6-8 months", "9-11 months",
  "0-11 months",
  "12-23 months", "24-35 months", "36-59 months",
  "0-59 months", 
  "12-59 months", "24-59 months"
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
  data_frame(mid_year = ., event_year = NA) %>%
  group_by(mid_year) %>%
  do({
    data_frame(
      event_year = seq(min(year(births$event_date)), year(.$mid_year))
    )
  }) %>%
  ungroup()

# Calculate mid year ages given birth date
mid_year_ages <- mid_years %>%
  # Relevant births for each year
  left_join(unique(select(births, event_year, event_date))) %>%
  filter(event_date < mid_year)


labeled_ages <- mid_year_ages %>%
  # Tag all births for both age group types
  mutate(
    group = "exclusive",
    correlative = 1,
    label = "0-27 days",
    date_threshold = mid_year - days(27)
  ) %>%
  bind_rows(
    mutate(
      .,
      group = "exclusive",
      correlative = 2,
      label = "28 days-<3 month",
      date_threshold = mid_year - months(3)
    ),
    mutate(
      .,
      group = "exclusive",
      correlative = 3,
      label = "3-5 months",
      date_threshold = mid_year - months(6)
    ),
    mutate(
      .,
      group = "exclusive",
      correlative = 4,
      label = "6-8 months",
      date_threshold = mid_year -  months(9)
    ),
    mutate(
      .,
      group = "exclusive",
      correlative = 5,
      label = "9-11 months",
      date_threshold = mid_year -  months(12)
    ),
    mutate(
      .,
      group = "exclusive",
      correlative = 6,
      label = "12-23 months",
      date_threshold = mid_year -  months(24)
    ),
    mutate(
      .,
      group = "exclusive",
      correlative = 7,
      label = "24-35 months",
      date_threshold = mid_year -  months(36)
    ),
    mutate(
      .,
      group = "exclusive",
      correlative = 8,
      label = "36-59 months",
      date_threshold = mid_year -  months(60)
    )
  ) %>% 
  # Assign possible age groups
  mutate(
    keep = event_date >= date_threshold
  ) %>%
  filter(keep) %>%
  select(-keep, -date_threshold) %>%
  # Pick oldest applicable age group
  arrange(mid_year, event_date, correlative) %>%
  group_by(mid_year, event_date) %>%
  filter(correlative == min(correlative)) %>%
  ungroup %>%
  select(mid_year, event_date, label)


# Label other cummulative age groups
labeled_ages2 <- mid_year_ages %>%
  select(mid_year, event_date) %>%
  mutate(
    months_12 = mid_year - months(12),
    months_24 = mid_year - months(25) + days(1),
    months_59 = mid_year - months(60),
    # born 12-59 months before midyear
    "12-59 months" = event_date > months_59 & event_date <= months_12,
    # born 24-59 months before midyear
    "24-59 months" = event_date > months_59 & event_date <= months_24,
    # Any age before 12 months
    "0-11 months" = event_date > months_12 & event_date < mid_year,
    # Any age before 60 months
    "0-59 months" = event_date > months_59 & event_date < mid_year
  ) %>%
  select(-months_12, -months_24, -months_59) %>%
  gather(key = label, value = keep, -mid_year, -event_date) %>%
  filter(keep) %>%
  select(-keep)


# Bind labeled birth dates
birth_age_groups <- labeled_ages %>%
  bind_rows(labeled_ages2)


# Count live people by age group
alive <- births %>%
  # Births by date for each location
  count(
    year = event_year,
    department = event_department, municipality = event_municipality,
    event_date
  ) %>%
  # Label with ages at each mid-year
  left_join(birth_age_groups) %>%
  # Only keep births inside the mid year pediods
  filter(!is.na(label)) %>%
  # Children alive by age group at each mid-year
  count(year = year(mid_year), department, municipality, age_group = label) %>%
  rename(alive = nn) %>%
  mutate(
    age_group = factor(age_group, levels = age_groups, ordered = TRUE)
  ) %>%
  arrange(department, municipality, year, age_group)




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
