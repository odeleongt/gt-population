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
# Load locations data ----
#------------------------------------------------------------------------------*

# Get municipality codes
municipalities <- sf::read_sf("data/raw/geo-data/municipalities.shp") %>%
  select(muni_id = COD_MUNI, municipality = MUNICIPIO) %>%
  mutate(
    # Fix encoding and case
    municipality = iconv(municipality, from = "Latin1", to = "ASCII//TRANSLIT"),
    municipality = gsub("(^| )([a-z])", "\\1\\U\\2", municipality, perl = TRUE),
    # Fix errors in names
    municipality = recode(
      muni_id,
      "0413" = "San Andres Itzapa",
      "2009" = "Quezaltepeque",
      "0204" = "San Cristobal Acasaguastlan",
      "0502" = "Santa Lucia Cotzumalguapa",
      "0506" = "Tiquizate",
      "0117" = "San Miguel Petapa",
      "0111" = "San Raimundo",
      "1314" = "San Rafael La Independencia",
      "1320" = "San Sebastian Huehuetenango",
      "1326" = "Santa Cruz Barillas",
      "2107" = "Mataquescuintla",
      "2217" = "Quezada",
      "1420" = "Ixcan",
      "1105" = "San Felipe",
      "0712" = "San Antonio Palopo",
      "0711" = "Santa Catarina Palopo",
      "1004" = "San Bernardino",
      "1009" = "San Pablo Jocopilas",
      "1011" = "San Miguel Panan",
      "0806" = "Santa Maria Chiquimula",
      .default = municipality
    )
  )




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
# Mid year counts for ages < 1 year used to estimate the proportion of children
# in each age group, which will be used to estimate the population in each
# age group from the official population projections.
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
    date_threshold = mid_year - days(28)
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


# Count live people by age group
local_births <- births %>%
  # Births by date for each location
  count(
    year = event_year,
    department = mother_department, municipality = mother_municipality,
    event_date
  ) %>%
  # Label with ages at each mid-year
  left_join(labeled_ages) %>%
  filter(
    # Only keep births inside the mid year pediods
    !is.na(label),
    # Ignore first half of 2009 for every succesive year
    (mid_year > ymd("2009-07-01") & event_date >= ymd("2009-07-01")),
    # Ignore first year, which can not be completed
    mid_year > ymd("2009-07-01")
  ) %>%
  # Children born by age group at each mid-year
  count(year = year(mid_year), department, municipality, age_group = label) %>%
  rename(births = nn) %>%
  mutate(
    age_group = factor(age_group, levels = age_groups, ordered = TRUE)
  ) %>%
  arrange(department, municipality, year, age_group) %>%
  # Keep only used departments
  filter(department %in% c("6", "9")) %>%
  mutate(
    # Fix municipality codes
    municipality = ifelse(
      test = as.integer(municipality) < 99,
      yes = paste0(
        stringr::str_pad(department, width = 2, side = "left", pad = "0"),
        stringr::str_pad(municipality, width = 2, side = "left", pad = "0")
      ),
      no = municipality
    ),
    # Label departments
    department = recode(
      department,
      "6" = "Santa Rosa",
      "9" = "Quetzaltenango"
    )
  ) %>%
  # Tag with municipality names
  rename(muni_id = municipality) %>%
  left_join(municipalities) %>%
  select(year, department, municipality, age_group, births)




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




#------------------------------------------------------------------------------*
# Fix missing ages for each mid-year ----
#------------------------------------------------------------------------------*
# Use simple year age projections data
#------------------------------------------------------------------------------*

# Prepare simple year projections from raw data
processed_file <- system("Rscript scripts/collect-raw-municipality.R", intern = TRUE)
load(file = processed_file[length(processed_file)] )
rm(processed_file)

# Sum simple year projections for 
simple_year <- population %>%
  filter(
    between(year, 2010, 2015),                            # Years missing data
    between(age, 0, 4),                                   # Relevant ages
    department %in% c("Santa Rosa", "Quetzaltenango")     # Relevant departments
  ) %>%
  group_by(year, department, municipality, age) %>%
  summarize(population = sum(population)) %>%
  # Keep necessary data
  filter(
    (year == 2013 & age == 4) |
      (year == 2012 & between(age, 3, 4)) |
      (year == 2011 & between(age, 2, 4)) |
      (year == 2010 & between(age, 1, 4))
  ) %>%
  # Label age groups
  mutate(
    age_group = recode(
      age,
      "1" = "12-23 months",
      "2" = "24-35 months",
      "3" = "36-59 months",
      "4" = "36-59 months"
    ),
    age_group = factor(age_group, levels = age_groups, ordered = TRUE)
  ) %>%
  # Summarize
  group_by(year, department, municipality, age_group) %>%
  summarize(population = sum(population))




# End of script
