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
    event_year = year(event_date),
    weight_kg = case_when(
      libras == "99" | onzas == "99" ~ NA_real_,
      TRUE ~ (as.numeric(libras) + as.numeric(onzas) / 16) / 2.20462
    ),
    sex = recode(
      sexo,
      "1" = "male",
      "2" = "female"
    ),
    birth_type = recode_factor(
      tipar,
      "1" = "simple",
      "2" = "double",
      "3" = "triple",
      "4" = "multiple"
    ),
    delivery_type = recode_factor(
       viapar,
       "1" = "Cesarean",
       "2" = "Vaginal"
    ),
    mother_group = recode_factor(
      pueblopm,
      "1" = "Maya",
      "2" = "Garífuna",
      "3" = "Xinka",
      "4" = "Mestizo / Ladino",
      "5" = "Other",
      "9" = "Ignored"
    ),
    father_group = recode_factor(
      pueblopp,
      "1" = "Maya",
      "2" = "Garífuna",
      "3" = "Xinka",
      "4" = "Mestizo / Ladino",
      "5" = "Other",
      "9" = "Ignored"
    )
  ) %>%
  select(
    # Event data
    event_year, event_date, event_department = depocu, event_municipality = mupocu,
    # Record metadata
    record_date, record_department = depreg, record_municipality = mupreg,
    sex, weight_kg, birth_type, delivery_type,
    # Mother residency location
    mother_department = deprem, mother_municipality = muprem,
    mother_age = edadm, mother_group, father_group
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
    correlative = 1,
    label = "0-27 days",
    date_threshold = mid_year - days(28)
  ) %>%
  bind_rows(
    mutate(
      .,
      correlative = 2,
      label = "28 days-<3 month",
      date_threshold = mid_year - months(3)
    ),
    mutate(
      .,
      correlative = 3,
      label = "3-5 months",
      date_threshold = mid_year - months(6)
    ),
    mutate(
      .,
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
  count(
    year = year(mid_year), department, municipality, age_group = label,
    wt = n
  ) %>%
  rename(births = n) %>%
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


# save deaths details

det_deaths <- deaths %>%
  mutate(
    death_year = case_when(
      is.na(anoocu) & !is.na(anoreg) ~ as.integer(anoreg),
      anoocu == "9" ~ 2009L,
      anoocu == "10" ~ 2010L,
      TRUE ~ as.integer(anoocu)
    ),
    death_date = ymd(
      paste(
        death_year,
        stringr::str_pad(mesocu, 2, "left", pad = "0"),
        stringr::str_pad(diaocu, 2, "left", pad = "0"),
        sep = "-"
      )
    ),
    assistance_type = recode_factor(
      asist,
      "1" = "Medic",
      "2" = "Paramedic",
      "3" = "Midwife",
      "4" = "Empiric",
      "5" = "None",
      "9" = "Ignored"
    ),
    death_site = recode_factor(
      ocur,
      "1" = "Hospital, public",
      "2" = "Hospital, private",
      "3" = "Health center",
      "4" = "Social security",
      "5" = "Public way",
      "6" = "Household",
      "7" = "Place of work",
      "8" = "Other",
      "9" = "Ignored"
    ),
    certified_death = recode_factor(
      cerdef,
      "1" = "Medic",
      "2" = "Paramedic",
      "3" = "Authority",
      "9" = "Ignored"
    ),
    sex = recode(
      sexo,
      "1" = "male",
      "2" = "female"
    ),
    cult_group = recode(
      puedif,
      "1" = "Maya",
      "2" = "Garífuna",
      "3" = "Xinka",
      "4" = "Mestizo / Ladino",
      "5" = "Other",
      "9" = "Ignored"
    ),
    age_unit = recode(
      perdif,
      "1" = "days",
      "2" = "months",
      "3" = "years",
      .default = NA_character_,
      .missing = NA_character_
    ),
    age_value = as.integer(edadif)
  ) %>%
  select(
    death_year, death_date,
    sex, age_value, age_unit,
    death_department = dnadif, death_municipality = mnadif,
    dept_resid = dredif, muni_resid = mredif, cult_group,
    cause_icd10 = caudef,
    cause_description = caudef.descrip,
    assistance_type, death_site, certified_death
  ) %>%
  print()




# Prepare deaths "events" dataset
local_deaths <- deaths %>%
  filter(
    # Ignore unknown ages
    as.integer(edadif) < 999,
    perdif != "9",
    # Ignore >= 1 year
    perdif %in% c("1", "2"),
    # Keep only used departments
    depreg %in% c("6", "9")
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
    age_value = as.integer(edadif)
  ) %>%
  # Calculate ages case by case
  rowwise() %>%
  do({
    bind_cols(
      .,
      data_frame(birth_date = .$event_date - period(.$age_value, units = first(.$age_unit)))
    )
  }) %>%
  ungroup %>%
  select(
    # Event data
    event_year, event_date, event_department = depreg, event_municipality = mupreg,
    # Deceased data
    age_value, age_unit, birth_date
  )


# Label death events
labeled_deaths <- local_deaths %>%
  # Add mid year dates
  left_join(mid_years) %>%
  # Keep only relevant dates (i.e. died before mid-year)
  filter(
    event_date < mid_year,               # died before mid-year
    birth_date > (mid_year - years(1))   # born at most one year prior
  ) %>%
  # Count relevant deaths by birth date
  count(
    mid_year, event_department, event_municipality, birth_date
  ) %>%
  # Tag all births for both age group types
  mutate(
    correlative = 1,
    label = "0-27 days",
    date_threshold = mid_year - days(28)
  ) %>%
  bind_rows(
    mutate(
      .,
      correlative = 2,
      label = "28 days-<3 month",
      date_threshold = mid_year - months(3)
    ),
    mutate(
      .,
      correlative = 3,
      label = "3-5 months",
      date_threshold = mid_year - months(6)
    ),
    mutate(
      .,
      correlative = 4,
      label = "6-8 months",
      date_threshold = mid_year -  months(9)
    ),
    mutate(
      .,
      correlative = 5,
      label = "9-11 months",
      date_threshold = mid_year -  months(12)
    )
  ) %>% 
  # Assign possible age groups
  mutate(
    keep = birth_date >= date_threshold
  ) %>%
  filter(keep) %>%
  select(-keep, -date_threshold) %>%
  # Pick oldest applicable age group
  arrange(mid_year, birth_date, correlative) %>%
  group_by(mid_year, birth_date) %>%
  filter(correlative == min(correlative)) %>%
  ungroup %>%
  # Label municipalities
  left_join(municipalities, by = c(event_municipality = "muni_id")) %>%
  mutate(
    # Label departments
    department = recode(
      event_department,
      `6` = "Santa Rosa",
      `9` = "Quetzaltenango"
    ),
    # Configure age group as factor
    label = factor(label, levels = age_groups, ordered = TRUE)
  ) %>%
  # Count deaths by age group
  group_by(
    year = year(mid_year), department, municipality, age_group = label
  ) %>%
  summarize(
    deaths = sum(n)
  ) %>%
  ungroup




#------------------------------------------------------------------------------*
# Calculate proportion contributed by each <1 year age group ----
#------------------------------------------------------------------------------*

# Get proportions by year, department, municipality and age group
proportion_age_group <- local_births %>%
  left_join(labeled_deaths) %>%
  mutate(
    # Fill missing values with 0
    deaths = ifelse(is.na(deaths), 0, deaths),
    # Get alive count at mid-year
    alive = births - deaths
  ) %>%
  group_by(year, department, municipality) %>%
  mutate(
    proportion = alive / sum(alive)
  )

# PLot proportions by year, department, municipality and age group
plot_age_groups <- proportion_age_group %>%
  ggplot(aes(x = year, y = proportion)) +
  geom_line(
    aes(group = municipality),
    alpha = 0.3
  ) +
  geom_line(
    data = summarize(
      group_by(proportion_age_group, year, department, age_group),
      proportion = mean(proportion)
    ),
    color = "red", size = 1
  ) +
  facet_grid(age_group ~ department) +
  theme_bw()

# Summarize to single proportion by age group <1 year
proportion_age_group <- proportion_age_group %>%
  group_by(age_group) %>%
  summarize(
    proportion = mean(proportion)
  )

# Check deviations from uniform distribution assumption
different_rows <- proportion_age_group %>%
  mutate(
    group_month_interval = c(1, 2, 3, 3, 3),
    uniform_proportion = group_month_interval / 12,
    deviation = proportion - uniform_proportion
  ) %>%
  filter(
    deviation > 0.01
  )

if( nrow(different_rows) > 0 ) stop("Data deviates from uniform assumption")




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
flu_age_groups <- population %>%
  filter(
    between(year, 2010, 2016),                            # Years missing data
    between(age, 0, 4),                                   # Relevant ages
    department %in% c("Santa Rosa", "Quetzaltenango")     # Relevant departments
  ) %>%
  group_by(year, department, municipality, age) %>%
  summarize(population = sum(population)) %>%
  # Add population proportion for each age group <1
  left_join(
    mutate(proportion_age_group, age = 0, age_group = as.character(age_group))
  ) %>%
  mutate(
    # Estimate population for age groups <1 year
    population = ifelse(
      test = age == 0 & !is.na(proportion),
      yes = population * proportion,
      no = population
    ),
    age_group = recode(
      age,
      "1" = "12-23 months",
      "2" = "24-35 months",
      "3" = "36-59 months",
      "4" = "36-59 months",
      .default = age_group
    ),
    age_group = factor(age_group, levels = age_groups, ordered = TRUE)
  ) %>%
  # Add cummulative age_groups
  bind_rows(
    mutate(filter(., age_group < "12-23 months"), age_group = "0-11 months"),
    mutate(., age_group = "0-59 months"),
    mutate(filter(., age_group > "0-11 months"), age_group = "12-59 months"),
    mutate(filter(., age_group > "12-23 months"), age_group = "24-59 months")
  ) %>%
  # Orger age groups
  mutate(
    age_group = factor(age_group, levels = age_groups, ordered = TRUE)
  ) %>%
  # Summarize
  group_by(department, municipality, year, age_group) %>%
  summarize(population = sum(population))


# Save population estimates for the age groups
write_csv(flu_age_groups, path = "output/flu_edinburgh_population.csv")




# End of script
