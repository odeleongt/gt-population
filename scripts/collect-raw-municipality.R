#------------------------------------------------------------------------------*
# Collect raw population data at the municipality level
#------------------------------------------------------------------------------*

#------------------------------------------------------------------------------*
# Prepare environment ----
#------------------------------------------------------------------------------*

# Load used packages
library(package = "readxl")
library(package = "tidyverse")

# Set metadata
data_path <- "data/raw/municipalities/"
temp_path <- paste(tempdir(), "raw_muni_data", sep = "/")
dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)




#------------------------------------------------------------------------------*
# Collect data from 2000-2010 period ----
#------------------------------------------------------------------------------*

# Zip path
zip_path <- paste0(data_path, "2000-2010.zip")

# Unzip files
file_list <- unzip(zipfile = zip_path, list = TRUE) %>%
  pull(Name) %>%
  file.path(temp_path, .)

unzip(zipfile = zip_path, exdir = temp_path)


#------------------------------------------------------------------------------*
# funtion to read files
#------------------------------------------------------------------------------*
read_population_2000 <- function(file_path, sheet = "Población TOTAL", skip = 0){
  # Get department name from file name
  department <- gsub(
    "(^| )([a-z])", "\\1\\U\\2",
    gsub("(^/([^/]+/)+)|([0-9]+ )|([^a-z .])|(.xls$)", "", tolower(file_path)),
    perl = TRUE
  )
  
  # Report current department
  cat("\nReading: ", department, "\n", sep = "")
  
  # Read file contents
  pop_file <- read_excel(
    path = file_path, sheet = sheet, skip = skip , na = "?"
  )
  
  # Get municipality names
  municipalities <- pop_file %>%
    slice(1) %>%
    unlist %>%
    na.omit %>%
    setdiff(0) %>%
    rep(each = 3)
  
  # Get first year
  first_year <- names(pop_file)[1]
  
  # Get column group names
  column_group_names <- c(
    "",
    rep(
      c(":Total", ":Masculino", ":Femenino"),
      times = (length(municipalities) / 3) + 1
    )
  )
  
  # Set variable names
  pop_file <- pop_file %>%
    # Select useful columns and rows
    select(seq(1, 4 + length(municipalities))) %>%
    slice(-1) %>%
    # Fix names
    set_names(
      c("age", rep(names(.)[2], 3), municipalities)
    ) %>%
    set_names(
      paste0(names(.), column_group_names)
    ) %>%
    set_names(
      gsub(" +", "_", tolower(iconv(names(.), to = "ASCII//TRANSLIT")))
    ) %>%
    # Set first year
    mutate(age = c(first_year, age[-1])) %>%
    # Fix age variables and extract years
    mutate(age = as.integer(gsub("['+]", "", age))) %>%
    filter(!is.na(age)) %>%
    mutate(
      group = stats::filter(age > 100, 1, "recursive")
    ) %>%
    group_by(group) %>%
    mutate(year = first(age)) %>%
    ungroup %>%
    filter(age < 100) %>%
    mutate(
      year = ifelse(year < 100, min(year[year > 100]) - 1, year)
    ) %>%
    gather(key = variable, value = population, -year, -age, -group) %>%
    separate(variable, into = c("municipality", "sex"), sep = ":") %>%
    filter(sex != "total", municipality != "total") %>%
    # Tag with department
    mutate(
      municipality = gsub(
        "(^| )([a-z])", "\\1\\U\\2",
        gsub("_", " ", municipality),
        perl = TRUE
      ),
      department = department
    ) %>%
    select(year, department, municipality, sex, age, population) %>%
    mutate(
      # Fix factors
      sex = recode(
        sex,
        masculino = "male",
        femenino = "female"
      ),
      population = as.numeric(population)
    )
  
  # Return data
  return(pop_file)
}

# Read in all files
pop_2000_2010 <- lapply(file_list, read_population_2000) %>%
  bind_rows()

# Remove files
file.remove(file_list)


    

#------------------------------------------------------------------------------*
# Collect data from 2011-2015 period ----
#------------------------------------------------------------------------------*

# Zip path
zip_path <- paste0(data_path, "2011-2015.zip")

# Unzip files
file_list <- unzip(zipfile = zip_path, list = TRUE) %>%
  pull(Name) %>%
  file.path(temp_path, .)

unzip(zipfile = zip_path, exdir = temp_path)


#------------------------------------------------------------------------------*
# funtion to read files
#------------------------------------------------------------------------------*
read_population_2011 <- function(file_path, skip = 3){
  # Get department name from file name
  department <- gsub(
    "(^| )([a-z])", "\\1\\U\\2",
    gsub("(^/([^/]+/)+)|([0-9]+ )|([^a-z .])|(.xls$)", "", tolower(file_path)),
    perl = TRUE
  )
  
  # Report current department
  cat("\nReading: ", department, "\n", sep = "")
  
  # Get file sheets
  file_sheets <- excel_sheets(file_path)
  
  
  # Function to read each sheet
  read_pop_sheet <- function(sheet, file = file_path, skip_lines = skip){
    
    # Report sheet (year)
    cat(" ", sheet)
    
    
    #--------------------------------------------------------------------------*
    # Fix parameters for specific files / sheets
    #--------------------------------------------------------------------------*
    
    # Fix individual skip parameters
    fixes_skip <- c(
      "Totonicapan", "Suchitepequez", "Retalhuleu", "Quiche", "Alta Verapaz",
      "Peten", "Jutiapa", " Jalapa", "Chiquimula", "Izabal", "Santa Rosa"
    )
    
    skip <- case_when(
      department %in% fixes_skip & sheet == "2012" ~ 4,
      department %in% fixes_skip & sheet == "2015" ~ 5,
      TRUE ~ skip
    )
    
    # Fix missing municipality names
    fix_municipalities <- function(.data){
      .data %>%
        # Setup fixing rules
        mutate(
          municipality = case_when(
            department == "Santa Rosa" & grepl("^X", municipality) ~ "Nueva Santa Rosa",
            TRUE ~ municipality
          )
        ) %>%
        return()
    }
    
    #--------------------------------------------------------------------------*
    # Get data
    #--------------------------------------------------------------------------*
    
    # Read file contents
    pop_sheet <- read_excel(
      path = file, sheet = sheet, skip = skip_lines , na = "?"
    )
    
    # Fix sheet names
    if(is.na(as.integer(sheet))){
      sheet <- as.character(2010 + as.integer(gsub("[^0-9]", "", sheet)))
      cat(" (", sheet, ") ", sep = "")
    }
    
    # Set variable names
    pop_sheet <- pop_sheet %>%
      # Fix names
      set_names(
        gsub(" +", "_", tolower(iconv(names(.), to = "ASCII//TRANSLIT")))
      ) %>%
      # Fix age variables and extract years
      filter(!is.na(sexo)) %>%
      mutate(
        year = as.integer(sheet),
        age = as.integer(gsub("['+]", "", sexo)),
        group = stats::filter(is.na(age), 1, "recursive")
      ) %>%
      group_by(group) %>%
      mutate(sexo = first(sexo)) %>%
      ungroup %>%
      filter(!is.na(age)) %>%
      gather(key = municipality, value = population, -year, -age, -group, -sexo) %>%
      # Tag with department
      mutate(
        municipality = gsub(
          "(^| )([a-z])", "\\1\\U\\2",
          gsub("_", " ", municipality),
          perl = TRUE
        ),
        department = department
      ) %>%
      # Fix missingmunicipality names
      fix_municipalities() %>%
      select(year, department, municipality, sex = sexo, age, population) %>%
      mutate(
        # Fix factors
        sex = recode(
          tolower(sex),
          hombres = "male",
          mujeres = "female"
        ),
        population = as.numeric(population)
      )
    
    # Return data
    return(pop_sheet)
  }
  
  # Read all sheets
  pop_file <- lapply(file_sheets, read_pop_sheet) %>%
    bind_rows()
  
  cat("\n")
  
  # Return data
  return(pop_file)
}

# Read in all files
pop_2011_2015 <- file_list %>%
  grep("^[^()]+$", ., value = TRUE) %>%
  lapply(read_population_2011) %>%
  bind_rows()

# Remove files
file.remove(file_list)




#------------------------------------------------------------------------------*
# Bind datasets ----
#------------------------------------------------------------------------------*


pop_2000_2015 <- pop_2000_2010 %>%
  bind_rows(pop_2011_2015) %>%
  # Fix categories
  mutate(
    department = recode(
      department,
      "Huehetenango" = "Huehuetenango",
      " Jalapa" = "Jalapa"
    ),
    municipality = gsub("(^ +)|( +$)", "", municipality),
    municipality = recode(
      municipality,
      "Santa Catarina La Tinta" = "Santa Catalina La Tinta",
      "Melchor" = "Melchor De Mencos",
      "San Agustin Acasagustlan" = "San Agustin Acasaguastlan",
      "Chinuautla" = "Chinautla",
      "Petapa" = "San Miguel Petapa",
      "Acatempa" = "San Jose Acatempa",
      "San Martin Zapotitlan" = "San Martin Zapotitlan",
      "San Bartolome" = "San Bartolome Milpas Altas",
      "Santa Maria De Jesus" = "Santa Maria De Jesus",
      "Santiago Sactepequez" = "Santiago Sacatepequez",
      "Santa Cruz Naranajo" = "Santa Cruz Naranjo",
      "Santa Catalina Ixtahuacan" = "Santa Catarina Ixtahuacan"
    ),
    # Fix Huehuetenango
    municipality = ifelse(
      test = department == "Huehuetenango",
      yes = recode(
        municipality,
        "Barrillas" = "Santa Cruz Barillas",
        "Concepcion" = "Concepcion Huista",
        "San  Idelfonso Ixtahuacan" = "San Idelfonso Ixtahuacan",
        "Santiago Chimaltenanango" = "Santiago Chimaltenango"
      ),
      no = municipality
    ),
    # Population as a number
    population = as.numeric(population)
  )

# Clean up
rm(pop_2000_2010, pop_2011_2015)




#------------------------------------------------------------------------------*
# Collect data from 2016-2020 period ----
#------------------------------------------------------------------------------*

#------------------------------------------------------------------------------*
# Load raw data 2016 - 2020
#------------------------------------------------------------------------------*
# Source:
# http://epidemiologia.mspas.gob.gt/files/Proyecciones%20Poblacion%202000-2020/
#------------------------------------------------------------------------------*


#------------------------------------------------------------------------------*
# funtion to read files
#------------------------------------------------------------------------------*
read_population_2016 <- function(file_path, skip = 0){
  
  # Read file to gather variable metadata
  pop_sheets <- excel_sheets(path = file_path) %>%
    set_names(.)
  
  
  #----------------------------------------------------------------------------*
  # Function to read each sheet
  #----------------------------------------------------------------------------*
  read_pop_sheet <- function(sheet){
    
    # Report sheet (year)
    cat(" ", sheet)
    
    # Configuration exceptions
    skip <- case_when(
      sheet == "Total" ~ 3,
      sheet == "Hombres" ~ 3,
      sheet == "Mujeres" ~ 3,
      TRUE ~ skip
    )
    
    # Read sheet contents
    pop_sheet <- read_excel(file_path, sheet = sheet, skip = skip, na = "-")
    
    # Get variable names
    years <- pop_sheet %>%
      slice(1) %>%
      unlist %>%
      setNames(NULL) %>%
      ifelse(is.na(.), "", .)
    
    headers <- pop_sheet %>%
      names %>%
      tibble(header = .) %>%
      mutate(
        header = ifelse(grepl("x_", header, ignore.case = TRUE), NA, header),
        group = stats::filter(!is.na(header), 1, "recursive")
      ) %>%
      group_by(group) %>%
      mutate(
        label = tolower(first(header)),
        label = gsub("[ y\n]+", "_", label)
      ) %>%
      pull(label)
    
    var_names <- paste(headers, years, sep = "_")
    
    # Prepare data
    pop_sheet <- pop_sheet %>%
      # Set names and keep unique columns
      set_names(var_names) %>%
      subset(select = !duplicated(names(.)) & !grepl("_[0-9]_", names(.))) %>%
      # Fix department and municipality data
      mutate(
        department = ifelse(
          test = is.na(lag(departamento_municipio_, n = 1)),
          yes = departamento_municipio_,
          no = NA
        ),
        group = as.integer(stats::filter(!is.na(department), 1, "recursive"))
      ) %>%
      filter(!is.na(departamento_municipio_)) %>%
      group_by(group) %>%
      mutate(department = first(department)) %>%
      slice(-1) %>%
      ungroup %>%
      select(
        department, municipality = departamento_municipio_, everything(), -group
      ) %>%
      # Gather year and type
      gather(
        key = group, value = population, -department, -municipality
      ) %>%
      mutate(population = as.numeric(population)) %>%
      separate(col = group, into = c("type", "year"), convert = TRUE) %>%
      select(type, year, department, municipality, population)
    
    # Return data
    return(pop_sheet)
  }
  
  
  # Read all sheets
  pop_file <- lapply(pop_sheets, read_pop_sheet) %>%
    bind_rows(.id = "sex") %>%
    mutate(
      sex = recode(
        sex,
        Hombres = "male",
        Mujeres = "female",
        Total = "all"
      )
    )
  
  cat("\n")
  
  # Return data
  return(pop_file)
}



file_name <- "data/raw/municipalities/2016-2020.xlsx" %>%
  set_names(.)

# Read all files
pop_2016_2020_aggregate <- lapply(
  file_name,
  read_population_2016
) %>%
  bind_rows()




#------------------------------------------------------------------------------*
# Project simple age estimates for 2016-2020 ----
#------------------------------------------------------------------------------*

# Binom model for proportion of population in each age
pop_binom <- pop_2000_2015 %>%
  group_by(department, municipality, year, sex) %>%
  mutate(
    total = sum(population),
    prop = population / total
  ) %>%
  # Model for each "independent" group
  group_by(department, municipality, sex) %>%
  do({
    data <- .
    
    tibble(
      binomial = list(
        glm(
          data = data, formula = population / total ~ year + age,
          family = binomial(link = "logit"), weights = total
        )
      )
    )
  }) %>%
  ungroup


# Match municipality names
pop_2016_2020_aggregate <- pop_2016_2020_aggregate %>%
  # Standardize characters and casing
  mutate_at(
    vars(department, municipality),
    funs(
      gsub(
        "(^| )([a-z])", "\\1\\U\\2",
        gsub("(^ +)|( +$)", "", tolower(iconv(., to = "ASCII//TRANSLIT"))),
        perl = TRUE
      )
    )
  ) %>%
  # Fix mismatches
  mutate(
    municipality = recode(
      municipality,
      "La Tinta" = "Santa Catalina La Tinta",
      "Santa Catarina La Tinta" = "Santa Catalina La Tinta",
      "San Agustin Acasagustlan" = "San Agustin Acasaguastlan",
      "Chinuautla" = "Chinautla",
      "Petapa" = "San Miguel Petapa",
      "Santiago Chimaltenanango" = "Santiago Chimaltenango",
      "Acatempa" = "San Jose Acatempa",
      "San Bartolome" = "San Bartolome Milpas Altas",
      "Santiago Sactepequez" = "Santiago Sacatepequez",
      "Santa Catalina Ixtahuacan" = "Santa Catarina Ixtahuacan"
    ),
    municipality = ifelse(
      test = department == "Huehuetenango" & municipality == "Concepcion",
      yes = "Concepcion Huista",
      no = municipality
    )
  )



# Check for mismatches
pop_2016_2020_aggregate %>%
  group_by(department, municipality) %>%
  tally %>%
  filter(
    !municipality %in% pop_2000_2015$municipality
  ) %>%
  nrow %>%
  {if(. > 0) stop("Check municipality names, found ", ., " mismatches.")}


# Get proportion by age for each year
pop_2016_2020_predicted <- pop_2016_2020_aggregate %>%
  filter(sex %in% c("male", "female"), type == "total") %>%
  left_join(
    expand.grid(
      age = unique(pop_2000_2015$age),
      year = unique(pop_2016_2020_aggregate$year)
    )
  ) %>%
  nest(year, age, population) %>%
  left_join(pop_binom) %>%
  mutate(
    predicted = map2(
      .x = binomial, .y = data,
      .f = ~cbind(.y, predicted = predict(.x, .y, type = "response"))
    )
  )


# Predict population by simeple age
pop_2016_2020 <- pop_2016_2020_predicted %>%
  unnest(predicted) %>%
  group_by(department, municipality, year, sex) %>%
  mutate(
    predicted = predicted / sum(predicted),
    population = population * predicted
  ) %>%
  ungroup %>%
  select_(.dots = names(pop_2000_2015))




#------------------------------------------------------------------------------*
# Save data ----
#------------------------------------------------------------------------------*

# Bind with previous years
population <- pop_2000_2015 %>%
  bind_rows(pop_2016_2020)

processed_file <- "data/processed/gt_2000_2020_municipality_population.RData"

# Save population data for use in R
save(
  population, file = processed_file
)

cat(processed_file)

# End of script
