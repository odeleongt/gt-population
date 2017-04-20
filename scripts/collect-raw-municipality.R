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
# Collect data from 2000-2010 period ----
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
    
    fixes <- c(
      "Totonicapan", "Suchitepequez", "Retalhuleu", "Quiche", "Alta Verapaz",
      "Peten", "Jutiapa", " Jalapa", "Chiquimula", "Izabal", "Santa Rosa"
    )
    
    # Configuration exceptions
    skip <- case_when(
      # department == "Santa Rosa" & sheet == "2012" ~ 4,
      # department == "Santa Rosa" & sheet == "2015" ~ 5,
      department %in% fixes & sheet == "2012" ~ 4,
      department %in% fixes & sheet == "2015" ~ 5,
      TRUE ~ skip
    )
    
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
# Collect data from 2016-2020 period ----
#------------------------------------------------------------------------------*




# End of script
