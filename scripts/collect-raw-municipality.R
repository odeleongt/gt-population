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


# Remove files
file.remove(file_list)




#------------------------------------------------------------------------------*
# Collect data from 2016-2020 period ----
#------------------------------------------------------------------------------*




# End of script
