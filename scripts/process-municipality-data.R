#------------------------------------------------------------------------------*
# Produce population data at the municipality level
#------------------------------------------------------------------------------*


#------------------------------------------------------------------------------*
# Load data ----
#------------------------------------------------------------------------------*

# Prepare snapshot from raw data
processed_file <- system("Rscript scripts/collect-raw-municipality.R", intern = TRUE)
load(file = processed_file[length(processed_file)] )


# Clean up
rm(processed_file)


# End of script
