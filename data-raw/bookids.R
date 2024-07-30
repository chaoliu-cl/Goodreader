# In data-raw/bookids.R

# Create the inst/extdata directory if it doesn't exist
dir.create(file.path("inst", "extdata"), showWarnings = FALSE, recursive = TRUE)

# Copy the bookids.txt file to inst/extdata using a relative path
file.copy("data-raw/bookids.txt", "inst/extdata/bookids.txt", overwrite = TRUE)
