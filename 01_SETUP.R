# ==============================================================================
# Script: 01_setup.R
# Purpose: Load packages and configure environment
# ==============================================================================

cat("Starting setup...\n")

# Load required packages ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(zoo)
  library(scales)
  library(patchwork)
})

cat("Packages loaded successfully\n")

# Verify data file exists ----
data_path <- here("IntegratedData.csv")

if (!file.exists(data_path)) {
  stop("ERROR: Data file not found at: ", data_path)
}

cat("Data file verified\n")

# Create output directories ----
dirs <- c(
  here("output"),
  here("output", "figures"),
  here("output", "tables"),
  here("docs")
)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

cat("Output directories ready\n")

# Set global options ----
options(scipen = 999)
theme_set(theme_minimal())

# Save session info ----
session_file <- here("docs", "session_info.txt")
sink(session_file)
cat("R Session Information\n")
cat("=====================\n\n")
print(sessionInfo())
sink()

cat("Session info saved\n")
cat("Setup complete!\n\n")