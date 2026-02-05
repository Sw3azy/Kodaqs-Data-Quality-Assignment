# ==============================================================================
# Script: 01_setup.R
# Purpose: Load packages and configure environment
# Author: Lukas Rädle
# Date: February 2026
# ==============================================================================

# Load required packages ----
cat("Loading required packages...\n")


library(tidyverse)   # Data manipulation and visualization
library(here)        # Project-relative paths
library(zoo)         # Rolling averages
library(scales)      # Scale functions for plots
library(patchwork)   # Combining plots

# Create output directories if they don't exist ----
cat("Creating output directories...\n")

dir.create(here("output"), showWarnings = FALSE)
dir.create(here("output", "figures"), showWarnings = FALSE)
dir.create(here("output", "tables"), showWarnings = FALSE)
dir.create(here("docs"), showWarnings = FALSE)

# Set global options ----
options(scipen = 999)  # Disable scientific notation
theme_set(theme_minimal())  # Set default ggplot theme

# Save session info for reproducibility ----
cat("Saving session info...\n")
sink(here("docs", "session_info.txt"))
cat("R Session Information\n")
cat("====================\n\n")
print(sessionInfo())
sink()

cat("Setup complete!\n\n")