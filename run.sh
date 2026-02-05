#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

Rscript 01_SETUP.R
Rscript 02_DESCRIPTIVE_ANALYSIS.R
Rscript 03_REGRESSION_ANALYSIS.R
Rscript 04_Report.R
