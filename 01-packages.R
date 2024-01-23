# Define vector of package names
packages <- c("ggplot2", "RColorBrewer", "tidyverse", "psych", "summarytools", "htmlTable", "dplyr", "stringr", "lsr", "gtsummary", "flextable", "officer", "rcompanion", "reshape2", "openxlsx", "lavaan", "ggtext", "drc", "ggh4x", "EnvStats", "GPArotation", "corrplot")

# Load packages
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

rm(packages, package)

