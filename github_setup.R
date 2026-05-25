# Automatyczne przygotowanie projektu do GitHub
# Uruchom w glownym folderze projektu: source("github_setup.R")

if (!requireNamespace("usethis", quietly = TRUE)) install.packages("usethis")
library(usethis)
use_git()
use_github(name = "salesTSKinga", private = FALSE)
