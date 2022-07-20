library(targets)
# _targets.R

library(targets)
library(tarchetypes)
library(here)
source(here("code", "targets_functions", "functions.R"))

# @@@@@@@@@@@@
# Set Up -----
# @@@@@@@@@@@

options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c(
    "janitor",
    "lubridate",
    "tidyverse",
    "vroom",
    "openxlsx"
  ),
  workspace_on_error = TRUE
   # ,debug = "list_sheets_cl"
)


list(


)
