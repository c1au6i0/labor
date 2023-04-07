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
  workspace_on_error = TRUE,
  garbage_collection = TRUE,
  memory = "transient"
)


list(
  # tar_file_read(
  #   file_to_import,
  #   here::here("data", "..."),
  #   readxl::read_xlsx(path = !!.x)
  # ),


)
