library(future)
library(future.batchtools)
library(batchtools)
library(targets)
library(tarchetypes)
library(here)
library(ggplot2)
source(here("code", "targets_functions", "functions.R"))

# @@@@@@@@@@@@
# Set Up -----
# @@@@@@@@@@@


# Initiate file for defining slurm resources to use in targets
source(here::here("_targets_resources.conf.R"))

ggplot2::theme_set(theme_bw())

options(tidyverse.quiet = TRUE)

tar_option_set(
  resources = resources_all,
  packages = c(
    "janitor",
    "lubridate",
    "fs",
    "tidyverse",
    "vroom",
    "openxlsx"
  ),
  workspace_on_error = TRUE,
  storage = "worker",
  garbage_collection = TRUE,
  retrieval = "worker",
  memory = "transient"
)

list(
  # tar_file_read(
  #   file_to_import,
  #   here::here("data", "..."),
  #   readxl::read_xlsx(path = !!.x)
  # ),


)
