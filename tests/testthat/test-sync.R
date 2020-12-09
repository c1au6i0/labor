# this is so we can test the setup
library(labor)

set_sync_lab(test = "test_set_sync")

destination_x <-  scan(file.path("test_origin", ".labor_destination"), comment.char = "#", what = "character", n = 1, quiet = TRUE)

test_that("labor_destination is created", {
  expect_equal(destination_x , file.path(getwd(), "test_destination"))
})

unlink("test_origin", recursive = TRUE)
unlink("test_destination", recursive = TRUE)

# sync_lab(x = here::here("laboR", "data-raw"), inter = FALSE)
#
# expected_files <- list.files(here::here("laboR", "data-raw"))
# actual_files <- list.files(paste0(here("laboR", "tests", "test_folder"), "/"))
#
# test_that("sync files", {
#   expect_equal(actual_files, expected_files )
# })
