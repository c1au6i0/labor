#' sync_proj
#'
#' sync the project folder with a local folder
#'
#' @param direction one of `c("l", "r", "bd")`
#' @details use rsync
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
sync_proj <- function(direction, path) {
  rsync_installed <- system("rsync --version") == 0
  if (rsync_installed == FALSE) {
    error("rsync installation not found! Please install rsync...")
  }
}

#' purlify
#'
#' Transform an `RMarkdown notebook` in a `R script`
#'
#' @param file path to file to `purlify`
#' @details the function relies on `knitr::purl` for the muscle work, and then clean the file up further.
#' @return `R` script  with layout common in the lab. The text is commented.
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
purlify <- function(file) {
  knitr::purl(file, documentation = 2)
}

#' add_lfolder
#'
#' Add lab folder and relative documentation in the appropriate file
#'
#' @param name_lf name of lab folder
#' @param path path to folder
#' @param inter boolean for using `svDialogs` to inteactively select folder
#' @return
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
add_lfolder <- function(name_lf, path = NULL, inter = TRUE) {
  knitr::purl(file, documentation = 2)

  # svDialog
}

#' remove_lfolder
#'
#' Remove lab folder and relative documentation in the appropriate file
#'
#' @param name_lf name of lab folder
#' @param path path to folder
#' @param inter boolean for using `svDialogs` to inteactively select folder
#' @return
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
remove_lfolder <- function(name_lf, path = NULL, inter = TRUE) {
  knitr::purl(file, documentation = 2)

  # svDialog
}

#' clean_up
#'
#' Check if folder and file are in the right place documentation is present and if not
#' cleans it up.
#'
#' @export
clean_up <- function() {


}


#' who_wants_to_talk
#'
#' @param lab one of `marchionni", "wheelan", "joint`
#'
#' @return a message

who_wants_to_talk <- function(lab = "marchionni") {
  if (!lab %in% c("marchionni", "wheelan", "joint")) stop("lab can only be marchionni, wheelan
                                                         , joint!")

  marchionni_lab <- c("Luigi", "Claudio", "Eddie", "Mohamed", "Wikum")
  wheelan_lab <- c("Sara", "McKinzie", "Heater", "Lauren", "Alyza")

  if (lab == "marchionni") members <- marchionni_lab
  if (lab == "wheelan") members <- wheelan_lab
  if (lab == "joint") members <- append(marchionni_lab, wheelan_lab)

  cat("Here the order:\n", paste0("[", seq(1, length(members)), "]: ", members[sample(length(members))], "\n"))
}
