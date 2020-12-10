#' Set the destination of the syncing.
#'
#' Interactively set the folder to sync. That info will be recorder in `.labor_destination` in the parental directory.
#'
#' @export
set_sync_lab <- function(...) {
  try(
    test <- unlist(list(...)),
    silent = TRUE
  )

  if (!is.null("test")) {
    svDialogs::dlg_message("Please set the Destination folder...")
    destination <- svDialogs::dlg_dir()$res
    origin <- here::here()
  } else {
    # this is used in testthat
    if (test == "test_set_sync") {
      folders_test <- list("test_destination", "test_origin")
      lapply(folders_test, dir.create)

      destination_origin <- lapply(folders_test, function(x) file.path(getwd(), x))
      names(destination_origin) <- c("destination", "origin")
      list2env(destination_origin, environment())
    }
  }


  if (file.exists(file.path(origin, ".labor_destination"))) {
    unlink(file.path(origin, ".labor_destination"))
  }

  sink(file.path(origin, ".labor_destination"))
  cat(paste0("# Destination Path for lab_sync\n", "\"", destination, "\""))
  sink()

  message(cat("Destination set to ", destination, "..."))
}


#' Sync lab project.
#'
#' sync the project folder with the local folder set by `set_lab_sync`. The origin is always the parental director found
#' by `here::here()`.
#'
#' @param direction one of `c("or_de", "de_or", "bidir") meaning `origin to destination`, `destination to origin` or
#'      `bidirectional`. Defalut is `or_de`.
#' @param exclude_files list of files to exclude. If "default", it will  not sync hidden files in parent as well as  folders in `renv`. If
#'    "none" will sync everything.
#' @param rsync_flags flag to use wit `rsync`.  Default is `-avtuP`. Check \href{https://ss64.com/bash/rsync_options.html}{rsync page}
#'     for the complete list of options.
#' @param  inter if to ask to confirm interactively the destination path
#' @param ... used internally
#' @details use `rsync` to sync and `here` to identify parental directory
#' @export
sync_lab <- function(
                     direction = "or_de",
                     exclude_files = "default",
                     rsync_flags = "-avtuP",
                     inter = TRUE,
                     ...) {


  # Check if rsync is installed and parameters
  rsync_installed <- system("rsync --version") == 0
  if (rsync_installed == FALSE) {
    stop("rsync installation not found! Please install rsync...")
  }

  if (!file.exists(here::here(".labor_destination"))) {
    stop("Run set_lab_sync() to setup the destination of the sync!")
  }

  if (!direction %in% c("or_de", "de_or", "bidir")) stop("Direction can only be `or_de`, `de_or`, `bidir`)!")


  # this is used for testthat so that origin is set in the test
  try(
    origin <- return_dots(...),
    silent = TRUE
  )

  # check if user wants to continue
  if (!exists("origin")) {
    origin <- paste0(here::here(), "/")
  }

  destination <- scan(here::here(".labor_destination"),
    comment.char = "#", what = "character", n = 1, quiet = TRUE
  )

  # Message
  mess <- paste0("Syncing ", origin, " to ", destination)
  sep_mess <- paste(rep("=", nchar(mess)), collapse = "")
  message(paste0(sep_mess, "\n", mess, "\n", sep_mess))

  if (inter == TRUE) {
    continue <- readline(prompt = "\nPress  c to cancel or anything else to continue... ")
    if (continue == "c") stop("Sync stopped!")
  }


  # file to exclude
  if (exclude_files == "default") {
    to_exlude_files <- c(".*", "renv/library/", "renv/python/", "renv/staging/")
  }

  if (!exclude_files %in% c("none", "default")) {
    to_exlude_files <- exclude_files
  }

  # here the rsync command is constructed
  if (exclude_files == "none") {
    to_exclude <- " "
  } else {
    to_exclude <- paste0("--exclude ", "\"", to_exlude_files, "\"", collapse = " ")
  }

  rsync_comand <- paste0("rsync ", rsync_flags)

  if (direction %in% c("or_de", "bidir")) {
    all_comand <- paste(rsync_comand, " ", to_exclude, " \"", origin, "\" ", "\"", destination, "\"", sep = "")
    system(all_comand)
  }

  if (direction %in% c("de_or", "bidir")) {
    all_comand <- paste(rsync_comand, " ", to_exclude, " \"", destination, "\" ", "\"", origin, "\"", sep = "")
    system(all_comand)
  }
}
