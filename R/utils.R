#' check if path exists
#'
#' Check if the path exists, if not it creates it.
#'
#' @param path the path of the folder to check
#' @return the `path` if exists, otherwise it throws an error
#' @keywords internal
check_path <- function(path) {

  # does folder exists
  if (!fs::file_exists(path)) {
    fs::dir_create(path)
  }
  cli::cli_alert_info("Folder {.file {path}} created.")
  path
}



#' Retrieve file and copy it
#'
#' Find file in {labor} package and copy it.
#'
#' @param file_names Name of the files.
#' @param folder_out Where to copy it.

retrive_copy_files_pkg <- function(file_names, folder_out) {

  files_to_parent <- as.character(lapply(file_names, fs::path_package))

  fs::file_copy(path = files_to_parent, new_path = folder_out)
}

























