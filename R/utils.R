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


#' return dots
#'
#' Returns the dots argument. Used internally for testing set_sync_lab and sync_lab
#' @keywords internal
return_dots <- function(x) {
  x
}


#' Remove File
#'
#' Function that remove a file recursivelly and added it to .gitignore to avoid tracking of those files. Based on
#' \href{https://stackoverflow.com/questions/107701/how-can-i-remove-ds-store-files-from-a-git-repository}{this post.}:
#'
#' @export
#
remove_file <- function(file_name, path_to_look){

  system_cmd <- paste("find", path_to_look, "-name ", file_name, "-print0 | xargs -0 git rm -f --ignore-unmatch", sep = " ")

  system(system_cmd)

  files_here <- fs::dir_ls(path_to_look, type = "file", all = TRUE)

  if(!".gitignore" %in% files_here){
    fs::file_create(file.path(path_to_look, ".gitignore"))
  }

  git_ignore <- scan(here::here(".gitignore"), what = "character", quiet = TRUE)

  if(file_name %in% git_ignore){

    cli::cli_alert_info("File {.file {file_name}} already in .gitignore.")
  } else {

    cli::cli_alert_info("File {.file {file_name}} added to {.file .gitignore}.")
    cat(file_name, file = here::here(".gitignore"), append = TRUE, sep = "\n")
  }

  message(paste0("\n",file_name, "files removed!"))
  cli::cli_alert_info("File {.file {file_name}} recursevelly removed")


}



#' Retrieve file from package
#'
#' Find path of file in {labor} package.
#'
#' @param file_name
retrive_file_pkg <- function(file_name){

  file.path(.libPaths(), "labor", file_name)[1]

}

retrive_copy_files_pkg <- function(file_names, folder_out) {

  files_to_parent <- as.character(lapply(file_names, retrive_file_pkg))

  file.copy(from = files_to_parent, to = folder_out)


}


# https://github.com/r-lib/cli























