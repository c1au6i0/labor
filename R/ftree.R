#' folder tree
#'
#' The list of folders to create. It is used internally
#'
ftree <- function() {
  c(
    "/code",
    "/data",
    "/figs",
    "/manuscripts",
    "/objs",
    "/reports"
  )
}

#' check if path exists
#'
#' Check if the path exists.
#'
#' @param path the path of the folder to check
#' @return the `path` if exists, otherwise it throws an Error
path_check <- function(path) {
  # if slash end of path, remove it
  path <- sub("/$", "", path)

  # does folder exists
  if (dir.exists(path) == FALSE) {
    stop(paste0("The directory ", path," does not exist!"))
  }

  path

}

#' create a readme
#'
#' Create readme file. If argument `type` is `data` then the file is created
#' in `path/data/`.
#'
#' @param path where to create the folder
#' @param type one of `c("data", "ftree")`
#
create_readme <- function(path, type) {
  if (!type %in% c("data", "ftree")) {
    stop("Argument type can only be data or ftree!")
  }

  # this need to be changed.
  # I don like this (json file with 2 lists: data and description)
  # we dont need the other readme.

  path <- path_check(path)

  if (type == "data") {
    output <- paste0(path, "/data/README")
    sink(output)
    cat("Datasets:")
    sink()
  }

  if (type == "ftree") {
    output <- paste0(path, "/README")
    file_conn <- file(output)
    writeLines(
      c(
        "Directory Structure:",
        "code:",
        "  Source code for the analysis.",
        "data:",
        " Data for the project.",
        "data:",
        "  Raw Data",
        "figs:",
        "  Figures.",
        "objs:",
        "  Analysis results, including intermediate step analysis and simulated datasets.",
        "manuscripts:",
        "  Manuscripts",
        "reports:",
        "   Reports.",
        "",
        "Please add a brief explanation to this README file for any directory you may add"
      ),
      file_conn
    )
    close(file_conn)
  }
}

#' create or  overwrite existing readme
#'
#' Check if README.txt exists in parent and overwrite it if user agres.
#'
#' @param path the path of the parent folder to check
#' @param  type one of `c("data", "ftree")`
create_overwrite_readme <- function(path, type) {

  path <- path_check(path)

  if (!type %in% c("data", "ftree")) {
    stop("Argument type can only be data or ftree!")
  }

  if(type == "ftree") {

        message_out <- paste0("Readme in parent created!")
        if (file.exists(paste0(path, "/README.txt"))) {
          over_readme <- svDialogs::dlg_message("There is already a readme in parent folder!\nDo you want to overwrite it?", type = "yesno")$res
          if (over_readme == "yes") create_readme(path, "ftree")
          message(message_out)
        } else {
          create_readme(path, "ftree")
          message(message_out)
        }
  }

  if(type == "data"){

    path_check(paste0(path, "/data"))

    message_out <- paste0("Readme in /data created!")

      if (file.exists(paste0(path, "data/README.txt"))) {
        over_readme <- svDialogs::dlg_message("There is already a readme!\nDo you want to overwrite it?", type = "yesno")$res
        if (over_readme == "yes") create_readme(path, "data")
        message(message_out)
      } else {
        create_readme(path, "data")
        message(message_out)
      }
  }

}



#' create_ftree
#'
#' Create the folder tree and relative documentation files.
#'
#' @param path where to create the folder (default is `here()`)
#'
#' @export
create_ftree <- function(path = here::here()) {
  folders_t <- ftree()
  full_folders_t <- paste0(path, folders_t)

  # folder is not empty--------------------------------------------
  # ADD IF THERE IS A README ALONE
  already_there <- unlist(lapply(full_folders_t, dir.exists))
  if (sum(already_there) > 0) {

    # create new folders
    to_create <- full_folders_t[!already_there]


    lapply(to_create, dir.create)

    # if among the folder to write there is data we have to create a readme
    if (length(grep("*data$", to_create)) > 0) {
      create_readme(paste0(path), type = "data")
    }

    message("New folders created!\n")

    # Overwrite readme in parent
    create_overwrite_readme(path, "ftree")


    # choose the destiny of the others
    resp <- svDialogs::dlg_list(
      title = "The containing folder is not empty, which one do you want to overwrite?\n",
      multiple = TRUE,
      full_folders_t[already_there]
    )$res

    # do what you have been told
    if (length(resp) == 0) {
      stop("Nothing to do here then!")

      # Overwrite
    } else {
      unlink(resp, recursive = TRUE)
      lapply(resp, dir.create)

      # if among the folder to overwrite there is data we have to create a readme
      if (length(grep("*data$", resp)) > 0) {
        create_readme(path, type = "data")
      }

      message("Folders overwrote!\n")
    }
  } else {
    # Overwrite readme in parent
    create_overwrite_readme(path, "ftree")

    lapply(paste0(path, folders_t), dir.create)
    create_readme(path = path, type = "data")

    }

  message("Da da! All done!")
}


#' remove_ftree
#'
#' remove the folder tree and relative documentation files
#'
#' @param path where to create the folder (defaut is `getwd()`)
#'
#' @export
remove_ftree <- function(path = getwd()) {

  path <- path_check(path)

  folders_t <- ftree()
  full_folders_t <- paste0(path, folders_t)

  unlink(full_folders_t, recursive = TRUE)

  message("Folders removed!\n")
  unlink(paste0(path, "/README.txt"))

}



#' clean_up
#'
#' Check if folder and file are in the right place documentation is present and if not
#' cleans it up.
#'
#' @export
clean_up <- function() {


}
























