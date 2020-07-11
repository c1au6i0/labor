#' folder tree
#'
#' The list of folders to create. It is used internally
#'
labtree <- function() {
  c(
    "/code",
    "/data",
    "/documents",
    "/figs",
    "/manuscripts",
    "/misc",
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
#' @param type one of `c("data", "labtree")`
create_readme <- function(path, type) {
  if (!type %in% c("data", "labtree")) {
    stop("Argument type can only be data or labtree!")
  }

  # this need to be changed.
  # I don like this (json file with 2 lists: data and description)
  # we dont need the other readme.

  path <- path_check(path)

  if (type == "data") {
    path <- path_check(paste0(path, "/data"))
    output <- paste0(path, "/README")
    sink(output)
    cat("Datasets:")
    sink()
    message("Readme created in data directory!")
  }

  if (type == "labtree") {
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
        "documents:",
        "  External documents like papers, tutorial.",
        "figs:",
        "  Figures.",
        "objs:",
        "  RDA and RDS files of analyses, and intermediate objects",
        "manuscripts:",
        "  Manuscripts",
        "misc:",
        "  Anything that does not fit in the other categories",
        "reports:",
        "   Reports, text files, markdown, ppt.",
        "",
        "Please add a brief explanation to this README file for any directory you may add"
      ),
      file_conn
    )
    close(file_conn)
    message("Readme created in parental directory!")
  }
}

#' create or  overwrite existing readme
#'
#' Check if README.txt exists in parent and overwrite it if user agres.
#'
#' @param path the path of the parent folder to check
#' @param  type one of `c("data", "labtree")`
create_overwrite_readme <- function(path, type) {

  path <- path_check(path)

  if (!type %in% c("data", "labtree")) {
    stop("Argument type can only be data or labtree!")
  }

  if(type == "labtree") {
        if (file.exists(paste0(path, "/README"))) {
          over_readme <- svDialogs::dlg_message("There is already a readme in parent folder!\nDo you want to overwrite it?", type = "yesno")$res
          if (over_readme == "yes") create_readme(path, "labtree")
        } else {
          create_readme(path, "labtree")
        }
  }

  if(type == "data"){
      if (file.exists(paste0(path, "data/README"))) {
        over_readme <- svDialogs::dlg_message("There is already a readme!\nDo you want to overwrite it?", type = "yesno")$res
        if (over_readme == "yes") create_readme(path, "data")
      } else {
        create_readme(path, "data")
      }
  }

}


#' create_labtree
#'
#' Create the folder tree and relative documentation files. If folders are already present, it will ask
#' which one to overwrite. It use the library `svDialogs` as interactive gui
#'
#' @param path where to create the folder (default is `here()`)
#'
#' @export
create_labtree <- function(path = here::here()) {
  folders_t <- labtree()
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
    create_overwrite_readme(path, "labtree")


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
    create_overwrite_readme(path, "labtree")

    lapply(paste0(path, folders_t), dir.create)
    create_readme(path = path, type = "data")

    }

  message("Da da! All done!")
}


#' remove_labtree
#'
#' remove the folder tree and relative documentation files
#'
#' @param path where to create the folder (defaut is `here()`)
#'
#' @export
remove_labtree <- function(path = here::here()) {

  path <- path_check(path)

  folders_t <- labtree()
  full_folders_t <- paste0(path, folders_t)
  unlink(full_folders_t, recursive = TRUE)

  unlink(paste0(path, "/README"))

  message("Folders and readme removed!\n")

}


#' clean_up
#'
#' Check if folder and file are in the right place documentation is present and if not
#' cleans it up.
#'
check_lab <- function(path = here::here()) {
  browser()

  # check if all folder are OK
  folders_lab <- list.dirs(path, recursive = FALSE, full.names = FALSE)
  folders_lab <- grep("^[^.]", folders_lab, value = TRUE)

  expected_lab <- sub("/", "", labtree())


}





























