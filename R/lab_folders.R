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

  path <- check_path(path)

  if (type == "data") {
    path <- check_path(paste0(path, "/data"))
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

  path <- check_path(path)

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

  path <- check_path(path)

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
#' @importFrom magrittr %>%
check_lab <- function(path = here::here()) {
  browser()

  path <- "/Users/heverz/Documents/R_projects/covid19_interference"

  path <- check_path(path)

  # check if all folders are OK in parental
  folders_lab <- list.dirs(path, recursive = FALSE, full.names = FALSE)
  folders_lab <- grep("^[^.]", folders_lab, value = TRUE)
  expected_lab <- c(sub("/", "", labtree()), "renv")

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Check folders in parent -------
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  folders_excess <- folders_lab[!folders_lab %in% expected_lab]
  folders_missing <- expected_lab[!expected_lab %in% folders_lab]

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Check files in parents no hidden ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  files_folders_parent <- list.files(path, all.files = FALSE, include.dirs = FALSE)
  files_parent <- files_folders_parent[!files_folders_parent %in% folders_lab]

  # If files are Rproj or renv_lock thats fine
  files_parent_excess <-  files_parent[!files_parent %in% grep("Rproj$|lock", files_parent, perl = TRUE, value = TRUE)]

  # @@@@@@@@@@@@@@@@@@@@@@@
  # Check other folders ----
  # @@@@@@@@@@@@@@@@@@@@@@@
  folder_to_check <- expected_lab[!expected_lab %in% folders_missing]





}


# "/code",
# "/data",
# "/documents",
# "/figs",
# "/manuscripts",
# "/misc",
# "/objs",
# "/reports"
# )


code_lab <-   c("R", "RMD", "RMarkdown", "txt", "md")
data_lab <- c("rda", "rds", "csv", "xls", "xlsm", "xlsx", "txt", "md")
documents_lab <- c("pdf", "html", "doc", "docm", "docx", "txt", "md")
figs_lab <-   c("html", "png", "pdf", "tex", "log", "aux", "txt", "md")
manuscripts_lab <- unique(unlist(c(documents_lab, figs_lab, "RMD", "RMarkdown")))
objs_lab <- data_lab
reports_lab <- manuscripts_lab

max_n  <- length(manuscripts_lab)

folder_ext <- data.frame()[1:max_n, ]

for(x in c(code_lab, documents_lab, figs_lab, manuscripts_lab, objs_lab, reports_lab)){

  folder_ext <- cbind(x[seq(1:max_n)], folder_ext)

}



























