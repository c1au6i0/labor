#' Folder tree.
#'
#' The list of folders to create. It is used internally.
#'
#' @keywords internal
labtree <- function() {
  c(
    "/code",
    "/data",
    "/documents",
    "/figs_tabs",
    "/manuscripts",
    "/misc",
    "/objs",
    "/reports"
  )
}



#' Create a readme.
#'
#' Create readme file. If argument `type` is `data` then the file is created
#' in `path/data/`.
#'
#' @param path where to create the folder
#' @param type one of `c("data", "labtree")`
#' @keywords internal
create_readme <- function(path, type) {
  if (!type %in% c("data", "labtree")) {
    stop("Argument type can only be data or labtree!")
  }


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

#' Create or  overwrite existing readme.
#'
#' Check if README.txt exists in parent and overwrite it if user agres.
#'
#' @param path the path of the parent folder to check
#' @param  type one of `c("data", "labtree")`
#' @keywords internal
create_overwrite_readme <- function(path, type) {
  path <- check_path(path)

  if (!type %in% c("data", "labtree")) {
    stop("Argument type can only be data or labtree!")
  }

  if (type == "labtree") {
    if (file.exists(paste0(path, "/README"))) {
      over_readme <- svDialogs::dlg_message("There is already a readme in parent folder!\nDo you want to overwrite it?", type = "yesno")$res
      if (over_readme == "yes") create_readme(path, "labtree")
    } else {
      create_readme(path, "labtree")
    }
  }

  if (type == "data") {
    if (file.exists(paste0(path, "data/README"))) {
      over_readme <- svDialogs::dlg_message("There is already a readme!\nDo you want to overwrite it?", type = "yesno")$res
      if (over_readme == "yes") create_readme(path, "data")
    } else {
      create_readme(path, "data")
    }
  }
}


#' Create labtree.
#'
#' Create the folder tree and relative documentation files. If folders are already present, it will ask
#' which one to overwrite. It use the library `svDialogs` as interactive gui.
#'
#' @param path where to create the folder (default is `here()`)
#' @export
#' @keywords internal
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


#' Remove labtree.
#'
#' Femove the folder tree and relative documentation files.
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


#' Check Lab Folder.
#'
#' Check if folder and file are in the right place.
#'
#' @param path where to check (defaut is `here()`)
#' @importFrom stats na.omit
#' @export
check_lab <- function(path = here::here()) {
  path <- check_path(path)

  # check if all folders are OK in parental
  folders_lab <- list.dirs(path, recursive = FALSE, full.names = FALSE)
  folders_lab <- grep("^[^.]", folders_lab, value = TRUE)
  expected_lab <- c(sub("/", "", labtree()), "renv")

  # Check folders in parent -------
  folders_excess <- folders_lab[!folders_lab %in% expected_lab]
  folders_missing <- expected_lab[!expected_lab %in% folders_lab]

  message("Analyzing labtree ---------------------------\n")
  summary_folders <- data.frame(
    "folders" = c("excess", "missing"),
    "tot" = unlist(lapply(list(folders_excess, folders_missing), length))
  )

  message("\nSummary of folders:")
  print(knitr::kable(summary_folders, align = "c", row.names = FALSE))

  if (length(folders_excess) > 0) {
    print(knitr::kable(as.data.frame(folders_excess), align = "c", row.names = FALSE))
  }

  if (length(folders_excess) > 0) {
    print(knitr::kable(as.data.frame(folders_missing), align = "c", row.names = FALSE))
  }


  # Check files in parents no hidden ------------
  files_folders_parent <- list.files(path, all.files = FALSE, include.dirs = FALSE)
  files_parent <- files_folders_parent[!files_folders_parent %in% folders_lab]

  # If files are Rproj or renv_lock thats fine
  files_parent_excess <- files_parent[!files_parent %in% grep("Rproj$|lock", files_parent, perl = TRUE, value = TRUE)]

  # misc by definition can have everything
  folders_to_check <- expected_lab[!expected_lab %in% c(folders_missing, "renv", "misc")]

  # check_files_folder(file.path(path, folders_to_check[1]), folder_ext[, names(folder_ext) %in% c("code")])

  if (length(folders_to_check) == 0) {
    message("There are no folder that can be checked! Run create_labtree to create folders!")
  }

  message("\nAnalyzing  files in labtree -----------------\n")
  if (length(folders_to_check) == 0) {
    message("There are no folder that can be checked! Run create_labtree to create folders!")
  }

  if (length(folders_to_check) == 1) {
    all_summary <- check_files_folder(file.path(path, folders_to_check), folder_ext[, names(folder_ext) %in% folders_to_check])
    misplace_files <- check_files_folder(
      file.path(path, folders_to_check),
      folder_ext[, names(folder_ext) %in% folders_to_check],
      out = "misplaced_files",
      verbose = FALSE
    )
  }

  if (length(folders_to_check) > 1) {
    all_summary_list <- as.data.frame(mapply(
      check_files_folder,
      file.path(path, folders_to_check),
      folder_ext[, names(folder_ext) %in% folders_to_check]
    ))

    all_summary <- do.call(rbind, all_summary_list)

    misplace_files_list <- mapply(check_files_folder,
      file.path(path, folders_to_check),
      folder_ext[, names(folder_ext) %in% folders_to_check],
      out = "misplaced_files",
      SIMPLIFY = FALSE
    )

    misplace_files <- do.call(rbind, misplace_files_list)
    misplace_files <- na.omit(misplace_files)
  }

  message("\nSummary of files:")
  print(knitr::kable(all_summary, row.names = FALSE, align = "c"))

  if (nrow(misplace_files) > 0) {
    message("\nMisplaced files:")
    print(knitr::kable(misplace_files, align = c("l", "c", "c"), row.names = FALSE))
  } else {
    cat("\nVery good! No misplaced files!")
  }
}


#' Labtree wizard.
#'
#' Choose folders to use in the labtree.

#' @param path path
setup_labtree <- function(path = here::here()) {

  # we add data by default.

  # do you want to use the defaul labtree?

  folders_user <- svDialogs::dlg_input("Insert the name of folders separated by comma and without quotes")$res
  folders_stripted <- unlist(strsplit(folders_user, ","))
  folders_cl <- unique(c(gsub("[[:punct:]]|\\s", "", folders_stripted), "data"))
}




#' set up project
#'
#' Install some packages commonly used and create the project structure.
#'
#' @param use_targets If TRUE creates target scripts and
#' @export
setup_lab_project <- function(use_targets = FALSE){
  pak::pkg_install(pkg = c(
                           "devtools",
                           "here",
                           "pak",
                           "renv",
                           "targets",
                           "tidyverse",
                           "usethis")
                   )
  remove_DS_Store()
  create_labtree()
  unlink(here::here("README"))
  unlink(here::here("code"," README"))

  if(use_targets){
      targets::tar_script()
      dir.create(here::here("code", "targets_functions"))
  }


}
