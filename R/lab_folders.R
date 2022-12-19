#' Folder tree.
#'
#' The list of folders to create. It is used internally.
#'
#' @keywords internal
labtree <- function() {
  c(
    "code",
    "data",
    "documents",
    "figs_tabs",
    "manuscripts",
    "misc",
    "objs",
    "reports"
  )
}



#' Create labtree.
#'
#' Create the folder tree and relative documentation files. If folders are already present, it will ask
#' which one to overwrite. It use the library `svDialogs` as interactive gui.
#'
#' @param path Where to create the folder (default is `here()`).
#' @param interative Boolean. If folder of the project is not empty, and interactive is `TRUE`, it will open `svDialogs` to
#' ask user to choose folders to overwrite.
#' @export
#' @keywords internal
create_labtree <- function(path_project = here::here(), interactive = TRUE) {

  check_path(path_project)
  folders_t <- labtree()
  full_folders_t <- file.path(path_project, folders_t)

  # folder is not empty--------------------------------------------
  already_there <- unlist(lapply(full_folders_t, dir.exists))
  if (any(already_there)) {
    # create new folders

    if (!interactive) stop("The folder is not empty!")

    to_create <- full_folders_t[!already_there]


    lapply(to_create, dir.create)

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


      message("Folders overwrote!\n")
    }
  } else {

    lapply(file.path(path_project, folders_t), dir.create)
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
remove_labtree <- function(project_oath = here::here()) {
  project <- check_path(project_path)

  folders_t <- labtree()
  full_folders_t <- paste0(project_path, folders_t)
  unlink(full_folders_t, recursive = TRUE)

  message("Folders and readme removed!\n")
}




#' labtree wizard.
#' #'
#' #' Choose folders to use in the labtree.
#'
#' #' @param path path
#' setup_labtree <- function(project_path = here::here()) {
#'   # we add data by default.
#'
#'   # do you want to use the defaul labtree?
#'
#'   folders_user <- svDialogs::dlg_input("Insert the name of folders separated by comma and without quotes")$res
#'   folders_stripted <- unlist(strsplit(folders_user, ","))
#'   folders_cl <- unique(c(gsub("[[:punct:]]|\\s", "", folders_stripted), "data"))
#' }




#' set up project
#'
#' Install some packages commonly used, initiate `renv` and create the project structure.
#'
#' @param use_targets If FALSE, do not use targets. If "local" install targets packages, add `_target.R` and `function.R` files
#'  , and update `README.Rmd`. If "cluster" add other config files for use with `slurm` and update `README.Rmd` accordingly.
#' @param path_project
#' @param pkg_to_install A vector of packages to install.
#' @param proj_name If `use_targets = "cluster"`, name of the project. Used to rename file.Rprj
#' @param use_python If TRUE launch `renv::use_python()`
#' @export
setup_lab_project <- function(use_targets = FALSE,
                              path_project = here::here(),
                              pkg_to_install = c("devtools", "here", "reticulate", "tidyverse", "usethis"),
                              files_git_rm = c(".DS_Store", "._.DS_Store", "._.*"),
                              proj_name,
                              use_python = TRUE
                              ) {

  # cli::cli_h1("Setting up renv...")

  install.packages("renv")
  renv::init(bioconductor = TRUE)

  if(use_python) renv::use_python()
  # cli_alert_success("renv installed an ready")

  # cli::cli_h1("Installing packages...")
  renv::install(c("devtools", "here", "reticulate", "tidyverse", "usethis"))


  lapply(files_git_rm, remove_file)

  create_labtree()

  if (use_targets %in% c("local", "cluster")) {
    targets::tar_script()

    dir.create(here::here("code", "targets_functions"))

    retrive_copy_files_package(c("README.Rmd", "_targets.R"), folder_out )
    files_to_parent <- lapply(c("README.Rmd", "_targets.R"), retrive_file_package)

    file.copy(from = files_to_parent, to = here::here())

    file.copy(
      from = retrive_file_package("functions.R"),
      to = here::here("code", "targets_functions")
    )
  }

  if (use_targets == "cluster") {

    lapply(c("README.Rmd", "_targets.R"), unlink)

    files_to_copy <- c(
      "_start_targets.sh",
      "_targets_resources.conf.R",
      "_targets_slurm.R",
      "batchtools.slurm.tmpl",
      "project_name.Rproj",
      "README_cluster_target.Rmd"
      )


    retrive_copy_files_pkg(file_names = files_to_copy, folder_out = path_project)

    before_rename <- fs::path(path_folder, c("README_cluster_target.Rmd", "_targets_resources.conf.R", "project_name.Rproj"))
    after_rename <- fs::path(path_folder, c("README.Rmd", "_targets.R", paste0(proj_name, ".Rproj")))

    fs::file_move(before_rename, after_rename)



  }
}
