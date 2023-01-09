#' Folder tree.
#'
#' The list of folders to create. It is used internally.
#'
#' @keywords internal
labtree <- function() {
  c(
    "code",
    file.path("data", "raw"),
    file.path("data", "other"),
    file.path("reports", "tabs_figs"),
    "other"
  )
}



#' Create labtree.
#'
#' Create the folder tree and relative documentation files. If folders are already present, it will ask
#' which one to overwrite.
#'
#' @param path Where to create the folder (default is `here()`).
#' @export
#' @keywords internal
create_labtree <- function(path_project = here::here()) {

  check_path(path_project)
  folders_t <- labtree()
  full_folders_t <- file.path(path_project, folders_t)

  # folder is not empty--------------------------------------------
  already_there <- unlist(lapply(full_folders_t, dir.exists))
  if (any(already_there)) {
    # create new folders

    if (!interactive) stop("The folder is not empty!")

    to_create <- full_folders_t[!already_there]

    fs::dir_create(to_create)

    # choose the destiny of the others
    resp <- utils::select.list(
      title = "The containing folder is not empty, which one do you want to overwrite?\n",
      multiple = TRUE,
      full_folders_t[already_there]
    )

    # do what you have been told
    if (length(resp) == 0) {
      cli::cli_alert_info("Nothing to do here then.")

      # Overwrite
    } else {
      unlink(resp, recursive = TRUE)
      fs::dir_create(resp)
      cli::cli_alert_success("Folders overwrote.")
    }
  } else {

    fs::dir_create(file.path(path_project, folders_t))
  }

  cli::cli_alert_success("Lab folder tree created.")
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

  cli::cli_alert_success("Folders removed.")
}


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
#' @param use_git If TRUE install git
#' @export
setup_lab_project <- function(
                              path_project = here::here(),
                              use_targets = FALSE,
                              pkg_to_install = c("BiocManager","devtools", "here", "git2r","lintr", "languageserver", "renv", "targets", "tidyverse", "usethis"),
                              files_git_rm = c(".DS_Store", "._.DS_Store", "._.*"),
                              use_python = TRUE,
                              use_git = TRUE
                              ) {


  check_path(path_project)

  create_labtree(path_project = path_project)

  cli::cli_h1("Copying files...")
  if (use_targets %in% c("local", "cluster")) {

    fs::dir_create(file.path(path_project,"code", "targets_functions"))
    retrive_copy_files_pkg(c("README.Rmd", "_targets.R"), folder_out = path_project)
    retrive_copy_files_pkg("functions.R", fs::path(path_project, "code", "targets_functions"))

  }

  if (use_targets == "cluster") {

    lapply(c("README.Rmd", "_targets.R"), unlink)

    files_to_copy <- c(
      "_start_targets.sh",
      "_targets_resources.conf.R",
      "_targets_slurm.R",
      "batchtools.slurm.tmpl",
      "README_cluster_target.Rmd"
    )

    retrive_copy_files_pkg(file_names = files_to_copy, folder_out = path_project)
    before_rename <- fs::path(path_project, c("README_cluster_target.Rmd", "_targets_resources.conf.R"))
    after_rename <- fs::path(path_project, c("README.Rmd", "_targets.R"))
    fs::file_move(before_rename, after_rename)
  }

  # cli::cli_h1("Setting up renv...")
  usethis::create_project(path = path_project, open = FALSE)
  usethis::with_project(
    path = path_project,
    {
      install.packages(c("renv", "BiocManager"))
      remotes::install_github("c1au6i0/labor@dev")
      renv::install(project = path_project, packages =  pkg_to_install, prompt = FALSE)
      renv::init(project = path_project, bioconductor = TRUE, restart = FALSE)
      renv::activate(project = path_project)
      renv::snapshot(project = path_project)
    }
  )

  # cli::cli_h1("Setting up git...")
  if (use_git) {
        git2r::init(path_project)
        lapply(files_git_rm, remove_file, path_to_look = path_project) # remove annoying files

  }

 # cli::cli_h1("Setting up git...")
  if(use_python) {

    usethis::with_project(
      path = path_project,

    # https://stackoverflow.com/questions/51585149/cant-figure-out-how-to-use-conda-environment-after-reticulateuse-condaenvpat
    {
      renv::install("reticulate")
      renv::use_python(project = path_project, type = "virtualenv")
      reticulate::conda_install(envname = basename(path_project), packages = "mamba")
    }
    )

    # https://rstudio.github.io/reticulate/reference/conda-tools.html
    # https://github.com/rstudio/reticulate/issues/1196
    # options(reticulate.conda_binary = Sys.which("mamba"))
    # # getOption("reticulate.conda_binary")
  }

}
