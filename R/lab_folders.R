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
create_labtree <- function(path_project) {

  check_path(path_project)
  folders_t <- labtree()
  full_folders_t <- file.path(path_project, folders_t)

  # folder is not empty--------------------------------------------
  already_there <- unlist(lapply(full_folders_t, dir.exists))
  if (any(already_there)) {
    # create new folders

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
#' @param project_path where to create the folder
#'
#' @export
remove_labtree <- function(project_path) {
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
#' @param path_project Where to setup the project.
#' @param use_targets If FALSE, do not use targets. If "local" install targets packages, add `_target.R` and `function.R` files
#'  , and update `README.Rmd`. If "cluster" add other config files for use with `slurm` and update `README.Rmd` accordingly.
#' @param pkg_to_install A vector of packages to install.
#' @param files_git_rm A vector of files name to remove.
#' @param use_git If TRUE install git
#' @export
setup_lab_project <- function(
                              path_project,
                              use_targets = FALSE,
                              pkg_to_install = c("BiocManager",
                                                 "devtools",
                                                 "here",
                                                 "lintr",
                                                 "languageserver",
                                                 "renv",
                                                 "targets",
                                                 "tidyverse",
                                                 "usethis"),
                              # use_python = TRUE,
                              use_git = TRUE
                              ) {


  check_path(path_project)

  create_labtree(path_project = path_project)

  cli::cli_h1("Copying files...")
  if (use_targets %in% c("local", "cluster")) {

    fs::dir_create(file.path(path_project,"code", "targets_functions"))
    retrive_copy_files_pkg("other.tar.gz", folder_out = path_project)
    utils::untar(tarfile = file.path(path_project, "other.tar.gz"), exdir = path_project)
    fs::file_delete(file.path(path_project, "other.tar.gz"))

  }

  if (use_targets == "cluster") {

    fs::dir_create(file.path(path_project, "log"))

    fs::file_delete(
      c(file.path(path_project, "README.Rmd"),
                      file.path(path_project, "_targets.R"))
    )


    retrive_copy_files_pkg("cluster.tar.gz", folder_out = path_project)
    utils::untar(tarfile = file.path(path_project, "cluster.tar.gz"), exdir = path_project)
    fs::file_delete(file.path(path_project, "cluster.tar.gz"))

    before_rename <- fs::path(path_project, c("README_cluster_target.Rmd", "_targets_slurm.R"))
    after_rename <- fs::path(path_project, c("README.Rmd", "_targets.R"))
    fs::file_move(before_rename, after_rename)


  }

  # cli::cli_h1("Setting up renv...")
  usethis::create_project(path = path_project, open = FALSE)
  usethis::with_project(
    path = path_project,
    {
      utils::install.packages(c("renv", "BiocManager"))
      remotes::install_github("c1au6i0/labor@dev")
      renv::install(project = path_project, packages =  pkg_to_install, prompt = FALSE)
      renv::init(project = path_project, bioconductor = TRUE, restart = FALSE)
      renv::activate(project = path_project)
      renv::snapshot(project = path_project)
      renv::install(project = path_project, "~/.vim/plugged/Nvim-R/R/nvimcom")
    }
  )


  # cli::cli_h1("Setting up git...")
  if (use_git) {
    usethis::with_project(
        path = path_project,
        {
          renv::install(project = path_project, "git2r")
          git2r::init(path_project)
          usethis::git_vaccinate()
        }
    )

  }

 # # cli::cli_h1("Setting up git...")
 #  if(use_python) {
 #
 #    usethis::with_project(
 #      path = path_project,
 #
 #    # https://stackoverflow.com/questions/51585149/cant-figure-out-how-to-use-conda-environment-after-reticulateuse-condaenvpat
 #    {
 #      renv::install("reticulate")
 #      renv::use_python(project = path_project, type = "virtualenv")
 #      reticulate::conda_install(envname = basename(path_project), packages = "mamba")
 #    }
 #    )

  # }


}
