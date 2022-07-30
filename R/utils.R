#' check if path exists
#'
#' Check if the path exists.
#'
#' @param path the path of the folder to check
#' @return the `path` if exists, otherwise it throws an Error
#' @keywords internal
check_path <- function(path) {
  # if slash end of path, remove it
  path <- sub("/$", "", path)

  # does folder exists
  if (dir.exists(path) == FALSE) {
    stop(paste0("The directory ", path, " does not exist!"))
  }

  path
}


#' checks the content of a folder
#'
#' It checks the extension of files in a folder. It also count the unexpected files.
#'
#' @param path the path
#' @param ext a vector of expected extensions of expected files
#' @param verbose if TRUE displays some messages related to the check
#' @param out `all_files` for a list of all files,  `summary_files` for a summary or `misplaced_files` for a list of misplaced files
#' @return  all_files, missplaced files, or  dataframe summary
#' @importFrom rlang .data
#' @keywords internal
check_files_folder <- function(path, ext, verbose = TRUE, out = "summary_files") {
  #
  # browser()
  # path <- "/Users/heverz/Documents/R_projects/covid19_interference/code"
  files_code_raw <- data.frame(files = list.files(path, recursive = TRUE, full.names = TRUE))
  files_code <- tidyr::extract(files_code_raw, col = .data$files, into = c("file", "extension"), regex = "(.+)\\.([[:alnum:]]+)")

  files_code$extension <- tolower(files_code$extension)


  all_files_mis <- dplyr::mutate(files_code, tidyness = dplyr::if_else(.data$extension %in% !!ext, "ok", "misplaced"))
  all_files <- as.data.frame(dplyr::mutate(all_files_mis, tidyness = factor(.data$tidyness, levels = c("ok", "misplaced"))))


  misplaced_files <- all_files[all_files$tidyness == "misplaced", ]


  summary_files_gp <- dplyr::group_by(all_files, .data$tidyness, .drop = FALSE)
  summary_files_sm <- dplyr::summarize(summary_files_gp, n = dplyr::n())
  summary_files <- tidyr::pivot_wider(summary_files_sm, names_from = .data$tidyness, values_from = .data$n)

  summary_files[, "total"] <- summary_files$misplaced + summary_files$ok
  summary_files[, "tidyindex"] <- as.integer(summary_files$ok / summary_files$total * 100)
  summary_files[, "folder"] <- regmatches(path, regexpr("[A-z0-9]+$", path))

  summary_files <- summary_files[, c("folder", "ok", "misplaced", "total", "tidyindex")]

  if (verbose) {
    cat("Analyzing folder --", summary_files$folder, "-- ...", "\n")
  }

  get(out)
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
#' Function that remove file and added it to .gitignore
#'
#' Remove `.DS_Store` files and modify .git_ignore to avoid tracking of those files. Based on
#' \href{https://stackoverflow.com/questions/107701/how-can-i-remove-ds-store-files-from-a-git-repository}{this post.}:
#'
#' @export
#
remove_file <- function(file_name){

  system("find . -name ", file_name, " -print0 | xargs -0 git rm -f --ignore-unmatch")

  files_here <- list.files(here::here(), all.files = TRUE)

  if(!".gitignore" %in% files_here){
    stop("Initiate git launching git init in the terminal!")
  }

  git_ignore <- scan(here::here(".gitignore"), what = "character", quiet = TRUE)

  if(file_name %in% git_ignore){
    message(paste0(file_name, " already in .gitignore!"))
  } else {
    message(".gitignore has been updated!")
    cat(file_name, file = here::here(".gitignore"), append = TRUE, sep = "\n")
  }

  message(paste0("\n",file_name, "files removed!"))

}




























