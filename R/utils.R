#' check if path exists
#'
#' Check if the path exists.
#'
#' @param path the path of the folder to check
#' @return the `path` if exists, otherwise it throws an Error
check_path <- function(path) {
  # if slash end of path, remove it
  path <- sub("/$", "", path)

  # does folder exists
  if (dir.exists(path) == FALSE) {
    stop(paste0("The directory ", path," does not exist!"))
  }

  path

}


#' checks the content of a folder
#'
#' It checks the extension of files in a folder. It also count the unexpected files.
#'
#' @param path
#' @param ext a vector of expected extensions of expected files
#'
#' @return list with all_files, missplaced files, and a dataframe summary
check_files_folder <- function(path, ext){

  files_code <-  data.frame(files =  list.files(path, recursive = TRUE)) %>%
    tidyr::separate(.data$files, sep = "\\.", into = c("file", "extension"), fill = "right")

  all_files <- files_code %>%
    dplyr::mutate(tidyness = dplyr::if_else(extension %in% !!ext, "OK", "misplaced"))

  misplaced_files <- all_files[all_files$tidyness == "misplaced", ]

  summary_files <-  all_files %>%
    dplyr::group_by(.data$extension, .data$tidyness) %>%
    dplyr::summarize(n = dplyr::n())

 list(all_files = all_files, misplaced_files = misplaced_files, summary_files = summary_files)
}










































