#' sync_proj
#'
#' sync the project folder with a local folder
#'
#' @param direction one of `c("l", "r", "bd")`
#' @param inter boolean interactive choosing
#' @param cloud if TRUE use `rdrop` else sync for local
#' @param origin path to origin folder (default is wd)
#' @param destination path to destination
#' @details use rsync or rdrop
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
sync_proj <- function() {

  # rjson
  rsync_installed <- system("rsync --version") == 0
  if (rsync_installed == FALSE) {
    error("rsync installation not found! Please install rsync...")
  }
  # https://phoenixnap.com/kb/rsync-exclude-files-and-directories

  # if
  # # svDialogs::dlg_dir("")$res

}



#' sync_proj_local
#'
#' sync the project folder with a local folder
#'
#' @param direction one of `c("l_to_r", "bd")`
#' @param inter boolean interactive choosing
#' @param origin path to origin folder (default is wd)
#' @param destination path to destination
#' @details use rsync


sync_proj_loc <- function(direction = "r", inter = "FALSE", origin = "man/", destination = "~/Desktop/man") {

  rsync_installed <- system("rsync --version") == 0
  if (rsync_installed == FALSE) {
    error("rsync installation not found! Please install rsync...")
  }

  if(inter = TRUE){
    r_dir <- svDialogs::dlg_list(list("left to right", "bidirectional"))$res

    if (r_dir == "left to right"){
      direction <- "r"
    } else {
      direction <- "bidirectional"
    }

    origin <- svDialogs::dlg_dir("Select Origin")$res
    destination <- svDialogs::dlg_dir("Select Destination")$res
  }

  if(origin == destination) stop("Origin and destination can not be the same!")

  origin <-  paste0(path_check(origin), "/")
  destination <-paste0(path_check(destination), "/")

  rsync_comand <- "rsync -avtuP"
  all_comand <-  paste(rsync_comand, origin, destination, sep = " ")
  system(all_comand)
  message("Sync left to right performed!")

  if (direction == "l_to_r") {
    all_comand <-  paste(rsync_comand, destination, origin, sep = " ")
    system(all_comand)
    message("Sync right to left performed!")
  }


  # https://www.digitalocean.com/community/tutorials/how-to-use-rsync-to-sync-local-and-remote-directories-on-a-vps
  # https://phoenixnap.com/kb/rsync-exclude-files-and-directories


}






#' clean_up
#'
#' Check if folder and file are in the right place documentation is present and if not
#' cleans it up.
#'
#' @export
clean_up <- function() {


}






#' who_wants_to_talk
#'
#' @param lab one of `marchionni", "wheelan", "joint`
#'
#' @return a message

who_wants_to_talk <- function(lab = "marchionni") {
  if (!lab %in% c("marchionni", "wheelan", "joint")) stop("lab can only be marchionni, wheelan
                                                         , joint!")

  marchionni_lab <- c("Luigi", "Claudio", "Eddie", "Mohamed", "Wikum")
  wheelan_lab <- c("Sara", "McKinzie", "Heater", "Lauren", "Alyza")

  if (lab == "marchionni") members <- marchionni_lab
  if (lab == "wheelan") members <- wheelan_lab
  if (lab == "joint") members <- append(marchionni_lab, wheelan_lab)

  cat("Here the order:\n", paste0("[", seq(1, length(members)), "]: ", members[sample(length(members))], "\n"))
}
