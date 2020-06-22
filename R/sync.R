#' choose_direction
#'
#' Use `svDialog` to choose is sync left to right or bidirectionally. Use internally
#'
#' @return an atomic vector `r` or `bidirectional`
choose_direction <- function(){

  r_dir <- svDialogs::dlg_list(list("left to right", "bidirectional"))$res

  if (r_dir == "left to right"){
    direction <- "r"
  } else {
    direction <- "bidirectional"
  }

  direction

}




#' sync project drop
#'
#' sync the project folder with dropbox folder
#'
#' @param direction one of `c("local_to_drop", "bd", "drop_to_local")
#' @param local path to origin folder (default is wd)
#' @param dropbox path to destination
#' @param inter if interaction is true choose the direction interactivelly
#' @details use rdrop
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
sync_project_drop <-function(direction = "local_to_drop", inter = "FALSE", local, dropbox_folder){
  rdrop2::drop_auth()

  if(inter == TRUE){
    direction <-  choose_direction()
  }

  local <-  paste0(path_check(local), "/")

  if(!drop_exists(dropbox_folder)){
    stop("Dropbox folder does not exist!")
  }

  if(length(grep("/$", dropbox)) == 0){
    dropbox <- paste0(dropbox, "/")
  }

  if(direction == "local_to_drop"){
    to_upload <- paste0(local, list.files(local))
    lapply(to_upload, rdrop2::drop_upload, path = dropbox_folder)
  }



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

  if(inter == TRUE){

    direction <- choose_direction()

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




