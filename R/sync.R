#' choose_direction
#'
#' Use `svDialog` to choose if sync left to right or bidirectionally. Use internally
#'
#' @param drop boolean, dropbox
#' @return an atomic vector `r` or `bidirectional`
choose_direction <- function(drop = FALSE){
  if(!drop){
    choose_options <- list("left_to_right", "bidirectional")
  } else {
    choose_options <- list("local_to_drop", "drop_to_local", "bidirectional")
  }
  direction <- svDialogs::dlg_list(choose_options )$res
  direction
}


#' sync_proj_local
#'
#' sync the project folder with a local folder
#'
#' @param direction one of `c("left_to_right", "bidirectional")`
#' @param inter boolean interactive choosing
#' @param origin path to origin folder (default is wd)
#' @param destination path to destination
#' @details use rsync
#' @export
sync_proj_loc <- function(direction = "r", inter = TRUE, origin = "man/", destination = "~/Desktop/man") {


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

  if (direction == "bidirectional") {
    all_comand <-  paste(rsync_comand, destination, origin, sep = " ")
    system(all_comand)
    message("Sync right to left performed!")
  }

  # https://www.digitalocean.com/community/tutorials/how-to-use-rsync-to-sync-local-and-remote-directories-on-a-vps
  # https://phoenixnap.com/kb/rsync-exclude-files-and-directories

}





#' sync project drop
#'
#' sync the project folder with dropbox folder. Currently rdrop2 does not support uploading entire folders.
#' so forget this
#'
#' @param direction one of `c("local_to_drop", "bd", "drop_to_local")
#' @param local path to origin folder (default is wd)
#' @param dropbox_folder `path_lower` of dropbox as returned by `drop_dir()`
#' @param inter if interaction is true choose the direction interactively
#' @details use rdrop
sync_project_drop <- function(direction = "local_to_drop",
                              inter = "FALSE",
                              local = "~/Desktop/prova",
                              dropbox_folder = "/COVID19/Interference/reports"){
  rdrop2::drop_auth()

  if(inter == TRUE){
    direction <-  choose_direction()
  }

  local <-  paste0(path_check(local), "/")

  if(!drop_exists(dropbox_folder)){
    stop("Dropbox folder does not exist!")
  }


  if(length(grep("/$", dropbox_folder)) == 0){
    dropbox_folder <- paste0(dropbox_folder, "/")
  }

  if(direction %in% c("local_to_drop", "bidirectional")){
    to_upload <- paste0(local, list.files(local))
    lapply(to_upload, rdrop2::drop_upload, path = dropbox_folder)
  }

  if(direction  %in% c("drop_to_local","bidirectional")){
    to_download <-drop_dir(dropbox_folder)
    lapply(to_download$path_lower, rdrop2::drop_download, local_path = local)
  }
}





#' clean_up
#'
#' Check if folder and file are in the right place documentation is present and if not
#' cleans it up.
#'
clean_up <- function() {


}




