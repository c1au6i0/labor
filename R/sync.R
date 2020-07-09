#' Set the destination of the syncing
#'
#' @export
set_labor_sync <-  function(){

  svDialogs::dlg_message("Please set the Destination folder...")
  destination <- svDialogs::dlg_dir()$res

  destination <- paste0(path_check(destination), "/")

  destination <- list(destination = destination)

  utils::write.csv(destination, here::here(".labor_destination"), row.names = FALSE)

  message(cat("Destination set to ", unlist(destination), "..."))
}


#' labor sync
#'
#' sync the project folder with a local folder. The origin is alwaysy the parental director
#'
#' @param bidirectional boolean
#' @param exclude_files list of file to exclude. if "default" list all hidden files in parent, and folders in `renv`.
#' @details use `rsync` to sync and `here` to identify parental directory
#' @export
labor_sync  <- function(bidirectional = FALSE,
                          exclude_files = "default" ) {

  rsync_installed <- system("rsync --version") == 0
  if (rsync_installed == FALSE) {
    error("rsync installation not found! Please install rsync...")
  }

  origin <-  paste0(here::here(), "/")

  if (!file.exists(here::here(".labor_destination"))) {
    stop("Run set_labor_sync() to setuo the destination of the sync!")
  }

  destination<- unlist(  utils::read.csv(here::here(".labor_destination")))

  message(cat("\nSyncing ", origin, " to ", destination, "..."))

  continue <- readline (prompt = "\nPress  c to cancel or anything else to continue... ")

  if (continue == "c") stop("Sync stopped!")

  if(!bidirectional %in%  c(TRUE, FALSE)) stop("Bidirectional can be TRUE or FALSE!")

  if (exclude_files == "default"){

    to_exlude_files <-  c(".*", "renv/library/", "renv/python/",  "renv/staging/")

  }

  if (length(exclude_files) > 1) {
     to_exlude_files <- exclude_files
  }

  to_exclude <- paste0("--exclude " , "\"", to_exlude_files, "\"", collapse = " ")

  rsync_comand <- "rsync -avtuP"
  all_comand <-  paste(rsync_comand, " ", to_exclude, " \"", origin, "\" ", "\"", destination, "\"", sep = "")

  system(all_comand)

  if (bidirectional == TRUE) {

    all_comand <-  paste(rsync_comand, " ", to_exclude, " \"", destination, "\" ", "\"", origin, "\"", sep = "")

    system(all_comand)
  }

}



#' clean_up
#'
#' Check if folder and file are in the right place documentation is present and if not
#' cleans it up.
#'
clean_up <- function() {


}




