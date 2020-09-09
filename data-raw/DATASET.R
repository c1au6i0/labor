# Folders and expected extensions

code_lab <-   c("R", "RMD", "RMarkdown", "txt", "md")
data_lab <- c("rda", "rds", "csv", "xls", "xlsm", "xlsx", "txt", "md")
documents_lab <- c("pdf", "html", "doc", "docm", "docx", "txt", "md")
figs_lab <-   c("html", "png", "pdf", "tex", "log", "aux", "txt", "md")
manuscripts_lab <- unique(unlist(c(documents_lab, figs_lab, "RMD", "RMarkdown")))
objs_lab <- data_lab
reports_lab <- manuscripts_lab

max_n  <- length(manuscripts_lab)

folder_ext <- data.frame()[1:max_n,]

folder_lab <- list(code_lab, data_lab, documents_lab, figs_lab, manuscripts_lab, objs_lab, reports_lab)

for(x in folder_lab){
  folder_ext <- cbind(folder_ext, tolower(x[1:max_n]))
}

row.names(folder_ext) <-  NULL
names(folder_ext) <- folder_lab


usethis::use_data(folder_ext, internal = TRUE, overwrite = TRUE)
