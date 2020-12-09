# Folders and expected extensions

code_lab <- c("r", "rmd", "rmarkdown", "txt", "md", "html")
data_lab <- c("rda", "rds", "csv", "xls", "xlsm", "xlsx", "txt", "md")
documents_lab <- c("pdf", "html", "doc", "docm", "docx", "txt", "md")
figs_lab <- c("html", "png", "pdf", "tex", "log", "aux", "txt", "md", "svg")
manuscripts_lab <- unique(unlist(c(documents_lab, figs_lab, "rmd", "rmarkdown", "bib", "cls")))
objs_lab <- data_lab
reports_lab <- unlist(c(manuscripts_lab, "pptx", "ppt"))

all_folders <- list(code_lab, data_lab, documents_lab, figs_lab, manuscripts_lab, objs_lab, reports_lab)

max_n <- max(unlist((lapply(all_folders, length))))

folder_ext <- data.frame()[1:max_n, ]

folder_lab <- list(code_lab, data_lab, documents_lab, figs_lab, manuscripts_lab, objs_lab, reports_lab)

for (x in folder_lab) {
  folder_ext <- cbind(folder_ext, tolower(x[1:max_n]))
}

row.names(folder_ext) <- NULL
names(folder_ext) <- c("code", "data", "documents", "figs", "manuscripts", "objs", "reports")


usethis::use_data(folder_ext, internal = TRUE, overwrite = TRUE)
