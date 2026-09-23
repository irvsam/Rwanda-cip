

main <- "main.tex"
base <- tools::file_path_sans_ext(main)

tinytex::pdflatex(main, clean = FALSE)
system2("biber", base)
tinytex::pdflatex(main, clean = FALSE)
tinytex::pdflatex(main, clean = FALSE)

# clean up auxiliary files, keep only the PDF
aux_ext <- c("aux", "bbl", "bcf", "blg", "log", "out", "toc",
             "lof", "lot", "fls", "fdb_latexmk", "run.xml", "synctex.gz")
aux_files <- unlist(lapply(aux_ext, function(e) list.files(pattern = paste0("\\.", e, "$"))))
if (length(aux_files) > 0) file.remove(aux_files)