library(tinytex)


# setwd("report/Seminar_Paper")
tinytex::pdflatex("main.tex")
system2("biber", "main")
tinytex::pdflatex("main.tex")
tinytex::pdflatex("main.tex")