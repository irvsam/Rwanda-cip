# Post-processes the modelsummary/kableExtra tables in
# output_tables_path so they can be \input{} without hand edits
# (which are lost every time 05/05b rerun):
#   - adds \label{} straight after \caption{}
#   - moves the long notes out of \multicolumn (which forces the
#     table wider than the page) into a minipage below it
#   - two-line column headers so wide tables fit the page
#   - footnotesize, [htbp], "HHI $\times$ LUC", Observations, $R^2$
#   - defines the abbreviations each table uses and adds a source
#     line, so every table can be read on its own
# Safe to rerun: files that already have a \\label{} are skipped, so
# rerun the scripts that write the tables (05 to 05e) before this one.


if (!exists(".setup_done")) source("scripts/00_setup.R")

# Labels the text already uses; everything else gets tab:<file name>
LABEL_OVERRIDES <- c(
  main_split                 = "tab:mechanism",
  robust_split_hdds_nonstaple = "tab:robust",
  chain_results              = "tab:mechanism_hhi",   # plain-HHI comparison
  robust_hdds                = "tab:robust_hdds"
)

# Source line and abbreviations added below every model table
SOURCE_NOTE <- paste("\\textit{Source:} Author's calculations based on NISR EICV7 (2023/24),",
                     "AHS 2024 and SAS 2024.")
# Extra notes for tables that have none of their own
EXTRA_NOTES <- c(
  table1_descriptives = paste("Primary estimation sample of farming households (n = 3,715), unweighted.",
                              "Food groups counted over the four EICV7 consumption visits; crop measures",
                              "from AHS 2024 (Seasons A and B); LUC intensity is the share of district",
                              "cultivated land under LUC in SAS 2024.")
)

# Tighter column spacing for the tables that are too wide (otherwise they spill over the page)
TABCOLSEP_OVERRIDES <- c(chain_results = "2.5pt", main_split = "2.5pt",
                         village_fe_split = "2.5pt", diff_split = "2pt")

# Header cell over two lines, split at the space nearest the middle
stack_cell <- function(cell) {
  cell <- trimws(cell)
  if (!grepl(" ", cell) || grepl("tabular|multicolumn", cell)) return(cell)
  spaces <- gregexpr(" ", cell)[[1]]
  cut    <- spaces[which.min(abs(spaces - nchar(cell) / 2))]
  paste0("\\begin{tabular}[c]{@{}c@{}}", substr(cell, 1, cut - 1), "\\\\",
         substr(cell, cut + 1, nchar(cell)), "\\end{tabular}")
}

tidy_tex <- function(file) {
  x    <- readLines(file, warn = FALSE)
  name <- tools::file_path_sans_ext(basename(file))
  
  # Only tables with a caption (skips the datasummary descriptives)
  cap <- grep("^\\\\caption\\{", x)[1]
  if (is.na(cap)) return(invisible(NULL))
  
  # Already tidied, or built by hand (e.g. food_groups.tex): leave alone
  if (any(grepl("\\\\label\\{", x))) return(invisible(NULL))
  
  # ---- label + font size after the caption ----
  {
    sep   <- if (name %in% names(TABCOLSEP_OVERRIDES)) TABCOLSEP_OVERRIDES[[name]] else "4pt"
    label <- if (name %in% names(LABEL_OVERRIDES)) LABEL_OVERRIDES[[name]] else paste0("tab:", name)
    x <- append(x, c(paste0("\\label{", label, "}"), "\\footnotesize",
                     paste0("\\setlength{\\tabcolsep}{", sep, "}")), after = cap)
  }
  
  # ---- notes: \multicolumn{n}{l}{\rule{0pt}{1em}TEXT}\\ -> minipage ----
  # Abbreviations and the source line are added to every captioned table
  note_pat <- "^\\\\multicolumn\\{[0-9]+\\}\\{l\\}\\{\\\\rule\\{0pt\\}\\{1em\\}(.*)\\}\\\\\\\\\\s*$"
  is_note  <- grepl(note_pat, x)
  notes    <- sub(note_pat, "\\1", x[is_note])
  if (name %in% names(EXTRA_NOTES)) notes <- c(notes, EXTRA_NOTES[[name]])
  x        <- x[!is_note]
  notes    <- sub("\\.?\\s*$", ".", notes)
  note_lines <- if (length(notes) > 0)
    paste0("\\textit{Notes:} ", paste(notes, collapse = " ")) else character(0)
  if (!any(grepl("Source:", x))) note_lines <- c(note_lines, "", SOURCE_NOTE)
  end_tab <- max(grep("^\\\\end\\{tabular", x))
  x <- append(x, c("", "\\vspace{0.5em}", "\\begin{minipage}{\\linewidth}",
                   "\\footnotesize", note_lines, "\\end{minipage}"), after = end_tab)
  # ---- column headers over two lines (row after \toprule) ----
  top <- grep("^\\\\toprule", x)[1]
  if (!is.na(top) && !grepl("begin\\{tabular\\}\\[c\\]", x[top + 1])) {
    hdr   <- sub("\\\\\\\\\\s*$", "", x[top + 1])
    cells <- strsplit(hdr, "&", fixed = TRUE)[[1]]
    cells <- c(cells[1], vapply(cells[-1], stack_cell, character(1)))
    x[top + 1] <- paste0(paste(cells, collapse = " & "), "\\\\")
  }
  
  # ---- small consistency fixes ----
  x <- sub("^\\\\begin\\{table\\}$", "\\\\begin{table}[htbp]", x)
  x <- gsub(" x LUC", " $\\\\times$ LUC", x, fixed = FALSE)   # HHI, Priority, Non-priority, difference rows
  x <- sub("^Num\\.Obs\\. &", "Observations &", x)
  x <- sub("^R2 &", "$R^2$ &", x)
  if (name == "village_fe_split") {
    x <- sub("^Priority-crop concentration \\(centred\\)", "Priority HHI", x)
    x <- sub("^Non-priority concentration \\(centred\\)", "Non-priority HHI", x)
    x <- gsub(" LUC intensity &", " LUC &", x)
  }
  
  writeLines(x, file)
  message("Tidied: ", basename(file))
}

walk_files <- list.files(output_tables_path, pattern = "\\.tex$", full.names = TRUE)
invisible(lapply(walk_files, tidy_tex))