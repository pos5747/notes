# Build the tidy CSV for the multinomial-logit chapter's CCES example.
#
# Run from anywhere: `Rscript notes/wk07/data/build-cces-budget.R`.
# The script writes the CSV next to itself. It downloads four columns of the
# CCES 2012 Common Content from Harvard Dataverse, so it needs a network
# connection. See README.md in this folder for the codebook.

library(tidyverse)

# where to write: the folder this script lives in
out_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)),
                    error = function(e) NULL)
if (is.null(out_dir)) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  out_dir <- if (length(file_arg) == 1) dirname(normalizePath(file_arg)) else getwd()
}

# CCES Common Content 2012, doi:10.7910/DVN/HQEVPK, datafile 2456347.
# Dataverse serves a column subset of the ingested file; the variable IDs come
# from the file's DDI metadata (CC328, pid7, faminc, gender).
url <- paste0("https://dataverse.harvard.edu/api/access/datafile/2456347",
              "?variables=14574921,14574633,14574649,14574786")
cces <- read_tsv(url, show_col_types = FALSE)

budget_labels <- c("Cut defense spending", "Cut domestic spending", "Raise taxes")

budget <- cces |>
  transmute(
    budget   = budget_labels[if_else(CC328 %in% 1:3, CC328, NA)],
    party_id = if_else(pid7 %in% 1:7, pid7, NA),     # 8 not sure, 98, 99 -> NA
    female   = as.integer(gender == 2),
    income   = if_else(faminc %in% 1:16, faminc, NA)  # 32, 97, 98 -> NA
  ) |>
  drop_na()

stopifnot(nrow(budget) == 45237)

write_csv(budget, file.path(out_dir, "cces2012-budget.csv"))
