# Build the three tidy CSVs for the ordered-logit chapter's examples.
#
# Run from anywhere: `Rscript notes/wk06/data/build-ordinal-examples.R`.
# The script writes the CSVs next to itself. It downloads two raw files from
# Chris Adolph's course page, so it needs a network connection; the Agresti
# table is typed in below. See README.md in this folder for the codebooks.
#
# Convention: each ordinal outcome is a character label with a numeric
# prefix ("1. Strongly disapprove"), so factor(x, ordered = TRUE) sorts it
# correctly with no levels = argument.

library(tidyverse)

# where to write: the folder this script lives in
out_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)),
                    error = function(e) NULL)
if (is.null(out_dir)) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  out_dir <- if (length(file_arg) == 1) dirname(normalizePath(file_arg)) else getwd()
}

# 1. Approval of President Bush, 1992 ANES ---------------------------------
# Source: Adolph's nes92con.csv (POLS/CSSS 510, Problem Set 4), a cleaned
# extract of the 1992 ANES Time Series Study. Missing values are blank in
# the raw file and stay NA here; nothing is dropped.

nes92 <- read_csv("https://faculty.washington.edu/cadolph/mle/nes92con.csv",
                  show_col_types = FALSE)

approval_labels <- c("1. Strongly disapprove", "2. Disapprove",
                     "3. Approve", "4. Strongly approve")
vote_labels <- c("Bush", "Clinton", "Perot")

bush_approval <- nes92 |>
  transmute(
    bush_approval     = approval_labels[bushapp + 1],
    military_force    = milforce,
    ideology_distance = rbdist,
    economy           = econ,
    party_id          = partyid,
    education         = yrsofed,
    ideology          = rlibcon,
    gulf_war          = gulfwar,
    nonwhite          = nonwhite,
    vote_1992         = vote_labels[vote92 + 1]
  )

write_csv(bush_approval, file.path(out_dir, "bush-approval-1992.csv"), na = "")

# 2. Working mothers, GSS 1977 and 1989 (ordwarm2) --------------------------
# Source: Long & Freese's ordwarm2, via Adolph's CSV. No missing values.
# The three cumulative indicators warmlt2-4 are dropped (they are functions
# of warm). Binary predictors become readable two-level character columns.

ordwarm2 <- read_csv("https://faculty.washington.edu/cadolph/mle/ordwarm2.csv",
                     show_col_types = FALSE)

warm_labels <- c("1. Strongly disagree", "2. Disagree",
                 "3. Agree", "4. Strongly agree")

working_mothers <- ordwarm2 |>
  transmute(
    warm      = warm_labels[warm],
    year      = if_else(yr89 == 1, "1989", "1977"),
    gender    = if_else(male == 1, "Male", "Female"),
    race      = if_else(white == 1, "White", "Nonwhite"),
    age       = age,
    education = ed,
    prestige  = prst
  )

write_csv(working_mothers, file.path(out_dir, "working-mothers-gss.csv"))

# 3. Political ideology by party and gender (Agresti) -----------------------
# Source: the 2 x 2 x 5 table in Agresti's Introduction to Categorical Data
# Analysis (cumulative logit example), N = 835. The counts are the source of
# truth; the CSV is the table expanded to one row per respondent.

ideology_labels <- c("1. Very liberal", "2. Slightly liberal", "3. Moderate",
                     "4. Slightly conservative", "5. Very conservative")

ideology_counts <- tribble(
  ~gender,  ~party,       ~n1, ~n2, ~n3, ~n4, ~n5,
  "Female", "Democrat",    44,  47, 118,  23,  32,
  "Female", "Republican",  18,  28,  86,  39,  48,
  "Male",   "Democrat",    36,  34,  53,  18,  23,
  "Male",   "Republican",  12,  18,  62,  45,  51
)

ideology <- ideology_counts |>
  pivot_longer(n1:n5, names_to = "ideology", values_to = "count") |>
  mutate(ideology = ideology_labels[as.integer(str_remove(ideology, "n"))]) |>
  uncount(count) |>
  select(ideology, party, gender)

write_csv(ideology, file.path(out_dir, "ideology-party-gender.csv"))

# report ---------------------------------------------------------------------
cat("wrote to", out_dir, "\n")
cat("bush-approval-1992.csv:    ", nrow(bush_approval), "rows\n")
cat("working-mothers-gss.csv:   ", nrow(working_mothers), "rows\n")
cat("ideology-party-gender.csv: ", nrow(ideology), "rows\n")
