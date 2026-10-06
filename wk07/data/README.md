# Data for the multinomial-logit chapter (`wk07/04-multinomial-logit.qmd`)

One tidy CSV, built 2026-10-05 by `build-cces-budget.R` in this folder
(`Rscript build-cces-budget.R` regenerates it; it downloads four columns of
the CCES 2012 Common Content from Harvard Dataverse). The chapter's other
example, the 1992 vote, reads `https://pos5747.github.io/data/bush-approval-1992.csv`
(documented in `../../wk06/data/README.md`). The script behind this example,
and how it relates to the SAGE guide, is
`../../../multinomial-examples/10-cces2012-budget.R`.

## `cces2012-budget.csv` — how to balance the federal budget, CCES 2012

- **Source.** CCES Common Content, 2012 (Ansolabehere & Schaffner), Harvard
  Dataverse doi:10.7910/DVN/HQEVPK, file `commoncontent2012.tab`
  (54,535 respondents).
- **Inspired by** Reid & Allum (2020), "Learn About Multinomial Logit in
  Stata With Data From the Cooperative Congressional Election Study (2012),"
  SAGE Research Methods Datasets, doi:10.4135/9781529710465. Same model
  (`CC328 ~ pid7 + female + faminc`, baseline "Raise taxes"), different
  sample.
- **N = 45,237**: the respondents with valid codes on all four columns.
  The SAGE guide has 35,759 because its extract, shared with other SAGE
  guides, keeps only respondents who answered all 15 survey items in it
  (immigration CC322_1–6, jobs vs. environment CC325, CC328, CC329, 7-point
  ideology CC334A, pid7, faminc, racial resentment CC422a/b, gun control
  CC320). Rebuilt from the public file, that sample reproduces the guide's
  Table 2 exactly (`../../../multinomial-examples/README.md`).
- Unweighted (the guide is too).

| column | raw name | values |
|---|---|---|
| `budget` | `CC328` | `Cut defense spending` (18,618), `Cut domestic spending` (17,062), `Raise taxes` (9,557). The item: "The federal budget deficit is approximately $1 trillion this year. If the Congress were to balance the budget it would have to consider cutting defense spending, cutting domestic spending (such as Medicare and Social Security), or raising taxes to cover the deficit. What would you MOST prefer that Congress do - cut domestic spending, cut defense spending, or raise taxes" (8 = skipped → dropped) |
| `party_id` | `pid7` | 1 = strong Democrat … 4 = independent … 7 = strong Republican (8 = not sure, 98 = skipped, 99 = not asked → dropped) |
| `female` | `gender` | 1 = female, 0 = male |
| `income` | `faminc` | family income in 16 brackets: 1 = less than $10,000 … 16 = $500,000 or more (32, 97 = prefer not to say, 98 = skipped → dropped) |
