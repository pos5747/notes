# Data for the ordered-logit chapter (`wk06/03-ordered-logit.qmd`)

Three tidy CSVs, one per example, built 2026-09-28 by
`build-ordinal-examples.R` in this folder (`Rscript build-ordinal-examples.R`
regenerates them; it downloads the two raw files from Chris Adolph's course
page and types in Agresti's table). The scripts that reproduce each source's
original analysis, and the checks that they do, are in
`../../../ordinal-examples/` (01, 10, 03).

Convention: each ordinal outcome is a character column whose labels carry a
numeric prefix (`1. Strongly disapprove`), so `factor(x, ordered = TRUE)`
sorts the levels correctly with no `levels =` argument. Predictors that the
source enters as numeric scores stay numeric; two-level predictors are
readable character columns.

`red-state-pid.csv` (2024 ANES party ID by income, with state median income)
is not part of these examples. It stays here because the exercises and
`../../../exercises/R/red-state.R` use it.

## `bush-approval-1992.csv` — approval of President Bush, 1992 ANES

- **Source.** 1992 ANES Time Series Study, as cleaned by Chris Adolph for
  POLS/CSSS 510 (University of Washington) Problem Set 4:
  `https://faculty.washington.edu/cadolph/mle/nes92con.csv`.
- **N = 750** rows, exactly the raw file. Missing values are blank in the raw
  file and are `NA` here; **nothing is dropped**. The chapter's model
  (`bush_approval ~ military_force + ideology_distance + economy + party_id +
  education`) has 558 complete cases; `polr()` drops the other 192 itself.
- Renamed from the raw columns; values unchanged except the two labeled
  columns.

| column | raw name | values |
|---|---|---|
| `bush_approval` | `bushapp` | `1. Strongly disapprove`, `2. Disapprove`, `3. Approve`, `4. Strongly approve` (raw 0–3; 23 NA) |
| `military_force` | `milforce` | 1–5, "how willing should the United States be to use military force to solve international problems?": 1 = extremely willing, 2 = very, 3 = somewhat, 4 = not very, 5 = never willing (8 NA). A general question, not about a specific conflict; ANES cumulative file `VCF0844`. Adolph's codebook says only "opposition to military force: 1 = would use force … 5 = would not" |
| `ideology_distance` | `rbdist` | 0–6, \|respondent's 7-point ideology − respondent's placement of Bush\| (175 NA) |
| `economy` | `econ` | 1–5, national economy compared with a year ago: 1 = much better … 5 = much worse (10 NA) |
| `party_id` | `partyid` | −3 strong Democrat … 0 independent … 3 strong Republican (8 NA) |
| `education` | `yrsofed` | years of education, 2–17 (5 NA) |
| `ideology` | `rlibcon` | 1–7, respondent's ideological self-placement, 1 = very liberal … 7 = very conservative (151 NA) |
| `gulf_war` | `gulfwar` | 0/1, "Was the Gulf War worth the cost?" 0 = not worth the cost, 1 = worth the cost (37 NA) |
| `nonwhite` | `nonwhite` | 0/1, 1 = nonwhite (0 NA) |
| `vote_1992` | `vote92` | `Bush`, `Clinton`, `Perot` (raw 0/1/2; 149 NA) — a nominal outcome for the multinomial chapter |

Codebook wording follows Adolph's Problem Set 4 (`https://faculty.washington.edu/cadolph/mle/510hw4.pdf`), which describes the data only as "the 1992 American National Election Study" and calls the file `nes92.csv`; the file it actually distributes is `nes92con.csv`.

## `working-mothers-gss.csv` — working mothers, GSS 1977 and 1989

- **Source.** General Social Survey, 1977 and 1989 samples: the `ordwarm2`
  data of Long (1997, ch. 5) and Long & Freese (*Regression Models for
  Categorical Dependent Variables Using Stata*), read from Adolph's copy,
  `https://faculty.washington.edu/cadolph/mle/ordwarm2.csv`.
- **N = 2,293** rows, no missing values (the source file is already complete
  cases).
- Dropped the raw file's three cumulative-outcome indicators (`warmlt2`,
  `warmlt3`, `warmlt4`); they are functions of `warm`.
- The item: "A working mother can establish just as warm and secure a
  relationship with her children as a mother who does not work."

| column | raw name | values |
|---|---|---|
| `warm` | `warm` | `1. Strongly disagree`, `2. Disagree`, `3. Agree`, `4. Strongly agree` (raw 1–4) |
| `year` | `yr89` | 1977, 1989 (raw 0/1). Use `factor(year)` in a formula to get Long & Freese's `yr89` coefficient |
| `gender` | `male` | `Female`, `Male` (raw 0/1) |
| `race` | `white` | `Nonwhite`, `White` (raw 0/1); `Nonwhite` is the alphabetical reference level, so `raceWhite` matches the raw `white` coefficient |
| `age` | `age` | years, 18–89 |
| `education` | `ed` | years of schooling, 0–20 |
| `prestige` | `prst` | occupational prestige score, 12–82 |

## `ideology-party.csv` — political ideology by party

- **Source.** Table 6.7, "Political Ideology by Gender and Political
  Party," in Agresti, *An Introduction to Categorical Data Analysis*, 2nd
  ed. (Wiley, 2007), Section 6.2.2. Agresti's source line reads "Source:
  General Social Survey," with no year. Agresti's text fits ideology on
  party alone (0.975, SE 0.129), and so does the chapter.
- **Gender is dropped.** The build script types in the table as published,
  by gender and party, then keeps only `ideology` and `party`, so the CSV is
  the table summed over gender.
- **N = 835**: one row per respondent, expanded from the cell counts with
  `tidyr::uncount()`, so `polr()` needs no `weights =`. No missing values.
- Cell counts (very liberal … very conservative), summed over gender:
  Democrats 80/81/171/41/55, Republicans 30/46/148/84/99. By gender, as
  published: Female Democrat 44/47/118/23/32, Female Republican
  18/28/86/39/48, Male Democrat 36/34/53/18/23, Male Republican
  12/18/62/45/51.

| column | values |
|---|---|
| `ideology` | `1. Very liberal`, `2. Slightly liberal`, `3. Moderate`, `4. Slightly conservative`, `5. Very conservative` |
| `party` | `Democrat`, `Republican` |
