# Plan: three self-contained examples for `wk06/03-ordered-logit.qmd`

Written 2026-09-28. Replaces the chapter's red-state examples (the `polr()`
party-ID-by-income example and the {brms} varying-slopes example) with three
independent examples on tidy CSVs built from `../../ordinal-examples/`
scripts 01, 10, and 03. The red-state data stay where they are for the
exercises.

## What is removed

Lines 223–363 of the chapter as of 2026-09-28: the heading
`## Example: Party ID by income in 2024` and everything after it (the
`polr()` fit, the {marginaleffects} probabilities, the cumulative-probability
plots, the stacked `geom_area()`, and the `### Example: The varying
relationship across states` {brms} fit). Lines 1–222 (ordered outcomes, the
model, the proportional-odds paragraph, the probability figure) stay
untouched.

Anchors that disappear: `#example-party-id-by-income-in-2024`,
`#polr`, `#probabilities-with-marginaleffects`,
`#computing-cumulative-probabilities`,
`#example-the-varying-relationship-across-states`. Checked 2026-09-28: the
wk06 exercise page links only the chapter URL with no anchor
(`5747website/exercises/wk06-exercises.qmd:532`); nothing else in the live
sources links these anchors.

Not removed: `data/red-state-pid.csv`, `ordered-figure.R`, and the chapter's
`03-ordered-logit_cache/` (the render will rebuild it). Cleanup candidates are
listed in the report, not acted on.

## Files created

```
notes/wk06/data/
  build-ordinal-examples.R    downloads the two raw CSVs, types in Agresti's
                              table, writes the three CSVs below
  README.md                   N, missing values, codebook for each CSV
  bush-approval-1992.csv      1992 ANES, N = 750 (558 complete on the model's
                              variables)
  working-mothers-gss.csv     GSS 1977 + 1989, N = 2,293 (no missing)
  ideology-party-gender.csv   Agresti's table, one row per respondent, N = 835
notes/wk06/PLAN-ordered-examples.md   this file
```

`notes/references.bib` gains the entries in the *Credit* section.

Regeneration: `Rscript notes/wk06/data/build-ordinal-examples.R` from the
course root (or from `notes/wk06/data/`; the script writes next to itself).
It fetches Adolph's two CSVs by URL, so it needs a network connection; the
Agresti counts are typed into the script.

## CSV schemas

Convention (from the chapter): the ordinal outcome is a character label with
a numeric prefix, `"1. Strongly disapprove"`, so that
`factor(x, ordered = TRUE)` sorts it correctly with no `levels =` argument.
Binary predictors that the model uses are stored as readable two-level
character columns (`"Female"` / `"Male"`), so `polr()` treats them as factors
and the coefficient names read `genderMale`. Ordinal predictors that the
source enters as numeric scores stay numeric.

### `bush-approval-1992.csv` — 1992 ANES (Adolph's `nes92con.csv`)

Raw: 750 rows, 10 columns, missing values coded as blanks. Kept as 750 rows
with `NA`s (no listwise deletion in the CSV; `polr()` drops incomplete rows
itself and reports "192 observations deleted due to missingness"; 558
complete cases on the five model variables plus the outcome).

| column | from | type | values |
|---|---|---|---|
| `bush_approval` | `bushapp` | chr, ordinal outcome | `1. Strongly disapprove`, `2. Disapprove`, `3. Approve`, `4. Strongly approve` (23 NA) |
| `military_force` | `milforce` | int 1–5 | opposition to using military force; 1 = most willing … 5 = least willing (8 NA) |
| `ideology_distance` | `rbdist` | int 0–6 | \|respondent's 7-point ideology − placement of Bush\| (175 NA) |
| `economy` | `econ` | int 1–5 | national economy vs. a year ago; 1 = much better … 5 = much worse (10 NA) |
| `party_id` | `partyid` | int −3…3 | −3 strong Democrat … 0 independent … 3 strong Republican (8 NA) |
| `education` | `yrsofed` | int 2–17 | years of schooling (5 NA) |
| `ideology` | `rlibcon` | int 1–7 | respondent's self-placement, 1 = extremely liberal … 7 = extremely conservative (151 NA) |
| `gulf_war` | `gulfwar` | int 0/1 | Gulf War item (definition to be confirmed from the problem set; see *Credit*) (37 NA) |
| `nonwhite` | `nonwhite` | int 0/1 | 1 = nonwhite (0 NA) |
| `vote_1992` | `vote92` | chr | `Bush`, `Clinton`, `Perot` (149 NA) |

The four extras (`ideology`, `gulf_war`, `nonwhite`, `vote_1992`) are kept
because the same file serves the wk07 multinomial chapter (`vote_1992` is a
three-way nominal outcome) and the exercises.

### `working-mothers-gss.csv` — GSS 1977 and 1989 (`ordwarm2`)

Raw: 2,293 rows, no missing values. Dropped the three redundant
cumulative-outcome indicators `warmlt2`, `warmlt3`, `warmlt4`.

| column | from | type | values |
|---|---|---|---|
| `warm` | `warm` | chr, ordinal outcome | `1. Strongly disagree`, `2. Disagree`, `3. Agree`, `4. Strongly agree` |
| `year` | `yr89` | int | `1977`, `1989`. Stored as the actual year; the chapter's formula uses `factor(year)`, so the coefficient `factor(year)1989` matches Long & Freese's `yr89` (0.524). A character column does not survive the CSV round trip (`read_csv()` parses `"1977"` as a number), and a factor column in the data made `predictions()` drop the grid columns under marginaleffects 1.0.0 (see the report), so the formula does the conversion. |
| `gender` | `male` | chr | `Female`, `Male` |
| `race` | `white` | chr | `Nonwhite`, `White` (alphabetical reference is Nonwhite, so `raceWhite` matches `white`) |
| `age` | `age` | int 18–89 | years |
| `education` | `ed` | int 0–20 | years of schooling |
| `prestige` | `prst` | int 12–82 | occupational prestige score |

The item: "A working mother can establish just as warm and secure a
relationship with her children as a mother who does not work" (GSS variable
`FECHLD`, to be confirmed; see *Credit*).

### `ideology-party-gender.csv` — Agresti's Table (2 × 2 × 5, N = 835)

One row per respondent (`tidyr::uncount()` on the published cell counts), so
the `polr()` call looks like the other two and needs no `weights =`. The
script keeps the counts as its source of truth; the CSV is the expanded form.

| column | type | values |
|---|---|---|
| `ideology` | chr, ordinal outcome | `1. Very liberal`, `2. Slightly liberal`, `3. Moderate`, `4. Slightly conservative`, `5. Very conservative` |
| `party` | chr | `Democrat`, `Republican` |
| `gender` | chr | `Female`, `Male` |

Cell counts (typed into the build script): Female Democrat 44/47/118/23/32,
Female Republican 18/28/86/39/48, Male Democrat 36/34/53/18/23, Male
Republican 12/18/62/45/51. Summed over gender: Democrats 80/81/171/41/55,
Republicans 30/46/148/84/99 (= the UVA tutorial's two-way table).

## Credit (bib entries)

Each example cites (a) the data's origin and (b) its canonical textbook or
course appearance, with `@key` so the citation renders in the margin. Keys
to reuse: `long1997`. New keys and how each was verified are recorded below
**after** the verification pass; anything not verified is flagged, not
guessed.

Verified 2026-09-28 (a Sonnet subagent fetched each source; full log with
URLs in the session scratchpad, `bib-verification.md`; I re-checked the
Agresti table and the Adolph problem-set text in the downloaded files).

| key | entry | verified how | flags |
|---|---|---|---|
| `anes1992` | Miller, Kinder, Rosenstone, and ANES. *ANES 1992 Time Series Study*. ICPSR 6067 v3, 2016. doi 10.3886/ICPSR06067.v3 | ICPSR study page metadata + on-page citation panel | electionstudies.org itself returned 403; ICPSR is the distributor record |
| `adolph2025` | Adolph, Christopher. 2025. "Problem Set 4: Modeling Presidential Approval with Ordered Probit." POLS/CSSS 510, University of Washington, Fall Quarter 2025. `mle/510hw4.pdf` | the PDF itself, downloaded and read: course title, term, due date 21 Nov 2025, Problem 1 spec, the codebook | the PS text calls the file `nes92.csv`, which 404s; the distributed file is `nes92con.csv` (N = 750). **The PS does not cite Alvarez & Nagler 1995 or any paper**; that lead did not pan out (A&N's model has no military-force or Gulf War covariate, and the PS never mentions them), so A&N is **not cited** and not added to the bib |
| `gss2019` | Smith, Davern, Freese, Morgan. *General Social Surveys, 1972–2018* [machine-readable data file]. NORC, 2019 | exact text from the NORC codebook intro PDF | the current cumulative file runs through 2024; gssdataexplorer.norc.org would not load (TLS error), so the 2019 cumulative citation is used |
| `long1997` | (existing) ch. 5 | UCLA OARC "Stata textbook examples" page for Long 1997 ch. 5: pooled 1977 & 1989 GSS, N = 2,293, `yr89`, outcome `warm` | — |
| `longfreese2014` | Long & Freese, 3rd ed., Stata Press, 2014, ISBN 978-1-59718-111-2, ch. 7 | Stata Press book page (ISBN, ch. 7 = ordinal outcomes). 2nd ed. (2006, ISBN 978-1-59718-011-5) has it as ch. 5, verified from the *Stata Journal* review 6(2):273–278 | the chapter cites the 3rd ed. only |
| `agresti2007` | Agresti, *An Introduction to Categorical Data Analysis*, 2nd ed., Wiley, 2007, ISBN 978-0-471-22618-5, doi 10.1002/0470114754 | the full 2nd-ed. text: copyright page (year, ISBN), **Section 6.2.2, Table 6.7 "Political Ideology by Gender and Political Party," source line "Source: General Social Survey"** (no year), all 20 cell counts match, sum 835. Agresti's text fits **party only**: 0.9745 (SE 0.1291), reported as 0.975 (SE 0.129) | DOI corroborated by the Wiley URL only. **The gender-and-party estimates 0.964 / 0.117 claimed in `ordinal-examples/README.md` do not appear in the 2nd-ed. text** (gender enters only as a table column); the chapter bullet now quotes Agresti's party-only 0.975 and gives the two-predictor numbers as the chapter's own. Whether the example survives into the 3rd ed. (2019) is unverified |
| `ford2015` | Ford, Clay. 2015-10-05. "Fitting and Interpreting a Proportional Odds Model." UVA Library StatLab | the page itself: author, date, party-only counts (R 30/46/148/84/99, D 80/81/171/41/55) = Table 6.7 summed over gender (checked by arithmetic) | the page cites the 1st ed. (Agresti 1996), not 2007 |

Not added: Alvarez & Nagler 1995 (verified bibliographically — *AJPS* 39(3):
714–744, JSTOR 2111651 — but no provenance link to the data, so nothing to
cite it for); Williams 2006 (*Stata Journal* 6(1): 58–82, verified; its use
of `ordwarm2` is only circumstantial from the free abstract page, so it is
left out rather than half-cited).

Extras confirmed from the problem set's codebook: `gulfwar` = "Was the Gulf
War worth the cost? 0 = not worth the cost, 1 = worth cost"; `rlibcon` 1 =
very liberal … 7 = very conservative; `yrsofed` "0–17".

## The three examples (outline)

Each is a `##` section, loads its own packages and data, and creates no
object another section needs. Object names are per-example (`approval`,
`warm`, `ideology`; fits `fit_approval`, `fit_warm`, `fit_ideology`) so
deleting a section leaves the others intact. Prose is bullets only, brief
phrases, no paragraphs. `library(MASS)` is loaded in each example
(masking `dplyr::select()`, so each example uses `dplyr::select()` explicitly
or avoids `select()`), and `library(tidyverse)` and
`library(marginaleffects)` are loaded in each as well.

Each example runs the same six steps:

1. `read_csv()` + `glimpse()`
2. `mutate(outcome = factor(outcome, ordered = TRUE))`; `levels()`
3. `polr(..., Hess = TRUE)` (probit where the source used probit)
4. `summary()`
5. `predictions(fit, newdata = datagrid(...))`
6. the stacked-area figure, `geom_area(position = "stack")`

### Example 1 — Approval of President Bush, 1992 ANES

- ordered **probit** (`method = "probit"`), as in Adolph's Problem Set 4
- `bush_approval ~ military_force + ideology_distance + economy + party_id + education`
- reproduces Adolph's specification; `polr()` reports 558 rows used
- figure: `x = military_force` (1–5), fill = approval category,
  `facet_wrap(vars(party_id))` at `party_id = c(-3, 3)` — the problem set's
  own "strong Democrats vs. strong Republicans across military force"
  scenario; other covariates at their means (`datagrid()` default). Movement
  is large: for strong Democrats Pr(strongly disapprove) runs 0.28 → 0.77
  across `military_force`; for strong Republicans Pr(strongly approve) runs
  0.49 → 0.09.
- bullets quote: the sign pattern (party_id positive, economy negative), the
  N used, and two probabilities from the grid

### Example 2 — Working mothers, GSS 1977 and 1989

- ordered **logit** (the default), Long & Freese's specification
- `warm ~ factor(year) + gender + race + age + education + prestige`
- reproduces Long & Freese: `factor(year)1989` 0.524, `genderMale` −0.733, `raceWhite`
  −0.391, `age` −0.022, `education` 0.067, `prestige` 0.006
- figure: `x = age` (18 to 89 by 1), fill = agreement category,
  `facet_wrap(vars(gender))`. Age has the largest movement of the continuous
  predictors: for women Pr(strongly agree) runs 0.26 at 20 → 0.09 at 80.
- bullets quote two coefficients and two probabilities

### Example 3 — Political ideology by party and gender (Agresti)

- ordered **logit**, Agresti's cumulative-logit (proportional odds) model
- `ideology ~ party + gender`
- reproduces Agresti: `partyRepublican` 0.964 (SE 0.130), `genderMale` 0.117
  (SE 0.127)
- figure: **stacked `geom_col()`**, `x = party`, fill = ideology category,
  `facet_wrap(vars(gender))`. Reason: both predictors are binary.
  `geom_area()` needs a numeric `x`; with a two-level character `x` it draws
  an empty panel with no warning (tested 2026-09-28 under ggplot2 4.0.3), and
  coercing party to 0/1 would draw a ramp between two points that the model
  does not describe (there is nothing between Democrat and Republican). A
  stacked column per party is the honest version of the same picture: each
  column is the full probability distribution for one group, exactly what
  each vertical slice of a `geom_area()` figure is. One bullet says so.
- bullets quote the two coefficients and the Republican vs. Democrat shift
  in Pr(very conservative) among women (0.11 → 0.25)

## Verification of quoted numbers

Every number in a bullet is read from the rendered chapter after
`make notes`, not from the scratch fit. The scratch fits (2026-09-28,
R 4.6.1, MASS 7.3-65, marginaleffects 1.0.0, ggplot2 4.0.3) already
reproduce: Adolph's ordered probit (coefficients −0.332, −0.215, −0.351,
0.257, −0.041; cutpoints −4.08, −3.22, −1.93), Long & Freese's ordered logit
(above), and Agresti's two coefficients (above).

## Process

1. Build script → CSVs → README (scratch first, then copy into `data/`).
2. Bib entries, verified.
3. Edit the chapter: delete from line 223, append the three sections.
4. `make notes` from the course root; `make check`.
5. Open `notes/docs/wk06/03-ordered-logit.html`; look at every figure.
6. Update the progress tables (root `CLAUDE.md` wk06 row; `notes/CLAUDE.md`
   footnote `wk06pass`).
7. Report; no git.
