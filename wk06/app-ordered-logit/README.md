# Ordered logit app: approval of President Bush, 1992

A small Shiny app that generalizes the stacked-area figure in the ordered-logit
chapter (`../03-ordered-logit.qmd`, "Example: Approval of President Bush,
1992"). It fits the chapter's `polr()` model once at startup and draws
Pr(y = j) for the four approval categories, stacked, as one predictor varies.
Below the plot it prints the `datagrid()` + `predictions()` + `ggplot()` code
that reproduces the current figure, assuming `fit_approval` from the notes.

## Run it

From the course root:

```r
shiny::runApp("notes/wk06/app-ordered-logit")
```

or `shiny::runApp()` with this folder as the working directory. Needs
{shiny}, {bslib}, {ggplot2}, {dplyr}, {purrr}, {readr}, {MASS}, and
{marginaleffects} (the individual tidyverse packages rather than the
{tidyverse} meta-package, so that Shinylive loads only what the app uses).
`data/bush-approval-1992.csv` is a byte-identical copy of `../data/`'s file,
so the folder is self-contained.

## Design: three roles

Every predictor plays one of three roles, and the sidebar is organized by
role rather than by variable. The **x-axis** variable runs over its observed
range (`seq(min, max)`). The **facet** variable, if any, takes the values the
user checks (default: its observed min and max); unchecking every value is the
same as choosing no facet. Every **fixed** variable is held at its sample
median unless the user unchecks "hold at sample median" and picks a value on
the slider. Choosing an x-variable removes it from the facet choices, and the
controls for the facet values and the fixed variables are generated on the fly
from the current roles. Choices are remembered per variable for the session,
so a variable that changes role and comes back keeps its earlier settings.

Note that with the defaults the plot is the chapter's figure exactly:
`datagrid()` rounds integer-valued columns to the nearest integer, so the
chapter's "means" (2.1, 4.0, 14.1) are the medians (2, 4, 14).

## Deployment

Not deployed. The chapter carries an Observable JS version of this figure
instead (decided 2026-09-28 after a Shinylive export of this app came in at
about 100 MB, almost all of it the webR runtime): the chapter's R chunk hands
the fit to the page with `ojs_define()` and `{ojs}` cells draw the same plot
and print the same code; see "Interactive figures" in `../../CLAUDE.md`. This
Shiny app stays as the local reference implementation (`shiny::runApp()`), and
its `code_text()` output is the contract the OJS version was checked against.
A Shinylive export still sits in `../../apps/` (unlinked, listed under
`resources:` in `_quarto.yml`) pending a decision to delete it; to re-export:

```r
shinylive::export(appdir = ".", destdir = "../../apps", subdir = "ordered-logit")
```
