# Ordered logit: approval of President Bush, 1992
#
# A Shiny app that generalizes the stacked-area figure in the ordered-logit
# chapter (notes/wk06/03-ordered-logit.qmd). The user picks which predictor
# runs along the x-axis, which predictor (if any) defines the facets, and at
# what value each remaining predictor is held fixed. The app then builds the
# same datagrid() + predictions() + geom_area() pipeline as the chapter and
# prints the R code that reproduces the current plot.

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(purrr)            # map(), set_names(), %||%
library(readr)
library(MASS)             # polr(); loaded after dplyr, so it masks dplyr::select()
library(marginaleffects)

# ---- fit the chapter's model once, at startup ------------------------------

approval <- read_csv("data/bush-approval-1992.csv", show_col_types = FALSE) |>
  mutate(bush_approval = factor(bush_approval, ordered = TRUE))

fit_approval <- polr(bush_approval ~ military_force + ideology_distance +
                       economy + party_id + education,
                     data = approval, Hess = TRUE)

# ---- describe each predictor from the model's complete cases ---------------

predictors <- c("military_force", "ideology_distance", "economy",
                "party_id", "education")

labels <- c(
  military_force    = "Opposition to military force (1 = extremely willing … 5 = never willing)",
  ideology_distance = "Ideological distance from Bush (0 = same position … 6 = opposite ends)",
  economy           = "National economy vs. a year ago (1 = much better … 5 = much worse)",
  party_id          = "Party identification (−3 = strong Democrat … 3 = strong Republican)",
  education         = "Years of education"
)

# the 558 rows polr() actually used
complete <- model.frame(fit_approval)

# one entry per predictor: label, observed min/max, sorted unique values, median.
# (Named var_info rather than vars, which would shadow ggplot2::vars().)
var_info <- map(predictors, function(v) {
  x <- complete[[v]]
  list(label  = labels[[v]],
       min    = min(x),
       max    = max(x),
       values = sort(unique(x)),
       median = round(median(x)))  # whole number, so the step-1 slider can sit on it
}) |>
  set_names(predictors)

# ---- helpers ---------------------------------------------------------------

# print a numeric vector the way a person would type it: 1:5, c(-3, 3), or 2
format_values <- function(x) {
  x <- sort(unique(x))
  if (length(x) == 1) return(as.character(x))
  if (all(diff(x) == 1)) return(paste0(min(x), ":", max(x)))  # consecutive integers
  paste0("c(", paste(x, collapse = ", "), ")")
}

# ---- UI --------------------------------------------------------------------

ui <- page_sidebar(
  title = "Ordered logit: approval of President Bush, 1992",
  fillable = FALSE,  # a fixed-height plot; a fillable page let the first render overlap the code panel
  sidebar = sidebar(
    width = 340,
    selectInput("x_var", "x-axis variable",
                choices = predictors, selected = "military_force"),
    selectInput("facet_var", "Facet variable",
                choices = c("None", setdiff(predictors, "military_force")),
                selected = "party_id"),
    uiOutput("facet_ui"),
    uiOutput("fixed_ui")
  ),
  p(class = "text-muted small",
    "This app runs R in your browser (via webR); the first load downloads the packages and can take half a minute."),
  p("Choose which predictor runs along the x-axis and which defines the panels; the rest are held at their sample median unless you set a value."),
  plotOutput("plot", height = "450px"),
  p("R code for this plot (assumes ", code("fit_approval"), " from the notes)."),
  verbatimTextOutput("code")
)

# ---- server ----------------------------------------------------------------

server <- function(input, output, session) {

  # memory of what the user chose earlier in the session, keyed by variable,
  # so a variable that changes role and comes back keeps its settings
  memory <- reactiveValues(facet = list(), median = list(), value = list())

  # -- keep the roles consistent --------------------------------------------

  # when x changes, drop it from the facet choices; if it *was* the facet,
  # reset the facet to None
  observeEvent(input$x_var, ignoreInit = TRUE, {
    current <- input$facet_var
    updateSelectInput(session, "facet_var",
                      choices  = c("None", setdiff(predictors, input$x_var)),
                      selected = if (identical(current, input$x_var)) "None" else current)
  })

  # -- remember the dynamic inputs as they change ---------------------------

  observeEvent(input$facet_vals, ignoreNULL = FALSE, {
    f <- input$facet_var
    if (is.null(f) || !f %in% predictors) return()
    vals <- input$facet_vals
    # NULL before the checkbox group exists is not a choice; NULL after the
    # user has chosen means they unchecked everything
    if (is.null(vals) && is.null(memory$facet[[f]])) return()
    memory$facet[[f]] <- vals %||% character(0)
  })

  for (v in predictors) local({
    v <- v
    observeEvent(input[[paste0("fix_", v, "_median")]], {
      memory$median[[v]] <- input[[paste0("fix_", v, "_median")]]
    })
    observeEvent(input[[paste0("fix_", v, "_value")]], {
      memory$value[[v]] <- input[[paste0("fix_", v, "_value")]]
    })
  })

  # -- the three roles ------------------------------------------------------

  # the facet variable, or NULL when there is none (also during the moment
  # after x changes to the current facet, before the reset above lands)
  facet_var <- reactive({
    f <- input$facet_var
    if (is.null(f) || f == "None" || f == input$x_var) NULL else f
  })

  # the facet values the user checked, restricted to values this variable
  # actually takes (a stale checkbox group from the previous facet variable
  # can linger for one tick); empty means "no facets"
  facet_values <- reactive({
    f <- facet_var()
    if (is.null(f)) return(NULL)
    vals <- as.numeric(input$facet_vals)
    vals <- vals[vals %in% var_info[[f]]$values]
    if (length(vals) == 0) NULL else sort(vals)
  })

  fixed_vars <- reactive({
    f <- if (is.null(facet_values())) NULL else facet_var()
    setdiff(predictors, c(input$x_var, f))
  })

  # the value a fixed variable is held at: its median, or the slider
  fixed_value <- function(v) {
    at_median <- input[[paste0("fix_", v, "_median")]]
    req(!is.null(at_median))
    if (at_median) return(var_info[[v]]$median)
    value <- input[[paste0("fix_", v, "_value")]]
    req(!is.null(value))   # not req(value): a slider at 0 is a real choice
    value
  }

  # -- dynamic controls -----------------------------------------------------

  # facet values: re-render only when the facet variable changes
  output$facet_ui <- renderUI({
    f <- input$facet_var
    if (is.null(f) || f == "None") return(NULL)
    info <- var_info[[f]]
    chosen <- isolate(memory$facet[[f]]) %||% c(info$min, info$max)
    checkboxGroupInput("facet_vals", paste0("Facet values (", f, ")"),
                       choices = info$values, selected = chosen, inline = TRUE)
  })

  # fixed variables: re-render only when x or the facet variable changes
  output$fixed_ui <- renderUI({
    f <- input$facet_var
    fixed <- setdiff(predictors, c(input$x_var, f))
    blocks <- lapply(fixed, function(v) {
      info <- var_info[[v]]
      id_median <- paste0("fix_", v, "_median")
      id_value  <- paste0("fix_", v, "_value")
      at_median <- isolate(memory$median[[v]]) %||% TRUE
      value     <- isolate(memory$value[[v]])  %||% info$median
      tags$div(
        style = "margin-bottom: 0.75em;",
        tags$strong(v), tags$br(), tags$small(info$label),
        checkboxInput(id_median, paste0("hold at sample median (", info$median, ")"),
                      value = at_median),
        conditionalPanel(
          condition = sprintf("!input['%s']", id_median),
          sliderInput(id_value, NULL, min = info$min, max = info$max,
                      value = value, step = 1, ticks = FALSE)
        )
      )
    })
    tagList(tags$h6("Fixed variables"), blocks)
  })

  # -- grid, predictions, plot, code ----------------------------------------

  # the arguments to datagrid(): x first, then the facet, then the fixed ones
  grid_args <- reactive({
    x <- input$x_var
    args <- list()
    args[[x]] <- seq(var_info[[x]]$min, var_info[[x]]$max)
    if (!is.null(facet_values())) args[[facet_var()]] <- facet_values()
    for (v in fixed_vars()) args[[v]] <- fixed_value(v)
    args
  })

  grid <- reactive({
    do.call(datagrid, c(list(model = fit_approval), grid_args()))
  })

  preds <- reactive({
    predictions(fit_approval, newdata = grid())
  })

  plot_obj <- reactive({
    x <- input$x_var
    g <- ggplot(preds(), aes(x = .data[[x]], y = estimate, fill = group)) +
      geom_area(position = position_stack(reverse = TRUE)) +
      theme_minimal(base_size = 14) +
      labs(x = var_info[[x]]$label, y = "Pr(y = j)", fill = "Approval")
    if (!is.null(facet_values())) {
      g <- g + facet_wrap(vars(.data[[facet_var()]]), labeller = label_both)
    }
    g
  })

  code_text <- reactive({
    args <- grid_args()
    arg_lines <- paste0(names(args), " = ", map_chr(args, format_values))
    grid_call <- paste0("newdata = datagrid(",
                        paste(arg_lines, collapse = paste0(",\n", strrep(" ", 36))),
                        "))")
    plot_lines <- c(
      sprintf("ggplot(p, aes(x = %s, y = estimate, fill = group)) +", input$x_var),
      "  geom_area(position = position_stack(reverse = TRUE))"
    )
    if (!is.null(facet_values())) {
      plot_lines[2] <- paste0(plot_lines[2], " +")
      plot_lines <- c(plot_lines,
                      sprintf("  facet_wrap(vars(%s), labeller = label_both)", facet_var()))
    }
    paste(c("p <- predictions(fit_approval,",
            paste0(strrep(" ", 17), grid_call),
            "",
            plot_lines),
          collapse = "\n")
  })

  output$plot <- renderPlot(plot_obj())
  output$code <- renderText(code_text())
}

shinyApp(ui, server)
