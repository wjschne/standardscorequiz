#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(bslib)
library(shiny)
library(brand.yml)
proportion_round <- function (p, digits = 2) {
  p1 <- round(p, digits)
  lower_limit <- 0.95 * 10^(-1 * digits)
  upper_limit <- 1 - lower_limit
  p1[p > upper_limit & p <= 1] <- 1 - signif(1 - p[p > upper_limit & 
                                                     p <= 1], digits - 1)
  p1[p < lower_limit & p >= 0] <- signif(p[p < lower_limit & 
                                             p >= 0], digits - 1)
  p1
}
proportion2percentile <- function(p, digits = 2, remove_leading_zero = TRUE, add_percent_character = FALSE) {
  p1 <- as.character(100 * proportion_round(p, digits = digits))
  if (remove_leading_zero) {
    p1 <- stringr::str_remove(p1, "^0")
  }
  if (add_percent_character) {
    p1 <- paste0(p1, "%")
  }
  stringr::str_remove_all(p1, " ")
} 

# Needed so shinylive/webR installs it: bslib only Suggests brand.yml,
# but bs_theme(brand = TRUE) requires it.
library(brand.yml)
d <- data.frame(
  Metric = c("Scaled Score", "T-Score", "Standard Score"),
  Mean = c(10L, 50L, 100L),
  SD = c(3L, 10L, 15L)
)

# Define UI for application that draws a histogram
ui <- page_fixed(
  withMathJax(),
  theme = bs_theme(
    brand = TRUE,
    bootswatch = "minty",
    version = 5,
    `tooltip-bg` = "var(--bs-primary)",
    `tooltip-color` = "var(--bs-light)",
    `tooltip-opacity` = 1,
    `tooltip-border-radius` = "6px",
    `tooltip-padding-y` = "10px",
    `tooltip-padding-x` = "12px"
  ),
  # Application title
  titlePanel("Standard Score Conversion Practice"),
  card(
    fluidRow(
      style = "display: flex; align-items: center;",
      column(
        width = 2,
        shiny::actionButton(
          "btn_restart",
          label = "New Quiz",
          style = "margin: 0; color: #2c3e50;"
        )
      ),
      column(
        width = 4,
        shiny::checkboxInput(
          "Answers",
          "Reveal Answers",
          value = FALSE
        )
      )
    ),
    # hr(),
    shiny::uiOutput("quiz"),
    hr(),
    p(
      em("Note", .noWS = "after"),
      ": All scale conversion answers are rounded to the nearest integer."
    )
  ),
  uiOutput("metrics"),
  uiOutput("equation")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  ss <- reactiveVal()
  ss(sample(1:1000000, 1))
  observeEvent(input$btn_restart, {
    ss(sample(1:1000000, 1))
  })

  output$equation <- renderUI({
    if (input$Answers) {
      card(
        h4("Calculation Steps"),
        tags$ol(
          tags$li("Start with Old Score."),
          tags$li("Subtract Old Mean."),
          tags$li("Divide by Old SD."),
          tags$li("Multiply by New SD."),
          tags$li("Add New Mean."),
          tags$li("If Needed, Round to Nearest Integer.")
        ),
        h4("All Steps in One Equation"),
        withMathJax(
          "\\(\\text{New Score} = \\frac{\\text{Old Score}-\\text{Old Mean}}{\\text{Old SD}}\\times \\text{New SD}+\\text{New Mean}\\)"
        )
      )
    } else {
      NULL
    }
  })

  output$metrics <- renderUI({
    if (input$Answers) {
      card(renderTable(d))
    } else {
      NULL
    }
  })

  output$quiz <- renderUI({
    set.seed(ss())

    list(
      fluidRow(
        column(
          width = 5,
          strong("Question"),
          style = "margin-top: 10px; text-align: left;"
        ),
        column(
          width = 4,
          strong("Correct Answer"),
          style = "margin-top: 10px; text-align: left;"
        )
      ),
      lapply(1:6, function(i) {
        if (i < 3) {
          dd <- d[sample(1:3, 2), ]
          z <- rnorm(1)
          m1 <- round(z * dd[1, "SD"] + dd[1, "Mean"])
          m2 <- round(
            dd[2, "SD"] * (m1 - dd[1, "Mean"]) / dd[1, "SD"] + dd[2, "Mean"]
          )
          
          fluidRow(
            column(
              width = 5,
              p(
                paste0(
                  i,
                  ". Convert a ",
                  dd[1, "Metric"],
                  " of ",
                  m1,
                  " to a ",
                  dd[2, "Metric"],
                  "."
                ),
                style = "margin-top: 10px; text-align: left;"
              )
            ),
            if (input$Answers) {
              column(
                width = 4,
                p(
                  span(
                    paste0(m2, " = ("),
                    paste0(" (", m1, " "),
                    HTML("&minus;"),
                    paste0(
                      dd[1, "Mean"],
                      ") / ",
                      dd[1, "SD"],
                      ") "
                    ),
                    HTML("&times;"),
                    paste(" ", dd[2, "SD"], " + ", dd[2, "Mean"]),
                    .noWS = "outside"
                  ),
                  style = "margin-top: 10px; text-align: left;"
                )
              )
            }
          )
        } else {
          
          ss <- sample(1:19, 1)
          pr <- proportion2percentile(pnorm(ss, 10, 3))
          
          fluidRow(
            column(
              width = 5,
              p(
                paste0(
                  i,
                  ". What is the percentile rank of a scaled score of ",
                  ss,
                  "?"
                ),
                style = "margin-top: 10px; text-align: left;"
              )
            ),
            if (input$Answers) {
              column(
                width = 4,
                p(pr,
                  style = "margin-top: 10px; text-align: left;"
                )
              )
            }
          )
          
        }
 
      })
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
