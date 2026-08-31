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
# Needed so shinylive/webR installs it: bslib only Suggests brand.yml,
# but bs_theme(brand = TRUE) requires it.
library(brand.yml)
d <- data.frame(
  Metric = c("Scaled Score", "T-Score", "Standard Score"),
  Mean = c(10, 50, 100),
  SD = c(3, 10, 15)
)

# Define UI for application that draws a histogram
ui <- fluidPage(
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
  mainPanel(
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
    hr(),
    shiny::uiOutput("quiz"),
    p(
      em("Note", .noWS = "after"),
      ": All answers are rounded to the nearest integer."
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  ss <- reactiveVal()
  ss(sample(1:1000000, 1))
  observeEvent(input$btn_restart, {
    ss(sample(1:1000000, 1))
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
          width = 2,
          strong("Correct Answer"),
          style = "margin-top: 10px; text-align: center;"
        )
      ),
      lapply(1:5, function(i) {
        dd <- d[sample(1:3, 2), ]
        z <- rnorm(1)
        m1 <- as.integer(z * dd[1, "SD"] + dd[1, "Mean"])
        m2 <- as.integer(
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
              width = 2,
              p(m2, style = "margin-top: 10px; text-align: center;")
            )
          }
        )
      })
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
