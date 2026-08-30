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
library(tibble)
d <- tibble(
  Metric = c("Scaled Score", "T-Score", "Standard Score"),
  Mean = c(10, 50, 100),
  SD = c(3, 10, 15)
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Standard Score Quiz"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      shiny::actionButton("btn_restart", label = "Restart"),
      shiny::checkboxInput("Answers", "Reveal Answers", value = FALSE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      shiny::uiOutput("quiz")
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
        column(width = 4, h4("Question")),
        column(width = 2, h4("Your Answer")),
        column(width = 2, h4("Correct Answer"))
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
            width = 4,
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
          column(
            width = 2,
            textInput(
              inputId = paste0("q_", i),
              label = NULL,
              value = ""
            )
          ),
          if (input$Answers) {
            column(
              width = 2,
              p(m2, style = "margin-top: 10px; text-align: left;")
            )
          }
        )
      })
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
