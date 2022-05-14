

library(shiny)
library(shiny.fluent)


# shinyApp(
#   ui = div(
#     Checkbox.shinyInput("checkbox", value = TRUE),
#     textOutput("checkboxValue")
#   ),
#   server = function(input, output) {
#     output$checkboxValue <- renderText({
#       sprintf("Value: %s", input$checkbox)
#     })
#   }
# )


analysis_page <- makePage(
  "Sales representatives",
  "Best performing reps",
  div(
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      makeCard("Filters", filters, size = 4, style = "max-height: 320px"),
      makeCard("Deals count", plotlyOutput("plot"), size = 8, style = "max-height: 320px")
    ),
    uiOutput("analysis")
  )
)


ui <- fluentPage(
  tags$style(".card { padding: 28px; margin-bottom: 28px; }"),
  analysis_page
)
