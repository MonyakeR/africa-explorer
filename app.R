# this script creates an app for displaying Africa in numbers

library(shiny)
library(bslib)
library(reactable)

# load the data set
africa_pop <- read_csv("data/population_africa.csv")

# define the ui
ui <- page_sidebar(
  title = "Africa Explorer",
  theme = bs_theme(version = 5, base_font = font_google("Archivo")),
  sidebar = sidebar(
    title = "Indicators",
    selectInput(
      inputId = "indicator",
      label = "Select indicator",
      choices = c("Population", "Gross Domestic Product", "Income")
    ),
    selectInput(
      inputId = "year",
      label = "Select year",
      choices = 2022:1960,
      selected = 2022
    )
  ),
  # main panel
  card(
    reactableOutput("countries")
  )
)

# define the server
server <- function(input, output, session) {
  # show the table with the countries
  output$countries <- renderReactable({
    africa_pop %>% 
      filter(Year == input$year) %>%
      select(`Country Name`, `Country Code`, Population) %>% 
      reactable(
        pagination = FALSE,
        highlight = TRUE,
        defaultSorted = list(Population = "desc"),
        defaultColDef = colDef(
          format = colFormat(separators = TRUE)
        )
      )
  })
}

shinyApp(ui, server)