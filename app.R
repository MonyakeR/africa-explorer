# this script creates an app for displaying Africa in numbers

library(shiny)
library(bslib)
library(reactable)
library(htmltools)
library(tidyverse)

# source scripts
source("www/R/utils.R")

# load the data set
africa <- read_csv("data/africa.csv")

# define the ui
ui <- page_navbar(
  title = "Africa Explorer",
  bg = "#2b455f",
  inverse = TRUE,
  theme = bs_theme(
    version = 5, 
    base_font = font_google("Public Sans")
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  nav_spacer(),
  nav_panel(
    title = "Countries",
    card(reactableOutput("countries"))
  ),
  nav_panel(
    title = "About",
    p("This is a WIP")
  )
)

# define the server
server <- function(input, output, session) {
  
  # create dataset for the table
  tbl_africa <- reactive({
    africa 
  })
  
  # show the table with the countries
  output$countries <- renderReactable({
      reactable(
        tbl_africa(),
        searchable = TRUE,
        pagination = FALSE,
        highlight = TRUE,
        defaultSorted = "Population, total",
        defaultSortOrder = "desc",
        defaultColDef = colDef(
          format = colFormat(separators = TRUE),
          vAlign = "center",
          headerVAlign = "bottom",
          class = "cell",
          headerClass = "header"
        ),
        columns = list(
          `Country Name` = colDef(
            minWidth = 160,
            #filterable = TRUE,
            cell = function(value, index) {
              div(
                class = "country",
                img(class = "country-flag", alt = paste(value, "flag"), src = sprintf("images/flags/%s.svg", value)),
                div(
                  span(class = "country-name", value),
                  span(class = "iso-code", sprintf("%s", tbl_africa()[index, "Country Code"]))
                )
              )
            }
          ),
          Currency = colDef(
            minWidth = 140,
            cell = function(value, index) {
              div(
                span(class = "currency", value),
                span(class = "currency-code", sprintf("%s", tbl_africa()[index, "Currency Code"]))
              )
            }
          ),
          `Currency Code` = colDef(show = FALSE),
          `GDP per capita (current US$)` = colDef(
            minWidth = 100,
            format = colFormat(digits = 0, separators = TRUE),
            class = "border-left",
            style = function(value) {
              if (!is.na(value)) {
                normalised <- (value - min(tbl_africa()$`GDP per capita (current US$)`, na.rm = TRUE)) / (max(tbl_africa()$`GDP per capita (current US$)`, na.rm = TRUE) - min(tbl_africa()$`GDP per capita (current US$)`, na.rm = TRUE))
                colour <-  rgb(colorRamp(c("#d8ebcc", "#5ea76a"))(normalised), maxColorValue = 255)
                list(
                  background = colour,
                  fontFamily = "JetBrains Mono"
                )
              }
            }
          ),
          `Population, total` = colDef(
            minWidth = 100,
            name = "Population (2022)",
            align = "right",
            class = "border-left",
            style = function(value) {
              if (!is.na(value)) {
                normalised <- (value - min(tbl_africa()$`Population, total`)) / (max(tbl_africa()$`Population, total`) - min(tbl_africa()$`Population, total`))
                colour <-  rgb(colorRamp(c("#92c5de", "#023959"))(normalised), maxColorValue = 255)
                list(
                  background = colour,
                  fontFamily = "JetBrains Mono",
                  color = "#ffffff"
                )
              }
            }
          ),
          `Country Code` = colDef(show = FALSE)
        )
      )
  })
}

shinyApp(ui, server)