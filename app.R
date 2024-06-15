# this script creates an app for displaying Africa in numbers

library(shiny)
library(bslib)
library(reactable)
library(htmltools)
library(tidyverse)

# source scripts
source("www/R/utils.R")

# load the data set
africa <- vroom::vroom("data/africa.csv")

# define the ui
ui <- page_navbar(
  title = "Africa Explorer",
  bg = "#2b455f",
  inverse = TRUE,
  theme = bs_theme(
    version = 5, 
    base_font = font_google("Public Sans")
  ),
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    )
  ),
  nav_spacer(),
  nav_panel(
    title = "Countries",
    card(reactableOutput("countries"))
  ),
  nav_panel(
    title = "About",
    card(
      card_header("About"),
      p("The aim of this project is to demonstrate how interactive tables utilise the details-on-demand principle. By clicking on a table row, users can access additional information."),
      HTML(
        '
        <p>The sources used are as follows:</p>
         <ul>
            <li><a href="https://datatopics.worldbank.org/world-development-indicators" target="_blank">World Development Indicators - World Bank</a></li>
            <li><a href="https://www.international.ucla.edu/asc/countries" target="_blank">Countries - UCLA International Institute</a></li>
            <li><a href="http://news.bbc.co.uk/1/hi/country_profiles" target="_blank">Country Profiles - BBC News</a></li>
        </ul>
         <p>The table was developed by <a href="http://rmonyake.com" target="_blank">Retselisitsoe Monyake.</a></p>
      '
      )
    )
  )
)

# define the server
server <- function(input, output, session) {
  
  # create dataset for the table
  tbl_africa <- reactive({
    africa 
  })
  
  # function to show details when user clicks on a country in the table
  row_details <- function(index) {
    
    country_profile <- tbl_africa()[index, ]
    
    country_field <- function(name, value) {
      if (name != "Country profile:") {
        div(
          class = "detail-value",
          div(class = "detail-label", name),
          div(class = "value", value)
        )
      } else {
        div(
          class = "detail-value",
          div(class = "detail-label", name),
          div(id = "country-profile", class = "value", value)
        )
      }
    }
    
    detail <- div(
      class = "country-details",
      div(
        class = "top-level",
        div(
          class = "details",
          country_field(
            name = "Population (2022)", 
            value = format(country_profile$population_total, big.mark = ",")
          ),
          country_field(
            name = "GDP per capita (current US$)", 
            value = format(round(country_profile$gdp_per_capita, digits = 0), big.mark = ",")
          ),
          country_field(
            name = "Area (sq Km)", 
            value = format(country_profile$area, big.mark = ",")
          ),
          country_field(
            name = "President", 
            value = country_profile$president
          ),
          country_field(
            name = "Languages", 
            value = country_profile$languages
          ),
          country_field(
            name = "Life Expectancy", 
            value = country_profile$life_expectancy
          )
        ),
        div(
          img(
            class = "country-map", 
            src = sprintf("images/maps/%s.png", country_profile$country_code)
          )
        )
      ),
      country_field(
        name = "Country profile:", 
        value = country_profile$country_profile
      )
    )
  }
  
  # show the table with the countries
  output$countries <- renderReactable({
      reactable(
        tbl_africa(),
        searchable = TRUE,
        pagination = FALSE,
        highlight = TRUE,
        defaultSorted = "population_total",
        defaultSortOrder = "desc",
        details = row_details,
        defaultColDef = colDef(
          format = colFormat(separators = TRUE),
          vAlign = "center",
          headerVAlign = "bottom",
          class = "cell",
          headerClass = "header"
        ),
        columns = list(
          country_name = colDef(
            name = "Country Name",
            minWidth = 160,
            cell = function(value, index) {
              div(
                class = "country",
                img(class = "country-flag", alt = paste(value, "flag"), src = sprintf("images/flags/%s.svg", value)),
                div(
                  span(class = "country-name", value),
                  span(class = "iso-code", sprintf("%s", tbl_africa()[index, "country_code"]))
                )
              )
            }
          ),
          Currency = colDef(
            minWidth = 140,
            cell = function(value, index) {
              div(
                span(class = "currency", value),
                span(class = "currency-code", sprintf("%s", tbl_africa()[index, "currency_code"]))
              )
            }
          ),
          currency_code = colDef(show = FALSE),
          country_profile = colDef(show = FALSE),
          gdp_per_capita = colDef(
            name = "GDP per capita (current US$)",
            minWidth = 100,
            format = colFormat(digits = 0, separators = TRUE),
            class = "border-left",
            style = function(value) {
              
              gdp_pc_min <- min(tbl_africa()$gdp_per_capita, na.rm = TRUE)
              gdp_pc_max <- max(tbl_africa()$gdp_per_capita, na.rm = TRUE)
              
              if (!is.na(value)) {
                normalised <- (value - gdp_pc_min) / (gdp_pc_max - gdp_pc_min)
                colour <-  rgb(
                  colorRamp(c("#d8ebcc", "#5ea76a"))(normalised), 
                  maxColorValue = 255
                )
                list(
                  background = colour,
                  fontFamily = "JetBrains Mono"
                )
              }
            }
          ),
          population_total = colDef(
            minWidth = 100,
            name = "Population (2022)",
            align = "right",
            class = "border-left",
            style = function(value) {
              
              pop_min <- min(tbl_africa()$population_total)
              pop_max <- max(tbl_africa()$population_total)
              
              if (!is.na(value)) {
                normalised <- (value - pop_min) / (pop_max - pop_min)
                colour <-  rgb(colorRamp(c("#92c5de", "#023959"))(normalised), maxColorValue = 255)
                list(
                  background = colour,
                  fontFamily = "JetBrains Mono",
                  color = "#ffffff"
                )
              }
            }
          ),
          country_code = colDef(show = FALSE),
          area = colDef(show = FALSE),
          languages = colDef(show = FALSE),
          life_expectancy = colDef(show = FALSE),
          president = colDef(show = FALSE)
        )
      )
  })
}

shinyApp(ui,  server, options = list(launch.browser = FALSE, port = 5288))