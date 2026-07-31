# Welcome to your new Shiny Cancer Dashboard!
# This file contains the code required to build your dashboard.
# For information on how this code is structured, please visit:
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/

# As diagnoses and deaths is essentially the same, but with different data,
# these files have been "modularised". See here:
# https://shiny.posit.co/r/articles/improve/modules/


# Web Browser Only
options(shiny.launch.browser = TRUE)



# Let's now load in the required packages
library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(markdown)




# Then we will "source", or load in the modules
source("module.R", local = TRUE)
source("load_data.R", local = TRUE)


# UI (User Interface)
ui <- page_navbar(
  header = tagList(
    withMathJax(),
    tags$head(
      tags$script(src = "scripts.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    )
  ),
  title = dashboard_title,
  nav_panel(
    title = "Diagnoses",
    id = "diagnoses",
    UI_module("Diagnoses")
  ),
  if(!no_mrt){
    nav_panel(
      title = "Deaths",
      id = "deaths",
      UI_module("Deaths")
    )
  },
  nav_spacer(),
  nav_panel(
    title = "Methods",
    id = "methods",
    class = "methods-panel",

    uiOutput("methods_ui")

  )
)

server <- function(input, output, session){

  server_module("Diagnoses")

  if(!no_mrt){
    server_module("Deaths")
  }

  output$methods_ui <- renderUI({

    if(length(measure_choices) != 1) {

      md_text <- paste(readLines("www/methods_rates.md", warn = FALSE), collapse = "\n")
      md_text <- gsub("{{standard_pop_name}}", standard_pop_name, md_text, fixed = TRUE)

      session$onFlushed(function() {
        session$sendCustomMessage("refreshMethodsPanel", TRUE)
      }, once = TRUE)

      HTML(markdown::markdownToHTML(text = md_text, fragment.only = TRUE))

    } else {

      md_text <- paste(readLines("www/methods_no_rates.md", warn = FALSE), collapse = "\n")

      session$onFlushed(function() {
        session$sendCustomMessage("refreshMethodsPanel", TRUE)
      }, once = TRUE)

      HTML(markdown::markdownToHTML(text = md_text, fragment.only = TRUE))

    }

  })

  output$report <- downloadHandler(
    filename = ""
  )

}


shinyApp(ui, server)
