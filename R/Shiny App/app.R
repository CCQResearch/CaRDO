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
    if(length(measure_choices) != 1){
      includeMarkdown("www/methods_rates.md")
    }else{
      includeMarkdown("www/methods_no_rates.md")
    }
  )
)

server <- function(input, output, session){

  server_module("Diagnoses")

  if(!no_mrt){
    server_module("Deaths")
  }

  output$report <- downloadHandler(
    filename = ""
  )

}


shinyApp(ui, server)
