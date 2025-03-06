# Welcome to your new Shiny Cancer Dashboard!
# This file contains the code required to build your dashboard.
# For information on how this code is structured, please visit:
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/

# As diagnoses and deaths is essentially the same, but with different data,
# these files have been "modularised". See here:
# https://shiny.posit.co/r/articles/improve/modules/



# ##### TEMPORARY - TO DELETE #####
# setwd("C:/Users/SeanFrancis/OneDrive - Cancer Council Queensland/Shiny App")
# setwd("C:/Users/JamesREne/OneDrive - Cancer Council Queensland/Desktop/James Braid/CaRDO/Shiny App")
# ##### #####




# Let's now load in the required packages
library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(markdown)




# Then we will "source", or load in the modules
# source("K:/EPI/R-Shiny Projects/CaRDO/Testing/Testing Development/inst/extdata/app_files/module.R", local = TRUE)
# source("K:/EPI/R-Shiny Projects/CaRDO/Testing/Testing Development/inst/extdata/app_files/load_data.R", local = TRUE)
source("module.R", local = TRUE)
source("load_data.R", local = TRUE)

# ##### TEMPORARY - TO DELETE #####
# setwd("K:/EPI/R-Shiny Projects/CaRDO/Testing/Testing Development")
# ##### #####


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
    title = "Diagnosis",
    id = "diagnosis",
    UI_module("Diagnosis")
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
    includeMarkdown("www/methods.md")
  )
)

server <- function(input, output, session){

  server_module("Diagnosis")

  if(!no_mrt){
    server_module("Deaths")
  }
  
  output$report <- downloadHandler(
    filename = ""
  )

}


shinyApp(ui, server)
