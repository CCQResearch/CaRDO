#   Document Package:          'Ctrl + Shift + D'
#   Build Package:              devtools::build()
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



# library(shiny)
# library(shiny)
# library(bslib)
# library(shinyWidgets)
# library(shinyjs)
# library(tools)
# library(data.table)
# library(vroom)
# library(haven)
# library(tidyverse)
# library(rclipboard)


#' Create your dashboard
#'
#' @import shiny
#' @import bslib
#' @import shinyWidgets
#' @import rclipboard
#' @import dplyr
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom tools file_ext
#' @importFrom data.table fread
#' @importFrom vroom vroom
#' @importFrom haven read_dta
#' @importFrom magrittr %>%
#' @importFrom tidyselect all_of
#' @export
#' @author Sean Francis

create_dashboard <- function(){

  # Increase max file size upload to 700MB
  options(shiny.maxRequestSize = 700*1024^2)

  pages <- list(
    "landing_page",
    "preface",
    "data_upload",
    "variable_select_inc",
    "variable_select_mrt",
    "variable_select_pop",
    "supplied_params"
  )

  data_var_list = c("counts", "year", "cancer type", "sex",
                    "age group"
                    #"geographical location"
  )

  standard_pop <- readRDS(system.file("extdata/stdPopulations_18.RDS", package = "CanDOR"))
  std_pop_names <- unique(standard_pop$std_name)

  addResourcePath("CanDOR", system.file("UX Styling/www", package = "CanDOR"))

  ## User Interface ----

  ui <- page_fillable(

    # Initial
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "CanDOR/styles.css")),
    useShinyjs(),
    rclipboardSetup(),

    # Top panels
    navset_hidden(
      id = "container",

      ### UI-Landing Page ----

      nav_panel_hidden(
        pages[[1]],
        div(
          class = pages[[1]],
          div(
            class = "acronym-container",
            h1(class = "acronym-title", "CanDOR"),
            div(
              class = "acronym-words",
              span(HTML("<b>Can</b>cer")),
              span(HTML("<b>D</b>ata")),
              span(HTML("<b>O</b>nline")),
              span(HTML("<b>R</b>eporting"))
            )
          ),
          div(
            class = "landing-body",
            h5(
              class = "landing-text",
              HTML(
                "Create an interactive dashboard for visualising routinely collected cancer data. Click <b>NEXT</b> to get started."
              )
            )
          )
        )
      ),

      ### UI-Preface ----

      nav_panel_hidden(
        pages[[2]],
        div(
          class = pages[[2]],
          div(
            class = "panel-title",
            p(HTML("Page 1 of ", length(pages)-1)),
            h2("Preface"),
            h6("Here will be an preface page")
          ),
          div(
            # class = "panel-body",
            actionButton(inputId = "understand", label = "Okay, I understand")
          )
        )
      ),

      ### UI-Data Upload ----

      nav_panel_hidden(
        pages[[3]],
        div(
          class = "content-panel",
          id = pages[[3]],

          div(
            class = "panel-title",
            p(HTML("Page 2 of ", length(pages)-1)),
            h2("Upload your dataset."),
            h4("Do you have Incidence, or Mortality data, or both?")
            #h6(HTML("<i>R saves these files to a temporary folder on your computer, which is immediately deleted by R.</i>"))
          ),

          div(
            class = "panel-body-3",
            div(
              class = "select-div",
              checkboxGroupInput(inputId = "select_data",
                                 label = "Please select",
                                 choiceNames = c("Incidence", "Mortality (optional)", "Population (optional)"),
                                 choiceValues = c("Incidence", "Mortality", "Population"),
                                 selected = NULL)
            ),
            div(
              class = "upload-div",
              conditionalPanel(
                condition = "input.select_data.includes('Incidence')",
                fileInput(inputId = "data_inc_upload",
                          label = HTML("Upload <b>incidence</b> data"),
                          accept = c(".csv", ".tsv", ".dta"))
              ),
              conditionalPanel(
                condition = "input.select_data.includes('Mortality')",
                fileInput(inputId = "data_mrt_upload",
                          label = HTML("Upload <b>mortality</b> data"),
                          accept = c(".csv", ".tsv", ".dta"))
              ),
              conditionalPanel(
                condition = "input.select_data.includes('Population')",
                fileInput(inputId = "pop_data_upload",
                          label = HTML("Upload <b>population</b> data"),
                          accept = c(".csv", ".tsv", ".dta"))
              )
            ),
            div(
              class = "menu-div",
              conditionalPanel(
                condition = "input.select_data.includes('Population')",
                selectInput(inputId = "std_pop_name",
                            label = HTML("Select the standard <b>population</b> to use."),
                            choices = std_pop_names,
                            selected = NULL)

              )
            )
          )
        )
      ),

      ### UI-Specify Incidence ----

      nav_panel_hidden(
        pages[[4]],
        div(
          class = "content-panel",
          id = pages[[4]],

          div(
            class = "panel-title",
            p(HTML("Page 3 of ", length(pages)-1)),
            h2("Specify incidence variables."),
            h4("Specify the variables that are present in your", tags$strong("incidence"), "files.")
          ),

          div(
            class = "cond-panels",
            uiOutput(outputId = "select_var_inc")
          )
        )
      ),

      ### UI-Specify Mortality ----

      nav_panel_hidden(
        pages[[5]],
        div(
          class = "content-panel",
          id = pages[[5]],

          div(
            class = "panel-title",
            p(HTML("Page 4 of ", length(pages)-1)),
            h2("Specify mortality variables."),
            h4("Specify the variables that are present in your", tags$strong("mortality"), "files.")
          ),

          div(
            class = "cond-panels",
            uiOutput(outputId = "select_var_mrt")
          )
        )
      ),
      # nav_panel_hidden(
      #   pages[[4]],
      #   div(
      #     class = pages[[4]],
      #     div(
      #       class = "panel-title",
      #       p(HTML("Page 3 of ", length(pages)-1)),
      #       h2("Upload population data."),
      #       h4("This will need to have an age group column that matches the cancer data"),
      #       h6(HTML("<i>This is required for age standardised rates</i>"))
      #     ),
      #     div(
      #       class = "panel-body",
      #       div(
      #         class = "cond-panels",
      #         uiOutput(outputId = "upload_pop")
      #       )
      #     )
      #   )
      # ),

      ### UI-Specify Population ----

      nav_panel_hidden(
        pages[[6]],
        div(
          class = "content-panel",
          id = pages[[6]],

          div(
            class = "panel-title",
            p(HTML("Page 5 of ", length(pages)-1)),
            h2("Specify population variables."),
            h4("Specify the variables that are present in your", tags$strong("population"), "files.")
          ),

          div(
            class = "cond-panels",
            uiOutput(outputId = "select_var_pop")
          )
        )
      ),

      ### UI-Submit Parameters ----

      nav_panel_hidden(
        pages[[7]],
        div(
          class = "content-panel",
          id = pages[[7]],

          div(
            class = "panel-title",
            p(HTML("Page 6 of ", length(pages)-1)),
            h2("Finalising a few things."),
            h6("These will be parameters that will be carried through to the dashboard.")
          ),

          div(
            class = "panel-body-3",
            div(
              id = "text-input-div",
              class = "final-panel-div",
              textInput(inputId = "dashboard_title",
                        label = "What do you wish to title your dashboard?"),
              textInput(inputId = "dashboard_location",
                        label = "Enter your location or catchment, to be displayed in the dashboard.")
            ),
            div(
              id = "all-cancer-category",
              class = "final-panel-div",
              uiOutput(outputId = "select_all_canc_var")
            ),
            div(
              id = "threshold-div",
              class = "final-panel-div",
              # sliderInput(inputId = "dashboard_suppression_threshold",
              #             label = "Select suppression threshold",
              #             min = 5, max = 30,
              #             value = 5,
              #             step = 1),
              sliderInput(inputId = "dashboard_suppression_threshold",
                          label = "Select suppression threshold",
                          min = 5, max = 30,
                          # ticks = FALSE,
                          value = 5,
                          step = 5),
              p(
                paste("If there are counts in your data that are below this value",
                      "they will not show in the dashboard. They are 'suppressed' after all necessary calculations are made.")
              )
            )
          )
        )
      )
    ),
    ### Top panels (END) ###

    ### UI-Navigation ----

    # Navigation
    div(
      class = "nav_div",
      ## Previous button
      div(
        id = "previous_page_div",
        style = "margin: 0 auto;",
        actionButton(inputId = "previous_page",
                     label = "Previous",
                     width = "100%")
      ),
      ## Next button / Save + Exit
      div(
        id = "next_page_div",
        style = "margin: 0 auto;",
        actionButton(inputId = "next_page",
                     label = "Next",
                     width = "100%")
      )
    )
    ### Navigation (END) ###

    # UI END
  )

  ## Server ----

  server <- function(input, output, session){

    is_na_char <- function(x){x=="NA"}

    # need_geog <- reactive(!is_na_char(input$var_select_inc_geog.loc))


    # need_geog <- reactive({
    #   if(is.null(input$variables_inc)){return(FALSE)}
    #   else {return("geographical location" %in% input$variables_inc)}
    # })


    # Initialise the current page index to the first value
    current_page_index <- reactiveVal(1)

    # Assign the current page
    current_page <- reactive({pages[[current_page_index()]]})

    # Create a bool that keeps track of whether mortality data has been selected
    req_mortality_data <- reactive({
      if(is.null(input$select_data)) { return(FALSE) }
      else{ return("Mortality" %in% input$select_data) }
    })

    # Create a bool that keeps track of whether mortality data has been uploaded
    req_population_data <- reactive({
      if(is.null(input$select_data)) { return(FALSE) }
      else{ return("Population" %in% input$select_data) }
    })

    # Assign a boolean reactive that ensures the current page has been completed
    completed_page <- reactive({

      bool <- TRUE

      if (current_page() == "data_upload"){
        bool <- if (req_mortality_data() & !req_population_data()){
          !is.null(data_incidence()) && !is.null(data_mortality())
        }else if (!req_mortality_data() & req_population_data()) {
          !is.null(data_incidence()) && !is.null(data_population())
        } else if (req_mortality_data() & req_population_data()){
          !is.null(data_incidence()) && !is.null(data_mortality()) && !is.null(data_population())
        } else {
          !is.null(data_incidence())
        }
      }
      else if (current_page() == "preface"){
        bool <- input$understand > 0
      }
      else if (current_page() == "variable_select_inc"){
        bool <- !any(
          sapply(
            c(input$var_select_inc_counts,
              input$var_select_inc_year,
              input$var_select_inc_cancer.type,
              input$var_select_inc_sex,
              input$var_select_inc_age.group
              #if(need_geog()){input$var_select_inc_geog.loc}else{NULL}
            ),
            is_na_char
          )
        )
      }
      else if (current_page() == "variable_select_mrt"){
        if(req_mortality_data()){
          bool <- !any(
            sapply(
              c(input$var_select_mrt_counts,
                input$var_select_mrt_year,
                input$var_select_mrt_cancer.type,
                input$var_select_mrt_sex,
                input$var_select_mrt_age.group
                #if(need_geog()){input$var_select_mrt_geog.loc}else{NULL}
              ),
              is_na_char
            )
          )
        }else{
          bool <- TRUE
        }
      }
      # Population file is optional
      else if (current_page() == "variable_select_pop"){

        bool <- if(req_population_data()){
          !any(
            sapply(
              c(input$var_select_pop_counts,
                input$var_select_,
                input$var_select_pop_cancer.type,
                input$var_select_pop_sex,
                input$var_select_pop_age.group
                #if(need_geog()){input$var_select_pop_geog.loc}else{NULL}
              ),
              is_na_char
            )
          )
        }else{
          bool <- TRUE
        }
      }else if (current_page() == "supplied_params"){
        bool <- trimws(input$dashboard_title) != "" && trimws(input$dashboard_location) != ""
      }

      return(bool)
    })

    # observe(print(current_page()))

    ### Observe Event Section ----

    # Change the page when the "next" button is clicked
    observeEvent(input$next_page, {

      if (current_page_index() == length(pages)){

        withProgress(
          message = "Transforming your data ...",
          detail = "Please wait",
          value = 0,
          {

            #geog.loc_var = if(need_geog()){input$var_select_pop_geog.loc}else{NULL}

            out <- tryCatch(
              {

                if (input$male_val == input$female_val){
                  stop("Sex specified for male and female Incidence/count data is identical - please select different values")
                }

                if (req_population_data() & length(unique(c(input$male_val_pop, input$female_val_pop))) != 2){#, input$persons_val_pop))) != 3){
                  stop("Sex specified for persons, male and female are duplicated - please select different values")
                }


                if(!dir.exists("tmp")){
                  dir.create("tmp", recursive = TRUE)
                }

                # Save uploaded data
                if(!is.null(data_incidence())){

                  cols_inc <- c("year", "cancer.type", "sex", "age.grp",
                                #"geog.loc",
                                "counts")

                  names(cols_inc) <- c(input$var_select_inc_year,
                                       input$var_select_inc_cancer.type,
                                       input$var_select_inc_sex,
                                       input$var_select_inc_age.group,
                                       #input$var_select_inc_geog.loc,
                                       input$var_select_inc_counts)

                  cols_inc <- cols_inc[names(cols_inc) != "NA"]

                  data_inc <- data_incidence() %>%
                    select(all_of(names(cols_inc)))

                  names(data_inc)[names(data_inc) %in% names(cols_inc)] <- unname(cols_inc)

                  data_inc <- data_inc %>%
                    mutate(
                      "sex" = case_when(
                        sex == input$male_val ~ 1,
                        sex == input$female_val ~ 2,
                        .default = NA
                      )
                    ) %>%
                    filter(!is.na(sex))


                  # Generate all cancers if not given
                  if(input$bool_all_canc == "No"){
                    tmp <- data_inc %>%
                      mutate("cancer.type" = "All cancers") %>%
                      group_by(across(-counts)) %>%
                      summarise("counts" = sum(counts),
                                .groups = 'drop')

                    data_inc <- bind_rows(data_inc, tmp)
                  }

                  saveRDS(data_inc, "tmp/data_inc.RDS")

                }

                if(!is.null(data_mortality())){

                  cols_mrt <- c("year", "cancer.type", "sex", "age.grp",
                                #"geog.loc",
                                "counts")

                  names(cols_mrt) <- c(input$var_select_mrt_year,
                                       input$var_select_mrt_cancer.type,
                                       input$var_select_mrt_sex,
                                       input$var_select_mrt_age.group,
                                       #input$var_select_mrt_geog.loc,
                                       input$var_select_mrt_counts)

                  cols_mrt <- cols_mrt[names(cols_mrt) != "NA"]


                  data_mrt <- data_mortality() %>%
                    select(all_of(names(cols_mrt)))

                  names(data_mrt)[names(data_mrt) %in% names(cols_mrt)] <- cols_mrt


                  data_mrt <- data_mrt %>%
                    mutate(
                      "sex" = case_when(
                        sex == input$male_val ~ 1,
                        sex == input$female_val ~ 2,
                        .default = NA
                      )
                    ) %>%
                    filter(!is.na(sex))

                  # Generate all cancers if not given
                  if(input$bool_all_canc == "No"){
                    tmp <- data_mrt %>%
                      mutate("cancer.type" = "All cancers") %>%
                      group_by(across(-counts)) %>%
                      summarise("counts" = sum(counts),
                                .groups = 'drop')

                    data_mrt <- bind_rows(data_mrt, tmp)
                  }

                  saveRDS(data_mrt, "tmp/data_mrt.RDS")
                }

                # Save uploaded data
                if(!is.null(data_population())){

                  cols_pop <- c("year", "sex", "age.grp",
                                #"geog.loc",
                                "population")

                  names(cols_pop) <- c(input$var_select_pop_year,
                                       input$var_select_pop_sex,
                                       input$var_select_pop_age.group,
                                       #ifelse(is.null(input$var_select_pop_geog.loc), "NA", input$var_select_pop_geog.loc),
                                       input$var_select_pop_population)

                  cols_pop <- cols_pop[names(cols_pop) != "NA"]


                  data_pop <- data_population() %>%
                    select(all_of(names(cols_pop)))


                  names(data_pop)[names(data_pop) %in% names(cols_pop)] <- cols_pop


                  data_pop <- data_pop %>%
                    mutate(
                      "sex" = case_when(
                        sex == input$male_val_pop ~ 1,
                        sex == input$female_val_pop ~ 2,
                        .default = NA
                        # sex == input$persons_val_pop ~ 3
                      )
                    ) %>%
                    filter(!is.na(sex))

                  # Generate sex == 3
                  tmp <- data_pop %>%
                    mutate("sex" = 3) %>%
                    group_by(across(-population)) %>%
                    summarise("population" = sum(population)) %>%
                    ungroup()

                  # Add back in to population file
                  data_pop <- data_pop %>% bind_rows(tmp)


                  saveRDS(data_pop, "tmp/data_pop.RDS")

                }

                suppress_threshold <- input$dashboard_suppression_threshold

                supplied_params <- list(
                  "All cancers" = if(input$bool_all_canc == "No") {"All cancers"}else{input$all_canc_name},
                  "Dashboard title" = input$dashboard_title,
                  "Dashboard catchment" = input$dashboard_location,
                  "Suppression threshold" = suppress_threshold
                )

                transform_data(
                  req_mortality_data(), req_population_data(),
                  standard_pop, input$std_pop_name,
                  supplied_params,
                  #geog.loc_var,
                  suppress_threshold)

              },
              error = function(e){e},
              warning = function(w){w}
            )

            if(is(out, "warning") | is(out, "error")){
              showModal(
                modalDialog(
                  title = "Oops! Something went wrong",
                  "Perhaps variables selected were duplicated."
                )
              )
              unlink("Shiny App")
              unlink("tmp")
            }else{
              confirmSweetAlert(
                type = NULL,
                inputId = "confirm",
                title = "Congratulations! Your Shiny app can be found below.",
                text = tagList(
                  div(
                    file.path(getwd(), "Shiny App"),
                    rclipButton(
                      inputId = "clipbtn",
                      label = "Copy to clipboard",
                      clipText = file.path(getwd(), "Shiny App"),
                      icon = icon("clipboard")
                    )
                  )
                ),
                btn_labels = c("Redo", "Exit")
              )
            }
          }
        )


      }else{

        # Update the current page index - this will in turn update current_page() when it is called
        current_page_index(current_page_index()+1)

        nav_select(
          id = "container",
          selected = current_page()
        )

      }
    })

    observeEvent(input$previous_page, {

      # Don't execute unless allowed
      req(current_page_index() > 1)

      # Update the current page index - this will in turn update current_page() when it is called
      current_page_index(current_page_index()-1)

      nav_select(
        id = "container",
        selected = current_page()
      )

    })


    # Disable/enable next/previous page when needed
    observe({

      if(current_page_index() == length(pages)){
        updateActionButton(inputId = "next_page",
                           label = "Save and exit")
      }else if (current_page_index()+1 == length(pages)){
        updateActionButton(inputId = "next_page",
                           label = "Next")
      }

      if (current_page_index() > 1){
        enable("previous_page")
      }else{
        disable("previous_page")
      }

      if (!completed_page()){
        disable("next_page")
      }else{
        enable("next_page")
      }

    })

    ### Data Section ----

    #### Incidence ----

    data_incidence <- reactive({

      if(is.null(input$data_inc_upload)){
        return(NULL)
      }

      ext <- file_ext(input$data_inc_upload$name)

      # Save the ID for removal later
      id_inc <- showNotification(paste("Upload completed, now processing the data"), duration = 0)

      data <- switch(
        ext,
        "dta" = read_dta(input$data_inc_upload$datapath),
        "csv" = fread(input$data_inc_upload$datapath),
        "tsv" = vroom(input$data_inc_upload$datapath, delim = "\t"),
        validate("Invalid file; Please upload a .csv, .tsv or .dta file")
      )

      removeNotification(id_inc)

      return(data)

    })

    #### Mortality ----

    data_mortality <- reactive({

      if(is.null(input$data_mrt_upload)){
        return(NULL)
      }

      ext <- file_ext(input$data_mrt_upload$name)

      # Save the ID for removal later
      id_mrt <- showNotification(paste("Upload completed, now processing the data"), duration = 0)

      data <- switch(
        ext,
        "dta" = read_dta(input$data_mrt_upload$datapath),
        "csv" = fread(input$data_mrt_upload$datapath),
        "tsv" = vroom(input$data_mrt_upload$datapath, delim = "\t"),
        validate("Invalid file; Please upload a .csv, .tsv or .dta file")
      )

      removeNotification(id_mrt)

      return(data)


    })

    #### Population ----

    data_population <- reactive({

      if(is.null(input$pop_data_upload)){
        return(NULL)
      }

      ext <- file_ext(input$pop_data_upload$name)

      # Save the ID for removal later
      id_pop <- showNotification(paste("Upload completed, now processing the data"), duration = 0)

      data <- switch(
        ext,
        "dta" = read_dta(input$pop_data_upload$datapath),
        "csv" = fread(input$pop_data_upload$datapath),
        "tsv" = vroom(input$pop_data_upload$datapath, delim = "\t"),
        validate("Invalid file; Please upload a .csv, .tsv or .dta file")
      )

      removeNotification(id_pop)

      return(data)

    })

    ### Outputs Section ----

    #### Specify Incidence Outputs ----

    output$select_var_inc <- renderUI({
      variable_names <- c(NA, names(data_incidence()))
      tagList(
        # Check-Panel
        # div(
        #   class = "checkbox-panel",
        #   checkboxGroupInput(
        #     inputId = "variables_inc",
        #     label = "Does your data have:",
        #     # choices = data_var_list,
        #     choices = "geographical location",
        #     NA)
        # ),
        # Menu-Panel
        div(
          class = "menu-inline",
          selectInput(inputId = "var_select_inc_counts",
                      choices = variable_names,
                      label = HTML("Select <b>count</b> variable"),
                      NA),

          selectInput(inputId = "var_select_inc_year",
                      choices = variable_names,
                      label = HTML("Select <b>year</b> variable"),
                      NA),
          selectInput(inputId = "var_select_inc_cancer.type",
                      choices = variable_names,
                      label = HTML("Select <b>cancer name</b> variable"),
                      NA),
          selectInput(inputId = "var_select_inc_age.group",
                      choices = variable_names,
                      label = HTML("Select <b>age group</b> variable"),
                      NA),
          selectInput(inputId = "var_select_inc_sex",
                      choices = variable_names,
                      label = HTML("Select <b>sex</b> variable"),
                      NA)
          # conditionalPanel(
          #   condition = "input.variables_inc.includes('geographical location')",
          #   selectInput(inputId = "var_select_inc_geog.loc",
          #               choices = variable_names,
          #               label = "Select 'geographical location' variable",
          #               NA)
          # )
        ),
        div(
          class = "menu-div",
          conditionalPanel(
            condition = "input.var_select_inc_sex != 'NA'",
            uiOutput(outputId = "select_var_inc_sex")
          )
        )
      )
    })

    output$select_var_inc_sex <- renderUI({
      tagList(
        selectInput(inputId = "male_val",
                    label = HTML("Which value in your 'sex' column represents <b>males</b>?"),
                    choices = unique(data_incidence()[[input$var_select_inc_sex]])),
        selectInput(inputId = "female_val",
                    label = HTML("Which value in your 'sex' column represents <b>females</b>?"),
                    choices = unique(data_incidence()[[input$var_select_inc_sex]]))
      )
    })

    #### Specify Mortality Outputs ----

    output$select_var_mrt <- renderUI({
      if(req_mortality_data()){
        variable_names <- c(NA, names(data_mortality()))
        tagList(
          # Menu-Panel
          div(
            class = "menu-inline",
            selectInput(inputId = "var_select_mrt_counts",
                        choices = variable_names,
                        label = HTML("Select <b>count</b> variable"),
                        NA),
            selectInput(inputId = "var_select_mrt_year",
                        choices = variable_names,
                        label = HTML("Select <b>year</b> variable"),
                        NA),
            selectInput(inputId = "var_select_mrt_cancer.type",
                        choices = variable_names,
                        label = HTML("Select <b>cancer name</b> variable"),
                        NA),
            selectInput(inputId = "var_select_mrt_age.group",
                        choices = variable_names,
                        label = HTML("Select <b>age group</b> variable"),
                        NA),
            selectInput(inputId = "var_select_mrt_sex",
                        choices = variable_names,
                        label = HTML("Select <b>sex</b> variable"),
                        NA)
            # conditionalPanel(
            #   condition = "input.variables_inc.includes('geographical location')",
            #   selectInput(inputId = "var_select_mrt_geog.loc",
            #               choices = variable_names,
            #               label = "Select 'geographical location' variable",
            #               NA)
            # )
          ),
          div(
            class = "menu-div",
            conditionalPanel(
              condition = "input.var_select_mrt_sex != 'NA'",
              uiOutput(outputId = "select_var_mrt_sex")
            )
          )
        )
      }else{
        h5("Not required, as a mortality file has not been uploaded.")
      }
    })

    output$select_var_mrt_sex <- renderUI({
      tagList(
        selectInput(inputId = "male_val",
                    label = HTML("Which value in your 'sex' column represents <b>males</b>?"),
                    choices = unique(data_mortality()[[input$var_select_mrt_sex]])),
        selectInput(inputId = "female_val",
                    label = HTML("Which value in your 'sex' column represents <b>females</b>?"),
                    choices = unique(data_mortality()[[input$var_select_mrt_sex]]))
      )
    })


    # output$select_std_pop <- renderUI({
    #   if(req_population_data()){
    #
    #   }
    # })

    #### Specify Population Outputs ----

    output$select_var_pop <- renderUI({
      variable_names <- c(NA, names(data_population()))
      # required_vars <- c('population', 'year', 'age group', 'sex', 'geographical location')
      if(req_population_data()){
        tagList(
          div(
            class = "menu-inline",
            selectInput(inputId = "var_select_pop_population",
                        choices = variable_names,
                        label = HTML("Select <b>population</b> variable"),
                        NA),
            condition = "input.variables_pop.includes('year')",
            selectInput(inputId = "var_select_pop_year",
                        choices = variable_names,
                        label = HTML("Select <b>year</b> variable"),
                        NA),
            selectInput(inputId = "var_select_pop_age.group",
                        choices = variable_names,
                        label = HTML("Select <b>age group</b> variable"),
                        NA),
            selectInput(inputId = "var_select_pop_sex",
                        choices = variable_names,
                        label = HTML("Select <b>sex</b> variable"),
                        NA)
            # conditionalPanel(
            #   condition = "input.variables_inc.includes('geographical location')",
            #   selectInput(inputId = "var_select_pop_geog.loc",
            #               choices = variable_names,
            #               label = "Select 'geographical location' variable",
            #               NA)
            #
            # )
          ),
          div(
            class = "menu-div",
            conditionalPanel(
              condition = "input.var_select_pop_sex != 'NA'",
              uiOutput(outputId = "select_var_pop_sex")
            )
          )

        )
      }else{
        h5("Not required, as a population file has not been uploaded.")
      }
    })

    output$select_var_pop_sex <- renderUI({
      tagList(
        selectInput(inputId = "male_val_pop",
                    label = HTML("Which value in your 'sex' column represents <b>males?</b>"),
                    choices = unique(data_population()[[input$var_select_pop_sex]])),
        selectInput(inputId = "female_val_pop",
                    label = HTML("Which value in your 'sex' column represents <b>females?</b>"),
                    choices = unique(data_population()[[input$var_select_pop_sex]]))
        # ,
        # selectInput(inputId = "persons_val_pop",
        #             label = HTML("Which value in your 'sex' column represents <b>persons?</b>"),
        #             choices = unique(data_population()[[input$var_select_pop_sex]]))
      )
    })


    output$select_all_canc_var <- renderUI({
      tagList(
        div(
          class = "panel-body",
          prettyRadioButtons(inputId = "bool_all_canc",
                             label = "Do you have a cancer category indicating all cancers?",
                             choices = c("No", "Yes"),
                             shape = "square",
                             icon = icon("check"),
                             selected = "No"),
          conditionalPanel(
            condition = "input.bool_all_canc == 'Yes'",
            selectInput(inputId = "all_canc_name",
                        label = "Select the cancer category indicating 'all cancers'",
                        choices = unique(data_incidence()[[input$var_select_inc_cancer.type]]))
          )
        )
      )
    })


    # Ensure if "geographical location" is not selected in input$variables_inc,
    # then set input$var_select_pop_geog.loc to NA
    # observe({
    #   # print(input$variables_inc)
    #   if(is.null(input$variables_inc)){
    #     updateSelectInput(session = session,
    #                       inputId = "var_select_inc_geog.loc",
    #                       selected = "NA")
    #
    #   }
    # })

    observeEvent(input$clipbtn, {
      updateActionButton(session, inputId = "clipbtn",
                         label = "Copied!")
    })


    # Finalise everything, and exit
    observeEvent(input$confirm, {
      if(input$confirm){stopApp()}
    })

  }

  # shinyApp(ui, server)
  runApp(shinyApp(ui, server), launch.browser = dialogViewer("", width = 1200, height = 800))

}



#' Transform the data into an appropriate format for the application
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom haven read_dta
#' @author Sean Francis
transform_data <- function(req_mortality_data, req_population_data,
                           standard_pop, std_pop_name,
                           supplied_params,
                           #geog.loc_var,
                           suppress_threshold){

  # browser()


  output_path <- "Shiny App/Data"

  # Delete the directory if it exists
  if(dir.exists(output_path)){
    unlink(output_path, recursive = TRUE)
  }

  # Create a new one
  dir.create(output_path, recursive = TRUE)

  # Save the inputs to a variable
  saveRDS(
    supplied_params,
    file.path(output_path, "supplied_params.RDS")
  )


  # Load in the data
  data_inc <- readRDS("tmp/data_inc.RDS")

  tmp <- data_inc %>%
    mutate('sex' = 3) %>%
    group_by(across(-c(counts))) %>%
    summarise('counts' = sum(counts),
              .groups = 'drop') %>%
    ungroup()

  data_inc <- bind_rows(tmp, data_inc)

  if (req_mortality_data){
    data_mrt <- readRDS("tmp/data_mrt.RDS")

    tmp <- data_mrt %>%
      mutate('sex' = 3) %>%
      group_by(across(-c(counts))) %>%
      summarise('counts' = sum(counts),
                .groups = 'drop') %>%
      ungroup()

    data_mrt <- bind_rows(tmp, data_mrt)
  }

  if (req_population_data){
    data_pop <- readRDS("tmp/data_pop.RDS")
  }

  # Delete the temporary directory
  unlink("tmp", recursive = TRUE)


  incProgress(1/4)

  # Group data by age, to smaller age groups
  age_grps <- list("0-34" = 1:7, "35-49" = 8:10,
                   "50-64" = 11:13, "65+" = 14:18) %>%
    stack() %>%
    rename("age.grp" = values,
           "age.grp_string" = ind)


  # Generate rates if required
  if (req_population_data){

    std_pop <- standard_pop %>%
      filter(std_name == std_pop_name) %>%
      mutate("age.grp" = ragegrp) %>%
      select(age.grp, wght)

    std_pop_grouped <- std_pop %>%
      merge(age_grps) %>%
      group_by(age.grp_string) %>%
      mutate("wt" = wght / sum(wght))


    ## Incidence ##
    data_inc_pop <- data.frame()

    for (canc in unique(data_inc$cancer.type)){
      tmp_df <- data_inc %>%
        filter(cancer.type == canc)

      tmp_df2 <- data_pop %>%
        left_join(tmp_df,
                  by = c("year", "sex", "age.grp"
                         #geog.loc_var
                  )
        ) %>%
        mutate("counts" = if_else(is.na(counts), true = 0, false = counts))

      tmp_df2$cancer.type <- canc

      data_inc_pop <- bind_rows(data_inc_pop, tmp_df2)
    }


    data_inc_annual <- data_inc_pop %>%
      merge(std_pop) %>%
      mutate(
        "rates" = counts / population * wght * 100000,
        "agspfc" = if_else(age.grp == 18, true = 0, false = counts / population * 100000)
      ) %>%
      group_by(year, sex, cancer.type) %>%
      summarise("Counts" = sum(counts),
                "Rates" = sum(rates),
                "agspfc" = sum(agspfc),
                .groups = 'drop') %>%
      mutate("cum_rate" = 5 * agspfc * 100 / 100000,
             "ltr" = 1 / 1 - exp(-cum_rate/100)) %>%
      select(-c(agspfc, cum_rate)) %>%
      pivot_longer(cols = c("Counts", "Rates", "ltr"),
                   names_to = "measure",
                   values_to = "obs")

    # By age
    data_inc_age <- data_inc_pop %>%
      merge(std_pop_grouped) %>%
      mutate("Rates" = counts / population * wt * 100000) %>%
      rename("Counts" = counts) %>%
      pivot_longer(cols = c("Counts", "Rates"),
                   names_to = "measure",
                   values_to = "obs") %>%
      select(-population) %>%
      merge(age_grps) %>%
      group_by(across(-c(obs, age.grp, wght, wt))) %>%
      summarise("obs" = sum(obs),
                .groups = 'drop')

    ### Calculate 5 year averages
    data_inc_average <- data_inc_pop %>%
      filter(year >= max(year) - 4) %>%
      group_by(sex, age.grp, cancer.type) %>%
      summarise("counts" = sum(counts),
                "population" = sum(population),
                "year" = paste0(min(year), "-", max(year))) %>%
      merge(std_pop) %>%
      mutate("Rates" = counts / population * wght * 100000) %>%
      rename("Counts" = counts) %>%
      group_by(year, sex, cancer.type) %>%
      summarise("Counts" = sum(Counts)/5,
                "Rates" = sum(Rates)) %>%
      ungroup() %>%
      pivot_longer(cols = c("Counts", "Rates"),
                   names_to = "measure",
                   values_to = "obs")


    incProgress(1/8)


    ## Mortality ##
    if (req_mortality_data){
      data_mrt_pop <- data.frame()

      for (canc in unique(data_mrt$cancer.type)){
        tmp_df <- data_mrt %>%
          filter(cancer.type == canc)

        tmp_df2 <- data_pop %>%
          left_join(tmp_df,
                    by = c("year", "sex", "age.grp"
                           #geog.loc_var
                    )
          ) %>%
          mutate("counts" = if_else(is.na(counts), true = 0, false = counts))

        tmp_df2$cancer.type <- canc

        data_mrt_pop <- bind_rows(data_mrt_pop, tmp_df2)
      }

      data_mrt_annual <- data_mrt_pop %>%
        merge(std_pop) %>%
        mutate(
          "rates" = counts / population * wght * 100000,
          "agspfc" = if_else(age.grp == 18, true = 0, false = counts / population * 100000)
        ) %>%
        group_by(year, sex, cancer.type) %>%
        summarise("Counts" = sum(counts),
                  "Rates" = sum(rates),
                  "agspfc" = sum(agspfc),
                  .groups = 'drop') %>%
        mutate("cum_rate" = 5 * agspfc * 100 / 100000,
               "ltr" = 1 / 1 - exp(-cum_rate/100)) %>%
        select(-c(agspfc, cum_rate)) %>%
        pivot_longer(cols = c("Counts", "Rates", "ltr"),
                     names_to = "measure",
                     values_to = "obs")


      # By age
      data_mrt_age <- data_mrt_pop %>%
        merge(std_pop_grouped) %>%
        mutate("Rates" = counts / population * wt * 100000) %>%
        rename("Counts" = counts) %>%
        pivot_longer(cols = c("Counts", "Rates"),
                     names_to = "measure",
                     values_to = "obs") %>%
        select(-population) %>%
        merge(age_grps) %>%
        group_by(across(-c(obs, age.grp, wght, wt))) %>%
        summarise("obs" = sum(obs),
                  .groups = 'drop')


      ### Calculate 5 year averages
      data_mrt_average <- data_mrt_pop %>%
        filter(year >= max(year) - 4) %>%
        group_by(sex, age.grp, cancer.type) %>%
        summarise("counts" = sum(counts),
                  "population" = sum(population),
                  "year" = paste0(min(year), "-", max(year))) %>%
        merge(std_pop) %>%
        mutate("Rates" = counts / population * wght * 100000) %>%
        rename("Counts" = counts) %>%
        group_by(year, sex, cancer.type) %>%
        summarise("Counts" = sum(Counts)/5,
                  "Rates" = sum(Rates)) %>%
        ungroup() %>%
        pivot_longer(cols = c("Counts", "Rates"),
                     names_to = "measure",
                     values_to = "obs")

    }
    incProgress(1/8)
  }
  # If rates are not required
  else{
    data_inc_annual <- data_inc %>%
      group_by(year, sex, cancer.type) %>%
      summarise("obs" = sum(counts),
                "measure" = "Counts",
                .groups = 'drop')

    data_inc_age <- data_inc %>%
      mutate("obs" = counts,
             "measure" = "Counts") %>%
      merge(age_grps) %>%
      group_by(across(-c(obs, age.grp, counts))) %>%
      summarise("obs" = sum(obs),
                .groups = 'drop')

    if (req_mortality_data){

      data_mrt_annual <- data_mrt %>%
        group_by(year, sex, cancer.type) %>%
        summarise("obs" = sum(counts),
                  "measure" = "Counts",
                  .groups = 'drop')

      data_mrt_age <- data_mrt %>%
        mutate("obs" = counts,
               "measure" = "Counts") %>%
        merge(age_grps) %>%
        group_by(across(-c(obs, age.grp, counts))) %>%
        summarise("obs" = sum(obs),
                  .groups = 'drop')

    }

    incProgress(1/4)

  }


  incProgress(1/4)

  # ### Calculate 5 year averages
  # data_inc_average <- data_inc_annual %>%
  #   filter(year >= max(year) - 4,
  #          measure == "Counts") %>%
  #   group_by(sex, cancer.type) %>%
  #   summarise("year" = paste0(min(year), "-", max(year)),
  #             "obs" = mean(obs)) %>%
  #   bind_rows(data_inc_average_rates)
  #
  # if (req_mortality_data) {
  #   data_mrt_average <- data_mrt_annual %>%
  #     filter(year >= max(year) - 4,
  #            measure == "Counts") %>%
  #     group_by(sex, cancer.type) %>%
  #     summarise("year" = paste0(min(year), "-", max(year)),
  #               "obs" = mean(obs)) %>%
  #     bind_rows(data_mrt_average_rates)
  # }

  ### Implement suppression
  data_inc_annual <- data_inc_annual %>%
    mutate("suppress" = if_else(measure == "Counts" & obs < suppress_threshold, true = 1, false = 0),
           "obs" = if_else(suppress == 1, true = NA, false = obs))

  data_inc_age <- data_inc_age %>%
    mutate("suppress" = if_else(measure == "Counts" & obs < suppress_threshold, true = 1, false = 0),
           "obs" = if_else(suppress == 1, true = NA, false = obs))

  write.csv(data_inc_annual,
            file.path(output_path, "data_inc_annual.csv"),
            row.names = FALSE)

  write.csv(data_inc_age,
            file.path(output_path, "data_inc_age.csv"),
            row.names = FALSE)

  write.csv(data_inc_average,
            file.path(output_path, "data_inc_average.csv"),
            row.names = FALSE)



  if (req_mortality_data){

    ### Implement suppression
    data_mrt_annual <- data_mrt_annual %>%
      mutate("suppress" = if_else(measure == "Counts" & obs < suppress_threshold, true = 1, false = 0),
             "obs" = if_else(suppress == 1, true = NA, false = obs))

    data_mrt_age <- data_mrt_age %>%
      mutate("suppress" = if_else(measure == "Counts" & obs < suppress_threshold, true = 1, false = 0),
             "obs" = if_else(suppress == 1, true = NA, false = obs))


    write.csv(data_mrt_annual,
              file.path(output_path, "data_mrt_annual.csv"),
              row.names = FALSE)

    write.csv(data_mrt_age,
              file.path(output_path, "data_mrt_age.csv"),
              row.names = FALSE)

    write.csv(data_mrt_average,
              file.path(output_path, "data_mrt_average.csv"),
              row.names = FALSE)
  }

  # At the end
  from_dir <- "Shiny App"
  # Copy and paste all files in system.file("extdata/app_files", package = "CanDOR")
  # to "Shiny App"
  path <- system.file("extdata/app_files", package = "CanDOR")

  for( item in list.files(path)) {

    from <- file.path(path, item)

    # If the item to copy across is a folder, and not a file
    if(!file_test("-f", from)){
      to <- from_dir
      to_recurse <- TRUE
    }else{
      to <- file.path(from_dir, item)
      to_recurse <- FALSE
    }

    file.copy(from, to, recursive = to_recurse,
              overwrite = TRUE)
  }

  incProgress(1/4)

}



