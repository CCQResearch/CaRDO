#   Document Package:          'Ctrl + Shift + D'
#   Build Package:              devtools::build()
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



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
# library(mgcv)
# library(strucchange)


#' Create your dashboard
#'
#' @import shiny
#' @import bslib
#' @import shinyWidgets
#' @import dplyr
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom rclipboard rclipboardSetup rclipButton
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
    "disclaimer",
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

  standard_pop <- readRDS(system.file("extdata/stdPopulations_18.RDS", package = "CaRDO"))
  std_pop_names <- unique(standard_pop$std_name)

  addResourcePath("CaRDO", system.file("UX_Styling/www", package = "CaRDO"))

  ## User Interface ----

  ui <- page_fillable(

    # Initial
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "CaRDO/styles.css"),
      tags$script(src = "CaRDO/script.js")
    ),
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
            h1(class = "acronym-title", "CaRDO"),
            div(
              class = "acronym-words",
              span(HTML("<b>Ca</b>ncer")),
              span(HTML("<b>R</b>egistry")),
              span(HTML("<b>D</b>ata")),
              span(HTML("<b>O</b>nline"))
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

      ### UI-Disclaimer ----

      nav_panel_hidden(
        pages[[2]],
        div(
          class = pages[[2]],
          div(
            class = "panel-title",
            p(HTML("Page 1 of ", length(pages)-1)),
            h2("Disclaimer")
          ),
          div(
            class = "panel-body-2",
            div(
              class = "disc-content",
              HTML(
                paste(
                  p("Any data loaded into CaRDO will be stored and processed on your local network. ",
                    "A temporary copy of the data is created for processing and generating the final statistics to be reported. ",
                    "This copy is deleted once the dissemination tool has been generated."),
                  p("We recommend setting the working directory to a local drive (not OneDrive or a cloud system). Information on how to change the working directory is available in the",
                    tags$a("handbook", href = "https://ccqresearch.github.io/CaRDO-Handbook/", target = "_blank"),
                    "under 'Build Your Dashboard'."),
                  p("If you choose to publish the dashboard (e.g. share it online) the data that is uploaded will be the processed version and displayed as it appears in the dashboard. ",
                    "It is your responsibility to ensure that all displayed data is appropriate for sharing before publishing publicly."),
                  p("For the dashboard to work appropriately, there are three key requirements for any cancer dataset that is loaded into CaRDO. ",
                    br(),
                    tags$b("Please read through the requirements outlined on this page before continuing.")),
                  p("Further details on data requirements and building a CaRDO dashboard are available",
                    tags$a("here", href = "https://ccqresearch.github.io/CaRDO-Handbook/", target = "_blank"),
                    ". If you have any other questions or concerns, please reach out to us at",
                    tags$a("statistics@cancerqld.org.au", href = "mailto:statistics@cancerqld.org.au"))
                )
              )
            ),
            div(
              class = "requirements-list",
              div(
                class = "rl-div",
                p("You must have a single column for each variable and outcome you wish to report, and each row in your dataset should correspond to a unique combination of each variable.")
              ),
              div(
                class = "rl-div",
                p("Cancer-type values must be coded as you wish them to be displayed.")
              ),
              div(
                class = "rl-div",
                p("Cancer counts and any population data must be aggregated by 5-year age groups, with age groups coded numerically from 1 – 18.")
              ),
              div(
                class = "understand-button",
                actionButton(inputId = "understand", label = "Okay, I understand")
              )
            )
          )
        )
      ),

      ### UI-Data Load ----

      nav_panel_hidden(
        pages[[3]],
        div(
          class = "content-panel",
          id = pages[[3]],

          div(
            class = "panel-title",
            p(HTML("Page 2 of ", length(pages)-1)),
            h2("Load your dataset."),
            h4("Select the datasets you can provide.")
          ),

          div(
            class = "panel-body-3",
            div(
              class = "select-div",
              checkboxGroupInput(inputId = "select_data",
                                 label = "Please select",
                                 choiceNames = c("Incidence", "Mortality (optional)", "Population (optional)"),
                                 choiceValues = c("Incidence", "Mortality", "Population"),
                                 selected = NULL),
              div(
                class = "hint-div",
                p("For age standarised rates check 'Population' and load a population file then select a standard from the menu!")
              )
            ),
            div(
              class = "upload-div",
              conditionalPanel(
                condition = "input.select_data.includes('Incidence')",
                fileInput(inputId = "data_inc_upload",
                          label = HTML("Load <b>incidence</b> data"),
                          accept = c(".csv", ".tsv", ".dta"))
              ),
              conditionalPanel(
                condition = "input.select_data.includes('Mortality')",
                fileInput(inputId = "data_mrt_upload",
                          label = HTML("Load <b>mortality</b> data"),
                          accept = c(".csv", ".tsv", ".dta"))
              ),
              conditionalPanel(
                condition = "input.select_data.includes('Population')",
                fileInput(inputId = "pop_data_upload",
                          label = HTML("Load <b>population</b> data"),
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
                            selected = "World (WHO) Std Million")

              )
            )
          )
          # div(
          #   class = "panel-base",
          #   div(
          #     p("The data that is loaded here will be copied and deleted once the necessary transformations have taken place. This will leave your original data safe and untouched.")
          #   ),
          #   div(
          #     p("If you would like age standarised rates to be calculated and visualised on the dashboard, please upload your regions population data and select a standard population from the menu.")
          #   )
          # )
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
            h4("These will be parameters that will be carried through to the dashboard.")
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
              sliderInput(inputId = "dashboard_suppression_threshold",
                          label = "Select suppression threshold",
                          min = 1, max = 10,
                          # ticks = FALSE,
                          value = 5,
                          step = 1),
              div(
                class = "hint-div",
                p("If there are counts in your data that are below this value",
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

    is_na_char <- function(x) { x=="NA" }

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

    # Create a bool that keeps track of whether population data has been uploaded
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
      else if (current_page() == "disclaimer"){
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

    #### EVENT: Next Button Clicked ----
    observeEvent(input$next_page, {

      if (current_page_index() == length(pages)) {

        ##### Last Page ----
        withProgress(
          message = "Transforming your data ...",
          detail = "Please wait",
          value = 0,
          {
            #geog.loc_var = if(need_geog()) {input$var_select_pop_geog.loc} else {NULL}

            ##### tryCatch ----
            out <- tryCatch({
              ####### ERROR: Sex Specification ----
              if (input$male_val == input$female_val) {
                stop("Sex specified for male and female Incidence/count data is identical - please select different values")
              }

              ####### ERROR: Sex Specification ----
              if (req_population_data() & length(unique(c(input$male_val_pop, input$female_val_pop))) != 2) {#, input$persons_val_pop))) != 3){
                stop("Sex specified for persons, male and female are duplicated - please select different values")
              }

              ####### Temp directory check ----
              if(!dir.exists("tmp")) {
                dir.create("tmp", recursive = TRUE)
              }

              ####### Save Uploaded Incidence Data ----
              if(!is.null(data_incidence())) {

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

                data_inc$cancer.type <- as.character(data_inc$cancer.type)


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

              ####### Save Uploaded Mortality Data ----
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

                data_mrt$cancer.type <- as.character(data_mrt$cancer.type)

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

              ####### Save Uploaded Population Data ----
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
                  summarise("population" = sum(population),
                            .groups = 'drop') %>%
                  ungroup()

                # Add back in to population file
                data_pop <- data_pop %>% bind_rows(tmp)


                saveRDS(data_pop, "tmp/data_pop.RDS")

              }

              ####### Save Threshold Value ----
              suppress_threshold <- input$dashboard_suppression_threshold

              ####### Save All Other Inputs ----
              supplied_params <- list(
                "All cancers" = if(input$bool_all_canc == "No") {"All cancers"}else{input$all_canc_name},
                "Dashboard title" = input$dashboard_title,
                "Dashboard catchment" = input$dashboard_location,
                "Suppression threshold" = suppress_threshold
              )

              ####### Transform Data ----
              transform_data(
                req_mortality_data(), req_population_data(),
                standard_pop, input$std_pop_name,
                supplied_params,
                #geog.loc_var,
                suppress_threshold)

            },

            error = function(e){e},
            warning = function(w){w}

            ) # tryCatch

            ##### errorhandling ----
            if(is(out, "warning") | is(out, "error")){

              # print(out$message)

              showModal(
                modalDialog(
                  title = "Oops! Something went wrong",
                  "Perhaps variables selected were duplicated."
                )
              )

              unlink("Shiny App")
              unlink("tmp")

            } else {

              confirmSweetAlert(
                type = NULL,
                inputId = "confirm",
                title = HTML("Dashboard Created!"),
                text = tagList(

                  div(
                    class = "directory-copy",
                    tags$p("Copy and run this command into the console (the RStudio window where you opened this app from) to preview your new dashboard. Make sure to hit Exit in this window before running the command in RStudio."),
                    div(
                      class = "file-path",
                      tags$code(id = "r-code", "shiny::runApp(.../Shiny App/app.R)")
                    ),
                    rclipButton(
                      inputId = "clipbtn",
                      label = "Copy to clipboard",
                      clipText = paste0("shiny::runApp('", file.path(getwd(), "Shiny App/app.R"), "')"),
                      icon = icon("clipboard")
                    ),
                    tags$p("Alternatively, the application can be located by navigating to the file path below."),
                    div(
                      class = "file-path",
                      file.path(getwd(), "Shiny App")
                    )
                  )

                ),
                btn_labels = c("Redo", "Exit")
              )

            }

          } # withProgress }
        ) # withProgress )

      } else {

        ##### Next Page ----
        adder <- 1

        # Update the current page index - this will in turn update current_page() when it is called
        if( current_page() == "variable_select_inc"){ # Use the page we're jumping from
          # If BOTH mortality and population data pages are to be skipped, skip both
          if (!req_population_data()  & !req_mortality_data()) {
            adder <- 3
            # Else if we're only skipping mortality, skip mortality
          } else if(!req_mortality_data()) {
            adder <- 2
          }
          # If we're currently on mortality, and DON'T require population, skip that
        } else if(current_page() == "variable_select_mrt"){
          if(!req_population_data()){
            adder <- 2
          }
        }

        current_page_index(current_page_index() + adder)

        nav_select(
          id = "container",
          selected = current_page()
        )

      }
    })

    #### EVENT: Previous Button Clicked ----
    observeEvent(input$previous_page, {

      # Don't execute unless allowed
      req(current_page_index() > 1)

      subtractor <- 1

      # Update the current page index - this will in turn update current_page() when it is called
      if( current_page() == "supplied_params"){ # Use the page we're jumping from
        # If BOTH mortality and population data pages are to be skipped, skip both
        if (!req_population_data()  & !req_mortality_data()) {
          subtractor <- 3
          # Else if we're only skipping population, skip population
        } else if(!req_population_data()) {
          subtractor <- 2
        }
        # If we're currently on population, and DON'T require mortality, skip that
      } else if(current_page() == "variable_select_pop") {
        if(!req_mortality_data()){
          subtractor <- 2
        }
      }

      current_page_index(current_page_index() - subtractor)

      nav_select(
        id = "container",
        selected = current_page()
      )

    })


    #### EVENT: Disabling Next/Previous ----
    observe({

      if(current_page_index() == length(pages)){
        updateActionButton(inputId = "next_page",
                           label = "Create Dashboard")
      }else{
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
      id_inc <- showNotification(paste("Load completed, now processing the data"), duration = 0)

      data <- switch(
        ext,
        "dta" = read_dta(input$data_inc_upload$datapath),
        "csv" = fread(input$data_inc_upload$datapath),
        "tsv" = vroom(input$data_inc_upload$datapath, delim = "\t"),
        validate("Invalid file; Please load a .csv, .tsv or .dta file")
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
      id_mrt <- showNotification(paste("Load completed, now processing the data"), duration = 0)

      data <- switch(
        ext,
        "dta" = read_dta(input$data_mrt_upload$datapath),
        "csv" = fread(input$data_mrt_upload$datapath),
        "tsv" = vroom(input$data_mrt_upload$datapath, delim = "\t"),
        validate("Invalid file; Please load a .csv, .tsv or .dta file")
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
      id_pop <- showNotification(paste("Load completed, now processing the data"), duration = 0)

      data <- switch(
        ext,
        "dta" = read_dta(input$pop_data_upload$datapath),
        "csv" = fread(input$pop_data_upload$datapath),
        "tsv" = vroom(input$pop_data_upload$datapath, delim = "\t"),
        validate("Invalid file; Please load a .csv, .tsv or .dta file")
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
                      NA),
          div(
            class = "hint-div",
            p("Here, we standardise the variable names. Please match your variables with these listed above")
          )
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
                        NA),
            div(
              class = "hint-div",
              p("Here, we standardise the variable names. Please match your variables with these listed above")
            )
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
        h5("Not required, as a mortality file has not been loaded.")
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
                        NA),
            div(
              class = "hint-div",
              p("Here, we standardise the variable names. Please match your variables with these listed above")
            )
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
        h5("Not required, as a population file has not been loaded.")
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
                             selected = "Yes"),
          conditionalPanel(
            condition = "input.bool_all_canc == 'Yes'",
            selectInput(inputId = "all_canc_name",
                        label = "Select the cancer category indicating 'all cancers'",
                        choices = unique(data_incidence()[[input$var_select_inc_cancer.type]]) %>% sort)
          ),
          conditionalPanel(
            condition = "input.bool_all_canc == 'No'",
            div(
              class = "hint-div",
              p("CaRDO automatically adds an 'all reported cancers' category but this will be misleading. If your data does NOT contain an 'all cancers' category please consider including one.")
            )
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
  runApp(shinyApp(ui, server), launch.browser = browserViewer())
  #runApp(shinyApp(ui, server), launch.browser = dialogViewer("", width = 1200, height = 800))

}



#' Transform the data into an appropriate format for the application
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom haven read_dta
#' @author Sean Francis
transform_data <- function(req_mortality_data, req_population_data,
                           standard_pop, std_pop_name,
                           supplied_params,
                           #geog.loc_var,
                           suppress_threshold){

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

  # Check for sex-specific cancers (then later, remove the persons options from them)
  sex_specific_cancers <- data_inc %>%
    group_by(cancer.type) %>%
    summarise("sex_specific" = if_else(length(unique(sex)) == 3, true = 0, false = 1),
              .groups = 'drop') %>%
    filter(sex_specific == 1) %>%
    pull(cancer.type)

  # Remove sex-specific cancers from "persons"
  data_inc <- data_inc %>%
    filter(!(cancer.type %in% sex_specific_cancers & sex == 3))


  if (req_mortality_data){
    data_mrt <- readRDS("tmp/data_mrt.RDS")

    tmp <- data_mrt %>%
      mutate('sex' = 3) %>%
      group_by(across(-c(counts))) %>%
      summarise('counts' = sum(counts),
                .groups = 'drop') %>%
      ungroup()

    data_mrt <- bind_rows(tmp, data_mrt)

    sex_specific_cancers_mrt <- data_mrt %>%
      group_by(cancer.type) %>%
      summarise("sex_specific" = if_else(length(unique(sex)) == 3, true = 0, false = 1),
                .groups = 'drop') %>%
      filter(sex_specific == 1) %>%
      pull(cancer.type)

    # Remove sex-specific cancers from "persons"
    data_mrt <- data_mrt %>%
      filter(!(cancer.type %in% sex_specific_cancers_mrt & sex == 3))
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

  # Generate rates if required ----
  if (req_population_data){

    std_pop <- standard_pop %>%
      filter(std_name == std_pop_name) %>%
      mutate("age.grp" = ragegrp) %>%
      select(age.grp, wght)

    # If there isn't 18 age groups, we need to rescale them to sum to 1
    if(length(unique(data_inc$age.grp)) != 18){
      std_pop <- std_pop %>%
        filter(age.grp %in% unique(data_inc$age.grp)) %>%
        mutate("wght" = wght / sum(wght))
    }

    std_pop_grouped <- std_pop %>%
      merge(age_grps) %>%
      group_by(age.grp_string) %>%
      mutate("wt" = wght / sum(wght))


    ## Incidence ## ----
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


    # Add in trends - need to do for each cancer type, for each sex
    data_inc_annual_tmp <- data.frame()

    for (canc in unique(data_inc_annual$cancer.type)){
      for (sex_i in unique(data_inc_annual$sex)){

        filt_df <- data_inc_annual %>%
          filter(cancer.type == canc,
                 sex == sex_i)

        filt_df_wide <- filt_df %>%
          pivot_wider(names_from = "measure", values_from = "obs")


        tmp_counts <- suppressWarnings(fit_trendline(filt_df_wide, "year", "Counts")) %>%
          select(-c(Rates, ltr)) %>%
          rename("Trend_Counts" = Trend)

        tmp_rates <- suppressWarnings(fit_trendline(filt_df_wide, "year", "Rates")) %>%
          select(-c(Counts, ltr)) %>%
          rename("Trend_Rates" = Trend)

        dat_merge <- merge(tmp_counts, tmp_rates)


        dat <- merge(
          dat_merge %>%
            select(-contains("Trend")) %>%
            pivot_longer(cols = c("Counts", "Rates"),
                         names_to = "measure",
                         values_to = "obs"),

          dat_merge %>%
            select(-c("Counts", "Rates")) %>%
            rename("Counts" = Trend_Counts,
                   "Rates" = Trend_Rates) %>%
            pivot_longer(cols = c("Counts", "Rates"),
                         names_to = "measure",
                         values_to = "obs_trend")
        )

        dat_final <- bind_rows(
          filt_df %>%
            filter(measure == "ltr"),
          dat)

        data_inc_annual_tmp <- bind_rows(data_inc_annual_tmp, dat_final)
      }
    }

    data_inc_annual <- data_inc_annual_tmp %>%
      filter(obs != 0)



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
                "year" = paste0(min(year), "-", max(year)),
                .groups = 'drop') %>%
      merge(std_pop) %>%
      mutate("Rates" = counts / population * wght * 100000) %>%
      rename("Counts" = counts) %>%
      group_by(year, sex, cancer.type) %>%
      summarise("Counts" = sum(Counts)/5,
                "Rates" = sum(Rates),
                .groups = 'drop') %>%
      ungroup() %>%
      pivot_longer(cols = c("Counts", "Rates"),
                   names_to = "measure",
                   values_to = "obs")

    # Remove obs_trend temporarily
    data_inc_annual_sup <- data_inc_annual %>%
      select(-obs_trend)


    ## Implement suppression - pivot wider then longer so that suppression can occur on counts and rates
    data_inc_annual <- data_inc_annual_sup %>%
      pivot_wider(names_from = measure, values_from = obs) %>%
      mutate("suppress" = if_else(Counts < suppress_threshold, true = 1, false = 0),
             "Counts" = if_else(suppress == 1, true = NA, false = Counts),
             "Rates" = if_else(suppress == 1, true = NA, false = Rates)
      ) %>%
      pivot_longer(cols = c("Counts", "Rates", "ltr"),
                   names_to = "measure",
                   values_to = "obs") %>%
      merge(data_inc_annual)

    data_inc_age <- data_inc_age  %>%
      pivot_wider(names_from = measure, values_from = obs) %>%
      mutate("suppress" = if_else(Counts < suppress_threshold, true = 1, false = 0),
             "Counts" = if_else(suppress == 1, true = NA, false = Counts),
             "Rates" = if_else(suppress == 1, true = NA, false = Rates)
      ) %>%
      pivot_longer(cols = c("Counts", "Rates"),
                   names_to = "measure",
                   values_to = "obs")


    incProgress(1/8)


    # Mortality ## ----
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

      # Add in trends - need to do for each cancer type, for each sex
      data_mrt_annual_tmp <- data.frame()

      for (canc in unique(data_mrt_annual$cancer.type)){
        for (sex_i in unique(data_mrt_annual$sex)){

          filt_df <- data_mrt_annual %>%
            filter(cancer.type == canc,
                   sex == sex_i)

          filt_df_wide <- filt_df %>%
            pivot_wider(names_from = "measure", values_from = "obs")


          tmp_counts <- suppressWarnings(fit_trendline(filt_df_wide, "year", "Counts")) %>%
            select(-c(Rates, ltr)) %>%
            rename("Trend_Counts" = Trend)

          tmp_rates <- suppressWarnings(fit_trendline(filt_df_wide, "year", "Rates")) %>%
            select(-c(Counts, ltr)) %>%
            rename("Trend_Rates" = Trend)

          dat_merge <- merge(tmp_counts, tmp_rates)


          dat <- merge(
            dat_merge %>%
              select(-contains("Trend")) %>%
              pivot_longer(cols = c("Counts", "Rates"),
                           names_to = "measure",
                           values_to = "obs"),

            dat_merge %>%
              select(-c("Counts", "Rates")) %>%
              rename("Counts" = Trend_Counts,
                     "Rates" = Trend_Rates) %>%
              pivot_longer(cols = c("Counts", "Rates"),
                           names_to = "measure",
                           values_to = "obs_trend")
          )

          dat_final <- bind_rows(
            filt_df %>%
              filter(measure == "ltr"),
            dat)

          data_mrt_annual_tmp <- bind_rows(data_mrt_annual_tmp, dat_final)
        }
      }

      data_mrt_annual <- data_mrt_annual_tmp %>%
        filter(obs != 0)


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
                  "year" = paste0(min(year), "-", max(year)),
                  .groups = 'drop') %>%
        merge(std_pop) %>%
        mutate("Rates" = counts / population * wght * 100000) %>%
        rename("Counts" = counts) %>%
        group_by(year, sex, cancer.type) %>%
        summarise("Counts" = sum(Counts)/5,
                  "Rates" = sum(Rates),
                  .groups = 'drop') %>%
        ungroup() %>%
        pivot_longer(cols = c("Counts", "Rates"),
                     names_to = "measure",
                     values_to = "obs")

      data_mrt_annual_sup <- data_mrt_annual %>%
        select(-obs_trend)


      ## Implement suppression
      data_mrt_annual <- data_mrt_annual_sup %>%
        pivot_wider(names_from = measure, values_from = obs) %>%
        mutate("suppress" = if_else(Counts < suppress_threshold, true = 1, false = 0),
               "Counts" = if_else(suppress == 1, true = NA, false = Counts),
               "Rates" = if_else(suppress == 1, true = NA, false = Rates)
        ) %>%
        pivot_longer(cols = c("Counts", "Rates", "ltr"),
                     names_to = "measure",
                     values_to = "obs") %>%
        merge(data_mrt_annual)

      data_mrt_age <- data_mrt_age  %>%
        pivot_wider(names_from = measure, values_from = obs) %>%
        mutate("suppress" = if_else(Counts < suppress_threshold, true = 1, false = 0),
               "Counts" = if_else(suppress == 1, true = NA, false = Counts),
               "Rates" = if_else(suppress == 1, true = NA, false = Rates)
        ) %>%
        pivot_longer(cols = c("Counts", "Rates"),
                     names_to = "measure",
                     values_to = "obs")

    }
    incProgress(1/8)
  }

  # If rates are not required ----
  else{
    ## Incidence ## ----
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

    ### Calculate 5 year averages
    data_inc_average <- data_inc_annual %>%
      filter(year >= max(year) - 4) %>%
      group_by(sex, cancer.type, measure) %>%
      summarise("year" = paste0(min(year), "-", max(year)),
                "obs" = sum(obs)/5,
                .groups = 'drop')

    # Add in trends - need to do for each cancer type, for each sex
    data_inc_annual_tmp <- data.frame()

    for (canc in unique(data_inc_annual$cancer.type)){
      for (sex_i in unique(data_inc_annual$sex)){

        filt_df <- data_inc_annual %>%
          filter(cancer.type == canc,
                 sex == sex_i)

        if(nrow(filt_df != 0)){

          tmp_counts <- suppressWarnings(fit_trendline(filt_df, "year", "obs")) %>%
            rename("obs_trend" = Trend)

          data_inc_annual_tmp <- bind_rows(data_inc_annual_tmp, tmp_counts)
        }
      }
    }

    data_inc_annual <- data_inc_annual_tmp %>%
      filter(obs != 0)

    ### Implement suppression
    data_inc_annual <- data_inc_annual %>%
      mutate("suppress" = if_else(measure == "Counts" & obs < suppress_threshold, true = 1, false = 0),
             "obs" = if_else(suppress == 1, true = NA, false = obs))

    data_inc_age <- data_inc_age %>%
      mutate("suppress" = if_else(measure == "Counts" & obs < suppress_threshold, true = 1, false = 0),
             "obs" = if_else(suppress == 1, true = NA, false = obs))

    # Mortality ## ----
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

      ### Calculate 5 year averages
      data_mrt_average <- data_mrt_annual %>%
        filter(year >= max(year) - 4) %>%
        group_by(sex, cancer.type, measure) %>%
        summarise("year" = paste0(min(year), "-", max(year)),
                  "obs" = sum(obs)/5,
                  .groups = 'drop')

      # Add in trends - need to do for each cancer type, for each sex
      data_mrt_annual_tmp <- data.frame()

      for (canc in unique(data_mrt_annual$cancer.type)){
        for (sex_i in unique(data_mrt_annual$sex)){

          filt_df <- data_mrt_annual %>%
            filter(cancer.type == canc,
                   sex == sex_i)

          if(nrow(filt_df) != 0) {
            tmp_counts <- suppressWarnings(fit_trendline(filt_df, "year", "obs")) %>%
              rename("obs_trend" = Trend)

            data_mrt_annual_tmp <- bind_rows(data_mrt_annual_tmp, tmp_counts)
          }
        }
      }

      data_mrt_annual <- data_mrt_annual_tmp %>%
        filter(obs != 0)

      ### Implement suppression
      data_mrt_annual <- data_mrt_annual %>%
        mutate("suppress" = if_else(measure == "Counts" & obs < suppress_threshold, true = 1, false = 0),
               "obs" = if_else(suppress == 1, true = NA, false = obs))

      data_mrt_age <- data_mrt_age %>%
        mutate("suppress" = if_else(measure == "Counts" & obs < suppress_threshold, true = 1, false = 0),
               "obs" = if_else(suppress == 1, true = NA, false = obs))

    }

    incProgress(1/4)

  }


  incProgress(1/4)

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
  # Copy and paste all files in system.file("extdata/app_files", package = "CaRDO")
  # to "Shiny App"
  path <- system.file("extdata/app_files", package = "CaRDO")

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



#' Selects either spline or linear model based on AIC
#'
#' @importFrom mgcv gam
#' @author Sean Francis

choose_best_model <- function(segmented_data, x_val, y_val) {
  # Fit a linear model
  lm_model <- lm(as.formula(paste0(y_val, " ~ ", x_val)), data = segmented_data)

  # Set max number of knots
  # max_knots <- min(10, floor(nrow(segment_data)/2))  # Ensure at least 2 unique values for knots
  max_knots <- 1

  # Fit a spline model
  gam_formula <- as.formula(paste0(y_val, " ~ s(", x_val, ", bs = 'cr', k = ", max_knots, ")"))

  # Fit a spline model
  spline_model <- gam(gam_formula, data = segmented_data)

  # Compare AIC
  aic_values <- c(AIC(lm_model), AIC(spline_model))
  names(aic_values) <- c("linear", "spline")

  best_model <- if(names(which.min(aic_values)) == "linear"){
    lm_model
  }else{
    spline_model
  }

  # Return the model type with the lowest AIC
  return(
    list(
      "type" = names(which.min(aic_values)),
      "model" = best_model
    )
  )
}


#' Fit piecewise model
#'
#' @importFrom dplyr sym
#' @author Sean Francis

fit_piecewise_model_with_selection <- function(data, breakpoints, x_val, y_val) {
  models <- list()
  fit_types <- character(length(breakpoints))

  # Add -Inf and Inf for proper segment handling
  breakpoints <- c(-Inf, breakpoints, Inf)

  for (i in 1:(length(breakpoints) - 1)) {
    # Define segment data
    segmented_data <- data %>%
      # Include the previous year in the predictions
      filter(!!sym(x_val) >= breakpoints[i] & !!sym(x_val) <= breakpoints[i + 1])

    if(nrow(segmented_data) != 1){
      best_model <- choose_best_model(segmented_data, x_val, y_val)

      # Choose the best model for the segment
      fit_types[i] <- best_model$type

      models[[i]] <- best_model$model
    }
  }

  return(list("models" = models, "fit_types" = fit_types))
}


#' Fits trendline to the data
#'
#' @param y_val The y value of the dataset, to be used in regression
#' @param x_val The x value of the dataset, to be used in regression
#' @param data The dataset to regress from
#' @importFrom dplyr filter sym
#' @importFrom magrittr %>%
#' @importFrom strucchange breakpoints
#' @author Sean Francis

fit_trendline <- function(data, x_val, y_val){

  max_joins <- 3

  # Identify the break points
  bp <- breakpoints(as.formula(paste0(y_val, " ~ ", x_val)),
                    data = data,
                    breaks = max_joins,
                    h = 5)

  breaks <- if(any(is.na(bp$breakpoints))){length(data[[x_val]])}else{bp$breakpoints}

  breakpoints <- data[[x_val]][breaks]

  # Fit the piecewise model
  result <- fit_piecewise_model_with_selection(data, breakpoints, x_val, y_val)
  models <- result$models
  fit_types <- result$fit_types

  # Create a new data frame for predictions
  predicted_data <- data.frame("tmp_name" = min(data[[x_val]]):max(data[[x_val]]))
  names(predicted_data) <- x_val
  predicted_data$Trend <- NA  # Initialize Trend with NA

  # Loop through segments and fill predictions
  for (i in 1:length(breakpoints)) {
    # Define the segment for prediction
    if (i == 1) {
      segment_data <- predicted_data %>% filter(!!sym(x_val) <= breakpoints[i])
    } else {
      segment_data <- predicted_data %>% filter(!!sym(x_val) > breakpoints[i - 1] & !!sym(x_val) <= breakpoints[i])
    }

    # Predict values for the current segment using the appropriate model
    if (nrow(segment_data) > 0) {
      segment_data$Trend <- predict(models[[i]], newdata = segment_data)

      # Update the predicted_data with the predictions
      predicted_data[which(predicted_data[[x_val]] > (ifelse(i == 1, -Inf, breakpoints[i - 1])) &
                             predicted_data[[x_val]] <= breakpoints[i]), "Trend"] <- segment_data$Trend
    }
  }

  # Handle the last segment after the last breakpoint
  if (length(breakpoints) > 0) {
    last_segment_data <- predicted_data %>% filter(!!sym(x_val) > breakpoints[length(breakpoints)])
    if(nrow(last_segment_data) > 1){
      last_segment_data$Trend <- predict(models[[length(breakpoints)+1]], newdata = last_segment_data)
      predicted_data[which(predicted_data[[x_val]] > breakpoints[length(breakpoints)]), "Trend"] <- last_segment_data$Trend
    }
  }


  out <- merge(data, predicted_data, by = x_val)

  return(out)
}



