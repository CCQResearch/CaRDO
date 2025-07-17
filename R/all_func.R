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
# library(strucchange)
# library(segmented)


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
            div(class = "logo"),
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
                    tags$a("here", href = "https://ccqresearch.github.io/CaRDO-Handbook/data-requirements.html", target = "_blank"),
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
            h2("Load your datasets."),
            h4("Select all the datasets you can provide.")
          ),

          div(
            class = "panel-body-3",
            div(
              class = "select-div",
              checkboxGroupInput(inputId = "select_data",
                                 label = "Please select",
                                 choiceNames = c("Incidence (required)", "Mortality (optional)", "Population (optional)"),
                                 choiceValues = c("Incidence", "Mortality", "Population"),
                                 selected = NULL),
              div(
                class = "hint-div",
                p("For age standarised rates check 'Population' and load a population file then select a standard from the menu")
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
                        label = "What do you wish to title your dashboard?",
                        placeholder = "eg. Registry Name"),
              textInput(inputId = "dashboard_location",
                        label = "Which region does your data represent? This will be displayed on the dashboard.",
                        placeholder = "eg. Queensland")
            ),
            div(
              id = "all-cancer-category",
              class = "final-panel-div",
              uiOutput(outputId = "select_all_canc_var"),
              uiOutput(outputId = "aggregation_var")
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
        conditionalPanel(
          condition = "!output.to_hide_previous_btn",
          actionButton(inputId = "previous_page",
                       label = "Previous",
                       width = "100%")
        )
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
        bool <- (trimws(input$dashboard_title) != "" && trimws(input$dashboard_location) != "") &
          (if (input$bool_all_canc == "Yes") {input$all_canc_name != "Please specify .."}else{TRUE})
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
            ####### Temp directory check ----
            if(!dir.exists("tmp")) {
              success <- dir.create("tmp", recursive = TRUE)
              if (!success) stop("Failed to create directory: Check Permissions")
            }

            ####### Save Uploaded Incidence Data ----
            withCallingHandlers({
              if(!is.null(data_incidence())) {

                cols_inc <- c("year", "cancer.type", "sex", "age.grp", "counts")

                names(cols_inc) <- c(input$var_select_inc_year,
                                     input$var_select_inc_cancer.type,
                                     input$var_select_inc_sex,
                                     input$var_select_inc_age.group,
                                     input$var_select_inc_counts)

                cols_inc <- cols_inc[names(cols_inc) != "NA"]

                data_inc <- data_incidence() %>%
                  dplyr::select(all_of(names(cols_inc)))

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
                    mutate("cancer.type" = "All reported cancers") %>%
                    group_by(across(-counts)) %>%
                    summarise("counts" = sum(counts),
                              .groups = 'drop')

                  data_inc <- bind_rows(data_inc, tmp)
                } else {
                  data_inc <- data_inc %>%
                    mutate(cancer.type = if_else(cancer.type == input$all_canc_name, "All malignant neoplasms", cancer.type))
                }

                saveRDS(data_inc, "tmp/data_inc.RDS")

              }
            }
            )

            ####### Save Uploaded Mortality Data ----
            withCallingHandlers({
              if(!is.null(data_mortality())){

                cols_mrt <- c("year", "cancer.type", "sex", "age.grp", "counts")

                names(cols_mrt) <- c(input$var_select_mrt_year,
                                     input$var_select_mrt_cancer.type,
                                     input$var_select_mrt_sex,
                                     input$var_select_mrt_age.group,
                                     input$var_select_mrt_counts)

                cols_mrt <- cols_mrt[names(cols_mrt) != "NA"]


                data_mrt <- data_mortality() %>%
                  dplyr::select(all_of(names(cols_mrt)))

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
                    mutate("cancer.type" = "All reported cancers") %>%
                    group_by(across(-counts)) %>%
                    summarise("counts" = sum(counts),
                              .groups = 'drop')

                  data_mrt <- bind_rows(data_mrt, tmp)
                } else {
                  data_mrt <- data_mrt %>%
                    mutate(cancer.type = if_else(cancer.type == input$all_canc_name, "All malignant neoplasms", cancer.type))
                }

                saveRDS(data_mrt, "tmp/data_mrt.RDS")
              }
            }
            )

            ####### Save Uploaded Population Data ----
            withCallingHandlers({
              if(!is.null(data_population())){

                cols_pop <- c("year", "sex", "age.grp", "population")

                names(cols_pop) <- c(input$var_select_pop_year,
                                     input$var_select_pop_sex,
                                     input$var_select_pop_age.group,
                                     input$var_select_pop_population)

                cols_pop <- cols_pop[names(cols_pop) != "NA"]


                data_pop <- data_population() %>%
                  dplyr::select(all_of(names(cols_pop)))

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
            }
            )

            ####### Save Threshold Value ----
            suppress_threshold <- input$dashboard_suppression_threshold

            aggregate_option <- input$aggregate_option

            ####### Save All Other Inputs ----
            supplied_params <- list(
              "All cancers" = if(input$bool_all_canc == "No") {"All reported cancers"} else {"All malignant neoplasms"},
              "Dashboard title" = input$dashboard_title,
              "Dashboard catchment" = input$dashboard_location,
              "Suppression threshold" = suppress_threshold,
              "aggregate_option" = aggregate_option
            )

            ####### Transform Data ----
            out <- tryCatch({

              transform_data(
                req_mortality_data(), req_population_data(),
                standard_pop, input$std_pop_name,
                supplied_params,
                suppress_threshold,
                aggregate_option
              )

            },
            error = function(e) {
              clean_msg <- gsub("\033\\[[0-9;]*m", "", conditionMessage(e))
              structure(list(message = clean_msg), class = "error")
            }
            )

            ##### errorhandling ----
            if(inherits(out, "warning") | inherits(out, "error")) {

              showModal(
                modalDialog(
                  tagList(

                    div(
                      class = "directory-copy",
                      span("There is something wrong with your data."),
                      tags$pre(out$message)
                    ),
                    hr(),
                    div(
                      class = "directory-copy",
                      span("How can I resolve this?"),
                      tags$ul(
                        tags$li("Please refer to the", tags$a("handbook", href = "https://ccqresearch.github.io/CaRDO-Handbook/", target = "_blank"), "for assistance in organising the data you load."),
                        tags$li("You can copy the error code and search for a solution online."),
                        tags$li("Please email us at", tags$a("statistics@cancerqld.org.au", href = "mailto:statistics@cancerqld.org.au"), "with the error and someone will be in touch to help out.")
                      )
                    )
                  )
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
                    span("So, what's next?"),
                    tags$ol(
                      tags$li("Copy and paste the code below into RStudio's", tags$a("console", href = "https://ccqresearch.github.io/CaRDO-Handbook/build-your-dashboard.html", target = "_blank"), "."),
                      tags$li("Make sure to click", strong("Exit"), "in this window before hitting enter.")
                    ),
                    div(
                      class = "file-path",
                      tags$code(id = "r-code", "shiny::runApp(.../Shiny App/app.R)")
                    ),
                    rclipButton(
                      inputId = "clipbtn",
                      label = "Copy to clipboard",
                      clipText = paste0("shiny::runApp('", file.path(getwd(), "Shiny App/app.R"), "')"),
                      icon = icon("clipboard")
                    )
                  ),

                  hr(),

                  div(
                    class = "file-path-div",
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

          withCallingHandlers({

            columns_incidence <- c("year", "cancer.type", "sex", "age.grp",
                                   #"geog.loc",
                                   "counts")

            inputs_inc <- c(input$var_select_inc_year,
                            input$var_select_inc_cancer.type,
                            input$var_select_inc_sex,
                            input$var_select_inc_age.group,
                            #input$var_select_inc_geog.loc,
                            input$var_select_inc_counts)

            if (input$male_val == input$female_val) {
              rlang::warn("The values for <b>males</b> and <b>females</b> have to be different:::Please select different values")
              return()
            }

            if(length(unique(inputs_inc)) != length(columns_incidence)) {
              rlang::warn("It looks like you've matched the same variable to multiple labels:::Please match the variables to the corresponding label.")
              return()
            }

          },
          warning = function(w) {

            parts <- strsplit(w$message, ":::", fixed = TRUE)[[1]]

            showModal(
              modalDialog(
                tagList(

                  div(
                    class = "directory-copy",
                    span("Mismatched Variables & Labels"),
                  ),
                  hr(),
                  div(
                    class = "directory-copy",
                    id = "mismatch",
                    p(HTML(paste(parts[1]))),
                    p(HTML(paste(parts[2])))
                  )

                ),
                easyClose = TRUE
              )
            )
            invokeRestart("muffleWarning")
          }
          )

          # If we're currently on mortality, and DON'T require population, skip that
        } else if(current_page() == "variable_select_mrt") {
          if(!req_population_data()){
            adder <- 2
          }

          withCallingHandlers({

            columns_mortality <- c("year", "cancer.type", "sex", "age.grp",
                                   #"geog.loc",
                                   "counts")

            inputs_mrt <- c(input$var_select_mrt_year,
                            input$var_select_mrt_cancer.type,
                            input$var_select_mrt_sex,
                            input$var_select_mrt_age.group,
                            #input$var_select_mrt_geog.loc,
                            input$var_select_mrt_counts)

            if (input$male_val == input$female_val) {
              rlang::warn("The values for <b>males</b> and <b>females</b> have to be different:::Please select different values")
              return()
            }

            if(length(unique(inputs_mrt)) != length(columns_mortality)) {
              rlang::warn("It looks like you've matched the same variable to multiple labels:::Please match the variables to the corresponding label.")
              return()
            }

          },
          warning = function(w) {

            parts <- strsplit(w$message, ":::", fixed = TRUE)[[1]]

            showModal(
              modalDialog(
                tagList(

                  div(
                    class = "directory-copy",
                    span("Mismatched Variables & Labels"),
                  ),
                  hr(),
                  div(
                    class = "directory-copy",
                    id = "mismatch",
                    p(HTML(paste(parts[1]))),
                    p(HTML(paste(parts[2])))
                  )

                ),
                easyClose = TRUE
              )
            )
            invokeRestart("muffleWarning")
          }
          )

        } else if(current_page() == "variable_select_pop") {

          withCallingHandlers({

            columns_population <- c("year", "sex", "age.grp", "population")

            inputs_pop <- c(input$var_select_pop_year,
                            input$var_select_pop_sex,
                            input$var_select_pop_age.group,
                            input$var_select_pop_population)

            if (req_population_data() & length(unique(c(input$male_val_pop, input$female_val_pop))) != 2) {
              rlang::warn("The values for <b>males</b> and <b>females</b> have to be different:::Please select different values")
              return()
            }

            if(length(unique(inputs_pop)) != length(columns_population)) {
              rlang::warn("It looks like you've matched the same variable to multiple labels:::Please match the variables to the corresponding label.")
              return()
            }

          },
          warning = function(w) {

            parts <- strsplit(w$message, ":::", fixed = TRUE)[[1]]

            showModal(
              modalDialog(
                tagList(

                  div(
                    class = "directory-copy",
                    span("Mismatched Variables & Labels"),
                  ),
                  hr(),
                  div(
                    class = "directory-copy",
                    id = "mismatch",
                    p(HTML(paste(parts[1]))),
                    p(HTML(paste(parts[2])))
                  )

                ),
                easyClose = TRUE
              )
            )
            invokeRestart("muffleWarning")
          }
          )

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

    #### EVENT: Hide Previous Button on First Page ----
    output$to_hide_previous_btn <- reactive({
      if(current_page_index() == 1) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
    outputOptions(output, 'to_hide_previous_btn', suspendWhenHidden = FALSE)


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
                      label = HTML("Select your <b>count</b> variable"),
                      NA),

          selectInput(inputId = "var_select_inc_year",
                      choices = variable_names,
                      label = HTML("Select your <b>year</b> variable"),
                      NA),
          selectInput(inputId = "var_select_inc_cancer.type",
                      choices = variable_names,
                      label = HTML("Select your <b>cancer name</b> variable"),
                      NA),
          selectInput(inputId = "var_select_inc_age.group",
                      choices = variable_names,
                      label = HTML("Select your <b>age group</b> variable"),
                      NA),
          selectInput(inputId = "var_select_inc_sex",
                      choices = variable_names,
                      label = HTML("Select your <b>sex</b> variable"),
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
                        label = HTML("Select your <b>count</b> variable"),
                        NA),
            selectInput(inputId = "var_select_mrt_year",
                        choices = variable_names,
                        label = HTML("Select your <b>year</b> variable"),
                        NA),
            selectInput(inputId = "var_select_mrt_cancer.type",
                        choices = variable_names,
                        label = HTML("Select your <b>cancer name</b> variable"),
                        NA),
            selectInput(inputId = "var_select_mrt_age.group",
                        choices = variable_names,
                        label = HTML("Select your <b>age group</b> variable"),
                        NA),
            selectInput(inputId = "var_select_mrt_sex",
                        choices = variable_names,
                        label = HTML("Select your <b>sex</b> variable"),
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
                        label = HTML("Select your <b>population</b> variable"),
                        NA),
            condition = "input.variables_pop.includes('year')",
            selectInput(inputId = "var_select_pop_year",
                        choices = variable_names,
                        label = HTML("Select your <b>year</b> variable"),
                        NA),
            selectInput(inputId = "var_select_pop_age.group",
                        choices = variable_names,
                        label = HTML("Select your <b>age group</b> variable"),
                        NA),
            selectInput(inputId = "var_select_pop_sex",
                        choices = variable_names,
                        label = HTML("Select your <b>sex</b> variable"),
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


    # This code gets run on app startup due to the outputOptions(suspendWhenHidden=FALSE)
    output$select_all_canc_var <- renderUI({
      # However, this now causes an issue, whereby input$var_select_inc_cancer.type does not exist yet,
      # So we put req() to say, "Hey don't run what's below until this variable exists".
      req(input$var_select_inc_cancer.type)
      variable_names <- c("Please specify ..", unique(data_incidence()[[input$var_select_inc_cancer.type]]) %>% sort)
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
                        choices = variable_names,
                        selected = "Please specify ..")
          ),
          conditionalPanel(
            condition = "input.bool_all_canc == 'No'",
            div(
              class = "hint-div",
              p("CaRDO will report statistics for 'all cancers'.
                If you select no, CaRDO will calculate 'all cancers reported' by summing the cancer data you provide.
                To ensure CaRDO reports accurate information,
                only click Yes if your 'All cancers' category sums up ALL cancers and not just the ones you supply.")
            )
          )
        )
      )
    })

    output$aggregation_var <- renderUI({

      tagList(
        checkboxInput(inputId = "aggregate_option",
                      label = "Do you want to aggregate and report your data by 5-year intervals (eg. 2018-2022)?",
                      value = FALSE),
        conditionalPanel(
          condition = "input.aggregate_option",
          div(
            class = "hint-div",
            p("This is a good option if you don't have enough data to report by year.
                CaRDO will add up the data for five year intervals and report that.")
          )
        )
      )

    })

    # Render this output on app startup
    outputOptions(output, "select_all_canc_var", suspendWhenHidden = FALSE)


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
#' @importFrom tidyr pivot_longer pivot_wider starts_with last_col
#' @importFrom haven read_dta
#' @author Sean Francis
transform_data <- function(req_mortality_data, req_population_data,
                           standard_pop, std_pop_name,
                           supplied_params, suppress_threshold,
                           aggregate_option){

  # There are three main datasets that are needed to create the dashboard:
  #   1.  'Annual' - This will have the measure (counts/rates) for each year
  #   2.  'Age' - This will have the measures by broad age group
  #   3.  'Average' - This is an aggregation of the most recent 5 years
  #
  # The base is Incidence -> Counts

  # Initialising Directories ----

  output_path <- "Shiny App/Data"

  # Remove the existing `Data` folder, if it exists
  if(dir.exists(output_path)){
    unlink(output_path, recursive = TRUE)
  }

  # Create a new `Data` folder to save the transformed data
  dir.create(output_path, recursive = TRUE)

  # Saving the supplied input parameters to an RDS file in the new `Data` folder
  # The supplied parameters include:
  # Dashboard title & location, a suppression threshold, and an aggregate number
  saveRDS(
    supplied_params,
    file.path(output_path, "supplied_params.RDS")
  )

  # -~-~-~-~-~-~-~--~-~-~-~-~-~-~--~-~-~-~-~-~-~--~-~-~-~-~-~-~--~-~-~-~-~-~-~-~ #

  # COUNTS ----

  ## Transforming Incidence Counts ----

  # Reading in the Incidence data file provided by the user
  data_inc <- readRDS("tmp/data_inc.RDS")


  # Collapsing `Diagnoses` counts to create a `Persons` sex category
  # `Persons` or `3` is just the sum of males and females (or 1 and 2)
  # Stitches it back to the original `data_inc` data frame
  tmp <- data_inc %>%
    mutate('sex' = 3) %>%
    group_by(across(-c(counts))) %>%
    summarise('counts' = sum(counts),
              .groups = 'drop') %>%
    ungroup()

  data_inc <- bind_rows(tmp, data_inc)


  # Identifying the cancer types that don't have all three sex categories
  # This are assumed to be sex-specific cancers (i.e. Breast cancer)
  # Once sex-specific cancers are identified, they are removed
  sex_specific_cancers <- data_inc %>%
    group_by(cancer.type) %>%
    summarise("sex_specific" = if_else(length(unique(sex)) == 3, true = 0, false = 1),
              .groups = 'drop') %>%
    filter(sex_specific == 1) %>%
    pull(cancer.type)

  data_inc <- data_inc %>%
    filter(!(cancer.type %in% sex_specific_cancers & sex == 3))

  # If an aggregation number is supplied, the data and counts need to collapse
  # across year to create year ranges that match that number
  # Aggregation is done by removing the earliest year(s) that don't fit nicely
  # into a multiple of the aggregate number.
  # Labels are created and matched to corresponding years
  if (aggregate_option) {
    start_year <- min(data_inc$year)
    end_year <- max(data_inc$year)

    new_start_year <- start_year + ((end_year - start_year + 1) %% 5)

    breaks <- seq(new_start_year, end_year + 1, by = 5)
    lefts <- head(breaks, -1)
    rights <- tail(breaks, -1) - 1
    labels <- paste0(lefts, "-", rights)

    data_inc <- data_inc %>%
      filter(year >= new_start_year & year <= end_year) %>%
      mutate(
        "year_labs" = cut(
          year,
          breaks = breaks,
          right = FALSE,
          include.lowest = TRUE,
          labels = labels
        ) %>% as.character(),
        "max_year" = max(rights)
      )
  }

  # -~-~-~-~-~-~-~--~-~-~-~-~-~-~--~-~-~-~-~-~-~--~-~-~-~-~-~-~--~-~-~-~-~-~-~-~ #



  ## Transforming Mortality Counts ----
  if (req_mortality_data){

    # Reading in the Mortality data file provided by the user
    data_mrt <- readRDS("tmp/data_mrt.RDS")


    # Collapsing `Death` counts to create a `Persons` sex category
    # `Persons` or `3` is just the sum of males and females (or 1 and 2)
    # Stitches it back to the original `data_inc` data frame
    tmp <- data_mrt %>%
      mutate('sex' = 3) %>%
      group_by(across(-c(counts))) %>%
      summarise('counts' = sum(counts),
                .groups = 'drop') %>%
      ungroup()

    data_mrt <- bind_rows(tmp, data_mrt)


    # Identifying the cancer types that don't have all three sex categories
    # This are assumed to be sex-specific cancers (i.e. Breast cancer)
    # Once sex-specific cancers are identified, they are removed
    sex_specific_cancers_mrt <- data_mrt %>%
      group_by(cancer.type) %>%
      summarise("sex_specific" = if_else(length(unique(sex)) == 3, true = 0, false = 1),
                .groups = 'drop') %>%
      filter(sex_specific == 1) %>%
      pull(cancer.type)

    data_mrt <- data_mrt %>%
      filter(!(cancer.type %in% sex_specific_cancers_mrt & sex == 3))


    # If an aggregation number is supplied, the data and counts need to collapse
    # across year to create year ranges that match that number
    # Aggregation is done by removing the earliest year(s) that don't fit nicely
    # into a multiple of the aggregate number.
    # Labels are created and matched to corresponding years
    if (aggregate_option) {
      start_year <- min(data_mrt$year)
      end_year <- max(data_mrt$year)

      new_start_year <- start_year + ((end_year - start_year + 1) %% 5)

      breaks <- seq(new_start_year, end_year + 1, by = 5)
      lefts <- head(breaks, -1)
      rights <- tail(breaks, -1) - 1
      labels <- paste0(lefts, "-", rights)

      data_mrt <- data_mrt %>%
        filter(year >= new_start_year & year <= end_year) %>%
        mutate(
          "year_labs" = cut(
            year,
            breaks = breaks,
            right = FALSE,
            include.lowest = TRUE,
            labels = labels
          ) %>% as.character(),
          "max_year" = max(rights)
        )
    }

  }

  # -~-~-~-~-~-~-~--~-~-~-~-~-~-~--~-~-~-~-~-~-~--~-~-~-~-~-~-~--~-~-~-~-~-~-~-~ #



  # RATES ----

  # Reading in the Population data file provided by the user
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

  if (req_population_data){

    # Processing the standard population files
    # This file is selected by the user and pulled from the package files
    std_pop <- standard_pop %>%
      filter(std_name == std_pop_name) %>%
      mutate("age.grp" = ragegrp) %>%
      dplyr::select(age.grp, wght)

    std_pop_grouped <- std_pop %>%
      merge(age_grps) %>%
      group_by(age.grp_string) %>%
      mutate("wt" = wght / sum(wght))

    ## Incidence ----

    # Initialising an empty data frame to be filled from the for-loop below
    data_inc_pop <- data.frame()


    # For loop isolates each cancer >
    #   left joins the population file (by year, sex and age-group) >
    #   changes any NA values in counts to 0 (you can't calculate rates using NA) >
    #   add the cancer name back in >
    #   recreates the `data_inc` file but with population data >
    #   saves it as `data_inc_pop`
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


    ### Rates calculations ----

    #### Aggregation ----
    if (aggregate_option) {
      data_inc_agg_pop <- data_inc_pop %>%
        group_by(year_labs, sex, cancer.type, age.grp) %>%
        summarise("counts" = sum(counts),
                  "population" = sum(population)) %>%
        ungroup() %>%
        rename("year" = year_labs)

      # Annual (By Year Graph)
      data_inc_annual <- data_inc_agg_pop %>%
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
        dplyr::select(-c(agspfc, cum_rate)) %>%
        pivot_longer(cols = c("Counts", "Rates", "ltr"),
                     names_to = "measure",
                     values_to = "obs") %>%
        filter(obs != 0)

      # Age-group (By Age Group Graph)
      data_inc_age <- data_inc_agg_pop %>%
        merge(std_pop_grouped) %>%
        mutate("Rates" = counts / population * wt * 100000) %>%
        rename("Counts" = counts) %>%
        pivot_longer(cols = c("Counts", "Rates"),
                     names_to = "measure",
                     values_to = "obs") %>%
        dplyr::select(-population) %>%
        merge(age_grps) %>%
        group_by(across(-c(obs, age.grp, wght, wt))) %>%
        summarise("obs" = sum(obs),
                  .groups = 'drop') # %>%
      # filter(obs != 0)

      # Averages (By Cancer Graph)
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

    }

    #### No Aggregation ----
    else {

      # Annual (By Year Graph)
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
        dplyr::select(-c(agspfc, cum_rate)) %>%
        pivot_longer(cols = c("Counts", "Rates", "ltr"),
                     names_to = "measure",
                     values_to = "obs")

      # Add Trends (By Year Graph)
      data_inc_annual_tmp <- data.frame()

      for (canc in unique(data_inc_annual$cancer.type)){
        for (sex_i in unique(data_inc_annual$sex)){

          filt_df <- data_inc_annual %>%
            filter(cancer.type == canc,
                   sex == sex_i)

          filt_df_wide <- filt_df %>%
            pivot_wider(names_from = "measure", values_from = "obs")


          tmp_counts <- suppressWarnings(fit_trendline(filt_df_wide, "year", "Counts", rates_req = FALSE)) %>%
            dplyr::select(-c(Rates, ltr)) %>%
            rename("Trend_Counts" = Trend,
                   "Trend_Counts_lower_ci" = lower_ci,
                   "Trend_Counts_upper_ci" = upper_ci)

          tmp_rates <- suppressWarnings(fit_trendline(filt_df_wide, "year", "Rates", rates_req = TRUE)) %>%
            dplyr::select(-c(Counts, ltr)) %>%
            rename("Trend_Rates" = Trend,
                   "Trend_Rates_lower_ci" = lower_ci,
                   "Trend_Rates_upper_ci" = upper_ci)

          dat_merge <- merge(tmp_counts, tmp_rates)

          # Just observed data
          dat_merge_obs_long <- dat_merge %>%
            dplyr::select(-contains(c("Trend", "apc", "signif"))) %>%
            pivot_longer(cols = c("Counts", "Rates"),
                         names_to = "measure",
                         values_to = "obs")


          # Just trend data
          dat_merge_trend_long <- dat_merge %>%
            dplyr::select(-c("Counts", "Rates")) %>%
            rename("Counts_obs" = Trend_Counts,
                   "Rates_obs" = Trend_Rates,
                   "Counts_lower.ci" = Trend_Counts_lower_ci,
                   "Counts_upper.ci" = Trend_Counts_upper_ci,
                   "Rates_lower.ci" = Trend_Rates_lower_ci,
                   "Rates_upper.ci" = Trend_Rates_upper_ci) %>%
            pivot_longer(cols = c("Counts_obs", "Rates_obs",
                                  "Counts_lower.ci", "Counts_upper.ci",
                                  "Rates_lower.ci", "Rates_upper.ci"),
                         names_to = c("measure", "type"),
                         names_sep = "_",
                         values_to = "obs_trend") %>%
            # Spread the 'type' to 'lower.ci' and 'upper.ci'
            pivot_wider(names_from = "type", values_from = "obs_trend") %>%
            rename("obs_trend" = obs,
                   "lower.ci_trend" = lower.ci,
                   "upper.ci_trend" = upper.ci)

          dat <- merge(
            dat_merge_obs_long,
            dat_merge_trend_long

            # dat_merge %>%
            #   dplyr::select(-c("Counts", "Rates")) %>%
            #   rename("Counts" = Trend_Counts,
            #          "Rates" = Trend_Rates) %>%
            #   pivot_longer(cols = c("Counts", "Rates"),
            #                names_to = "measure",
            #                values_to = "obs_trend")
          ) %>%
            relocate(c(tidyr::starts_with("apc"), "signif"), .after = tidyr::last_col())

          dat_final <- bind_rows(
            filt_df %>%
              filter(measure == "ltr"),
            dat)

          data_inc_annual_tmp <- bind_rows(data_inc_annual_tmp, dat_final)
        }
      }

      data_inc_annual <- data_inc_annual_tmp %>%
        filter(obs != 0)

      # Age (By Age Group Graph)
      data_inc_age <- data_inc_pop %>%
        merge(std_pop_grouped) %>%
        mutate("Rates" = counts / population * wt * 100000) %>%
        rename("Counts" = counts) %>%
        pivot_longer(cols = c("Counts", "Rates"),
                     names_to = "measure",
                     values_to = "obs") %>%
        dplyr::select(-population) %>%
        merge(age_grps) %>%
        group_by(across(-c(obs, age.grp, wght, wt))) %>%
        summarise("obs" = sum(obs),
                  .groups = 'drop')

      # Averages (By Cancer Graph)
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

    }


    ### Suppression ----

    # Remove trend temporarily
    data_inc_annual_sup <- data_inc_annual %>%
      dplyr::select(-contains(c("trend", "apc")))

    # Implement suppression
    # Pivot wider then longer so that suppression can occur on counts and rates
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



    ## Mortality ----

    if (req_mortality_data){

      # Initialising an empty data frame to be filled from the for-loop below
      data_mrt_pop <- data.frame()

      # For loop isolates each cancer >
      #   left joins the population file (by year, sex and age-group) >
      #   changes any NA values in counts to 0 (you can't calculate rates using NA) >
      #   add the cancer name back in >
      #   recreates the `data_inc` file but with population data >
      #   saves it as `data_inc_pop`
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


      ### Rates calculations ----

      #### Aggregation ----
      if (aggregate_option) {
        data_mrt_agg_pop <- data_mrt_pop %>%
          group_by(year_labs, sex, cancer.type, age.grp) %>%
          summarise("counts" = sum(counts),
                    "population" = sum(population)) %>%
          ungroup() %>%
          rename("year" = year_labs)

        # Annual (By Year Graph)
        data_mrt_annual <- data_mrt_agg_pop %>%
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
          dplyr::select(-c(agspfc, cum_rate)) %>%
          pivot_longer(cols = c("Counts", "Rates", "ltr"),
                       names_to = "measure",
                       values_to = "obs") %>%
          filter(obs != 0)

        # Age (By Age Group Graph)
        data_mrt_age <- data_mrt_agg_pop %>%
          merge(std_pop_grouped) %>%
          mutate("Rates" = counts / population * wt * 100000) %>%
          rename("Counts" = counts) %>%
          pivot_longer(cols = c("Counts", "Rates"),
                       names_to = "measure",
                       values_to = "obs") %>%
          dplyr::select(-population) %>%
          merge(age_grps) %>%
          group_by(across(-c(obs, age.grp, wght, wt))) %>%
          summarise("obs" = sum(obs),
                    .groups = 'drop')

        # Averages (By Cancer Graph)
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

      }

      #### No Aggregation ----
      else {

        # Annual (By Year Graph)
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
          dplyr::select(-c(agspfc, cum_rate)) %>%
          pivot_longer(cols = c("Counts", "Rates", "ltr"),
                       names_to = "measure",
                       values_to = "obs")

        # Add Trends
        data_mrt_annual_tmp <- data.frame()

        for (canc in unique(data_mrt_annual$cancer.type)){
          for (sex_i in unique(data_mrt_annual$sex)){

            filt_df <- data_mrt_annual %>%
              filter(cancer.type == canc,
                     sex == sex_i)

            filt_df_wide <- filt_df %>%
              pivot_wider(names_from = "measure", values_from = "obs")


            tmp_counts <- suppressWarnings(fit_trendline(filt_df_wide, "year", "Counts", rates_req = FALSE)) %>%
              dplyr::select(-c(Rates, ltr)) %>%
              rename("Trend_Counts" = Trend,
                     "Trend_Counts_lower_ci" = lower_ci,
                     "Trend_Counts_upper_ci" = upper_ci)

            tmp_rates <- suppressWarnings(fit_trendline(filt_df_wide, "year", "Rates", rates_req = TRUE)) %>%
              dplyr::select(-c(Counts, ltr)) %>%
              rename("Trend_Rates" = Trend,
                     "Trend_Rates_lower_ci" = lower_ci,
                     "Trend_Rates_upper_ci" = upper_ci)

            dat_merge <- merge(tmp_counts, tmp_rates)

            # Just observed data
            dat_merge_obs_long <- dat_merge %>%
              dplyr::select(-contains(c("Trend", "apc", "signif"))) %>%
              pivot_longer(cols = c("Counts", "Rates"),
                           names_to = "measure",
                           values_to = "obs")


            # Just trend data
            dat_merge_trend_long <- dat_merge %>%
              dplyr::select(-c("Counts", "Rates")) %>%
              rename("Counts_obs" = Trend_Counts,
                     "Rates_obs" = Trend_Rates,
                     "Counts_lower.ci" = Trend_Counts_lower_ci,
                     "Counts_upper.ci" = Trend_Counts_upper_ci,
                     "Rates_lower.ci" = Trend_Rates_lower_ci,
                     "Rates_upper.ci" = Trend_Rates_upper_ci) %>%
              pivot_longer(cols = c("Counts_obs", "Rates_obs",
                                    "Counts_lower.ci", "Counts_upper.ci",
                                    "Rates_lower.ci", "Rates_upper.ci"),
                           names_to = c("measure", "type"),
                           names_sep = "_",
                           values_to = "obs_trend") %>%
              # Spread the 'type' to 'lower.ci' and 'upper.ci'
              pivot_wider(names_from = "type", values_from = "obs_trend") %>%
              rename("obs_trend" = obs,
                     "lower.ci_trend" = lower.ci,
                     "upper.ci_trend" = upper.ci)

            dat <- merge(
              dat_merge_obs_long,
              dat_merge_trend_long

              # dat_merge %>%
              #   dplyr::select(-c("Counts", "Rates")) %>%
              #   rename("Counts" = Trend_Counts,
              #          "Rates" = Trend_Rates) %>%
              #   pivot_longer(cols = c("Counts", "Rates"),
              #                names_to = "measure",
              #                values_to = "obs_trend")
            ) %>%
              relocate(c(tidyr::starts_with("apc"), "signif"), .after = tidyr::last_col())

            dat_final <- bind_rows(
              filt_df %>%
                filter(measure == "ltr"),
              dat)

            data_mrt_annual_tmp <- bind_rows(data_mrt_annual_tmp, dat_final)
          }
        }

        data_mrt_annual <- data_mrt_annual_tmp %>%
          filter(obs != 0)

        # Age (By Age Group Graph)
        data_mrt_age <- data_mrt_pop %>%
          merge(std_pop_grouped) %>%
          mutate("Rates" = counts / population * wt * 100000) %>%
          rename("Counts" = counts) %>%
          pivot_longer(cols = c("Counts", "Rates"),
                       names_to = "measure",
                       values_to = "obs") %>%
          dplyr::select(-population) %>%
          merge(age_grps) %>%
          group_by(across(-c(obs, age.grp, wght, wt))) %>%
          summarise("obs" = sum(obs),
                    .groups = 'drop')

        # Averages (By Cancer Graph)
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

      }


      ### Suppression ----

      # Remove trend temporarily
      data_mrt_annual_sup <- data_mrt_annual %>%
        dplyr::select(-contains(c("trend", "apc")))


      # Implement suppression
      # Pivot wider then longer so that suppression can occur on counts and rates
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

  # -~-~-~-~-~-~-~--~-~-~-~-~-~-~--~-~-~-~-~-~-~--~-~-~-~-~-~-~--~-~-~-~-~-~-~-~ #

  # NO RATES ----

  else{

    ## Incidence ----

    ### Aggregation ----
    if (aggregate_option) {

      data_aggregate <- data_inc %>%
        group_by(year_labs, sex, cancer.type, age.grp, max_year) %>%
        summarise("counts" = sum(counts)) %>%
        ungroup() %>%
        rename("year" = year_labs)

      data_inc_annual <- data_inc %>%
        group_by(year, sex, cancer.type, max_year) %>%
        summarise("obs" = sum(counts),
                  "measure" = "Counts",
                  .groups = 'drop')

      data_inc_age <- data_aggregate %>%
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

    }

    ### No Aggregation ----
    else {

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

      data_inc_annual_tmp <- data.frame()

      for (canc in unique(data_inc_annual$cancer.type)){
        for (sex_i in unique(data_inc_annual$sex)){

          filt_df <- data_inc_annual %>%
            filter(cancer.type == canc,
                   sex == sex_i)

          if(nrow(filt_df != 0)){
            tmp_counts <- suppressWarnings(fit_trendline(filt_df, "year", "obs", rates_req = FALSE)) %>%
              rename("obs_trend" = Trend,
                     "lower.ci_trend" = lower_ci,
                     "upper.ci_trend" = upper_ci)

            data_inc_annual_tmp <- bind_rows(data_inc_annual_tmp, tmp_counts)
          }
        }
      }

    }

    # Finalising the Annual data frame
    if (aggregate_option) {

      data_inc_annual <- data_inc_annual %>%
        filter(obs != 0,
               year >= new_start_year & year <= end_year) %>%
        mutate(
          "year_labs" = cut(
            year,
            breaks = breaks,
            right = FALSE,
            include.lowest = TRUE,
            labels = labels
          ) %>% as.character(),
          "max_year" = max(rights)
        ) %>%
        group_by(year_labs, sex, cancer.type, measure, max_year) %>%
        summarise("obs" = sum(obs)) %>%
        ungroup() %>%
        rename("year" = year_labs)

    } else {

      data_inc_annual <- data_inc_annual_tmp %>%
        filter(obs != 0)

    }


    ### Suppression ----
    data_inc_annual <- data_inc_annual %>%
      mutate("suppress" = if_else(measure == "Counts" & obs < suppress_threshold, true = 1, false = 0),
             "obs" = if_else(suppress == 1, true = NA, false = obs))

    data_inc_age <- data_inc_age %>%
      mutate("suppress" = if_else(measure == "Counts" & obs < suppress_threshold, true = 1, false = 0),
             "obs" = if_else(suppress == 1, true = NA, false = obs))


    ## Mortality ----

    ### Aggregation ----
    if (req_mortality_data){

      if (aggregate_option) {

        data_aggregate <- data_mrt %>%
          group_by(year_labs, sex, cancer.type, age.grp, max_year) %>%
          summarise("counts" = sum(counts)) %>%
          ungroup() %>%
          rename("year" = year_labs)

        data_mrt_annual <- data_mrt %>%
          group_by(year, sex, cancer.type, max_year) %>%
          summarise("obs" = sum(counts),
                    "measure" = "Counts",
                    .groups = 'drop')

        data_mrt_age <- data_aggregate %>%
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

      }

      ### No Aggregation ----
      else {

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

        # Calculate 5 year averages
        data_mrt_average <- data_mrt_annual %>%
          filter(year >= max(year) - 4) %>%
          group_by(sex, cancer.type, measure) %>%
          summarise("year" = paste0(min(year), "-", max(year)),
                    "obs" = sum(obs)/5,
                    .groups = 'drop')

        data_mrt_annual_tmp <- data.frame()

        for (canc in unique(data_mrt_annual$cancer.type)){
          for (sex_i in unique(data_mrt_annual$sex)){

            filt_df <- data_mrt_annual %>%
              filter(cancer.type == canc,
                     sex == sex_i)

            if(nrow(filt_df) != 0) {
              tmp_counts <- suppressWarnings(fit_trendline(filt_df, "year", "obs", rates_req = FALSE)) %>%
                rename("obs_trend" = Trend,
                       "lower.ci_trend" = lower_ci,
                       "upper.ci_trend" = upper_ci)

              data_mrt_annual_tmp <- bind_rows(data_mrt_annual_tmp, tmp_counts)
            }
          }
        }

      }

      # data_mrt <- data_mrt %>%
      #   group_by(year_labs, sex, cancer.type, age.grp) %>%
      #   summarise("counts" = sum(counts)) %>%
      #   ungroup() %>%
      #   rename("year" = year_labs)
      #
      # data_mrt_annual <- data_mrt %>%
      #   group_by(year, sex, cancer.type) %>%
      #   summarise("obs" = sum(counts),
      #             "measure" = "Counts",
      #             .groups = 'drop')
      #
      # data_mrt_age <- data_mrt %>%
      #   mutate("obs" = counts,
      #          "measure" = "Counts") %>%
      #   merge(age_grps) %>%
      #   group_by(across(-c(obs, age.grp, counts))) %>%
      #   summarise("obs" = sum(obs),
      #             .groups = 'drop')
      #
      # ### Calculate 5 year averages
      # data_mrt_average <- data_mrt_annual %>%
      #   filter(year >= max(year) - 4) %>%
      #   group_by(sex, cancer.type, measure) %>%
      #   summarise("year" = paste0(min(year), "-", max(year)),
      #             "obs" = sum(obs)/5,
      #             .groups = 'drop')

      if (aggregate_option) {

        data_mrt_annual <- data_mrt_annual %>%
          filter(obs != 0,
                 year >= new_start_year & year <= end_year) %>%
          mutate(
            "year_labs" = cut(
              year,
              breaks = breaks,
              right = FALSE,
              mrtlude.lowest = TRUE,
              labels = labels
            ) %>% as.character(),
            "max_year" = max(rights)
          ) %>%
          group_by(year_labs, sex, cancer.type, measure, max_year) %>%
          summarise("obs" = sum(obs)) %>%
          ungroup() %>%
          rename("year" = year_labs)

      } else {

        data_mrt_annual <- data_mrt_annual_tmp %>%
          filter(obs != 0)

      }

      # data_mrt_annual <- data_mrt_annual_tmp %>%
      #   filter(obs != 0)

      ### Implement suppression ----
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



#' Fits trendline to the data
#'
#' @param y_val The y value of the dataset, to be used in regression
#' @param x_val The x value of the dataset, to be used in regression
#' @param data The dataset to regress from
#' @param rates_req A boolean indicating whether rates or counts are supplied
#' @import segmented
#' @importFrom dplyr filter ensym
#' @importFrom magrittr %>%
#' @importFrom strucchange breakpoints
#' @author Sean Francis

fit_trendline <- function(data, x_val, y_val, rates_req){

  data <- data %>% arrange(!!ensym(x_val))

  n_obs <- nrow(data)
  seg_size <- max(2, floor(n_obs / 3))

  # Run regression on natural log scale if calculating rates
  if(rates_req){
    data[[y_val]] <- log(data[[y_val]])

    data[[y_val]][data[[y_val]] == -Inf] <- 0
  }

  max_joins <- floor(length(unique(data[[x_val]]))/5)

  # max_joins <- 3


  # Find breakpoints. We do this here, so we can specify the number of breaks, and min number of points between
  # (Can't do that in segmented function)
  bp <- breakpoints(as.formula(paste0(y_val, " ~ ", x_val)),
                    data = data,
                    breaks = max_joins,
                    h = seg_size)

  breakpoints <- data[[x_val]][bp$breakpoints]

  # estimate the 'starting' linear model with the usual "lm" function using the log values
  o <- lm(as.formula(paste0(y_val, " ~ ", x_val)),
          data = data)

  # If there are no breakpoints
  if(any(is.na(breakpoints))){

    # Run lm model preds
    preds <- predict(
      o,
      newdata = data,
      se.fit = TRUE, type = "response",
      level = 0.95,
      interval = "confidence"
    )


    ciValues <- data.frame(
      preds$fit,
      "se" = preds$se.fit
    )


  }else{

    # Fit segmented model, using fixed breakpoints
    os <- segmented(
      obj = o,
      # Specify the starting breakpoints
      psi = breakpoints,
      # Don't run any additional iterations - keep them fixed at the supplied values
      control = seg.control(it.max = 0)
    )

    preds <- predict(
      os,
      newdata = os$model,
      se.fit = TRUE, type = "response",
      level = 0.95,
      interval = "confidence"
    )

    ciValues <- data.frame(
      preds$fit,
      "se" = preds$se.fit
    )
  }

  data$Trend <- ciValues$fit
  data$lower_ci <- ciValues$lwr
  data$upper_ci <- ciValues$upr

  # Calculate Annual Percentage Change (APC)
  if(rates_req){
    # Get the entire join if no breakpoints
    cutoff <- if(any(is.na(breakpoints))){
      min(data[[x_val]])
      # Else get the last join
    }else{
      max(breakpoints)
    }

    # Subset the data to the last break only
    tmp <- data %>%
      filter(!!ensym(x_val) >= cutoff)

    n <- nrow(tmp)

    # Calculate our beta/slope value - rise/run
    beta <- (tmp$Trend[1] - tmp$Trend[n]) / (tmp[[x_val]][1] - tmp[[x_val]][n])

    # Now calculate our APC according to JP methodology
    # https://surveillance.cancer.gov/help/joinpoint/setting-parameters/method-and-parameters-tab/apc-aapc-tau-confidence-intervals/estimate-average-percent-change-apc-and-confidence-interval
    apc <- (exp(beta)-1) * 100

    # Then calculate our standard error of our beta
    y_hat <- tmp$Trend
    residuals <- tmp[[y_val]] - y_hat
    SSE <- sum(residuals^2)
    SXX <- sum((tmp[[x_val]] - mean(tmp[[x_val]]))^2)

    se_beta <- sqrt((1/(n - 2)) * (SSE / SXX))

    # p_star <- 2 * (length(breakpoints) + 2)
    # df <- n - p_star

    df <- preds$df

    # Now calculate the 95% CI
    apc_lower_ci <- 100 * (exp(beta - se_beta * qt(p = 0.975, df = df)) - 1)
    apc_upper_ci <- 100 * (exp(beta + se_beta * qt(p = 0.975, df = df)) - 1)

    # Assign
    data <- data %>%
      mutate(
        "apc" = apc,
        "apc_time" = paste0(cutoff, "-", max(data[[x_val]])),
        "apc_lower_ci" = apc_lower_ci,
        "apc_upper_ci" = apc_upper_ci,
        "signif" = if_else(apc_lower_ci * apc_upper_ci < 0, NA, "*") # product of same signs is +ve, product of opposite signs is -ve
      )



    # Revert the log transform of the data
    data[[y_val]] <- exp(data[[y_val]])
    data$Trend <- exp(data$Trend)
    data$lower_ci <- exp(data$lower_ci)
    data$upper_ci <- exp(data$upper_ci)
  }


  return(data)

}



