#' Create your dashboard
#'
#' Launches the application for the user to load their data and input the
#' details relating to the data's format.
#'
#' @import shiny
#' @importFrom bslib page_fillable navset_hidden nav_panel_hidden nav_select
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
#' @importFrom stringr str_to_title
#' 
#' @return No return value
#' 
#' @export

create_dashboard <- function() {
  
  # ------------------------------------------------------------------------ #
  # SET UP ----
  # ------------------------------------------------------------------------ #
  
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

  data_var_list = c("counts", "year", "cancer type", "sex", "age group")

  standard_pop <- readRDS(system.file("extdata/stdPopulations_18.RDS", package = "CaRDO"))
  std_pop_names <- unique(standard_pop$std_name)

  addResourcePath("CaRDO", system.file("UX_Styling/www", package = "CaRDO"))
  
  
  
  
  
  # ------------------------------------------------------------------------ #
  # UI START ----
  # ------------------------------------------------------------------------ #

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
                          accept = c(".csv", ".tsv", ".dta")),
                uiOutput("data_inc_upload_msg")
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
              uiOutput(outputId = "aggregation_var"),
              div(
                class = "hint-div",
                p("We assume that you are using ICD-10 as a coding system for cancers.",
                  "This system will be displayed as a table in the Methods page.")
              )
            ),
            div(
              id = "age-group-structure",
              class = "final-panel-div",
              radioButtons(
                inputId = "age_group_var",
                label = "Age collection scope: All ages (0-85+)",
                choices = list(
                  "All ages (default)" = "all_ages",
                  "Adults only - WHO (15-85+)" = "adults_who",
                  "Adults only - Legal (18-85+)" = "adults_legal"
                  # "Paediatric only (0-14)",
                  # "Paediatric & Adolescent (0-19)"
                ),
                selected = "all_ages"
              ),
              div(
                class = "hint-div",
                p("Only change this if you do not collect across the full age range.")
              )
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
              ),
              div(
                class = "hint-div feedback",
                p("If you use CaRDO or have feedback, please contact us at",
                  tags$a("statistics@cancerqld.org.au", href = "mailto:statistics@cancerqld.org.au"),
                  "– we’d love to hear from you. Your input helps us demonstrate CaRDO’s impact and secure funding for its continued development.")
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
          condition = "!output.hide_previous_btn",
          actionButton(inputId = "previous_page",
                       label = "Previous",
                       width = "100%")
        )
      ),
      div(
        id = "license_modal",
        style = "margin: 0 auto;",
        actionButton(inputId = "license",
                     label = "View Licensing",
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
  
  # -------------------------------------------------------------------------- #
  # UI END ----
  # -------------------------------------------------------------------------- #
  
  
  
  
  
  
  
  
  
  
  # -------------------------------------------------------------------------- #
  # SERVER START ----
  # -------------------------------------------------------------------------- #

  server <- function(input, output, session){
    
    # ------------------------------------------------------------------------ #
    # SERVER SET UP ----
    # ------------------------------------------------------------------------ #
    
    
    
    is_na_char <- function(x) { x=="NA" }

    licensing_modal <- showModal(
      
        modalDialog(
          size = "l",
          easyClose = TRUE,
          
          # Modal Header + Body
          tagList(
            
            div(class = "directory-copy", span("CaRDO Licensing")),
            hr(), # --- Horizontal Line --- #
            div(
              class = "license-text",
              p("The CaRDO software and associated materials are made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International Licence (CC BY-NC-SA 4.0).", tags$br(),
                "This licence allows users to:", tags$br()),
              tags$ul(
                tags$li("Share - copy and redistribute the material in any medium or format; and/or,"),
                tags$li("Adapt - remix, transform, and build upon the material,")
              ),
              p("Under the following conditions:", tags$br()),
              tags$ul(
                tags$li(tags$b("Attribution"), "- You must give appropriate credit, provide a link to the licence, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use. Please use this reference when providing credit: ", tags$i("Retell, J., Francis, S., Rene, J., & Baade, P. (2025). CaRDO Handbook (Version 06-2025). Cancer Council Queensland: Viertel Cancer Research Centre. https://ccqresearch.github.io/CaRDO-Handbook/")),
                tags$li(tags$b("NonCommercial"), "- You may not use the material for commercial purposes without prior written permission."),
                tags$li(tags$b("ShareAlike"), "- If you remix, transform, or build upon the material, you must distribute your contributions under the same licence (CC BY-NC-SA 4.0). This ensures that any derivative works remain freely available under the same terms, supporting open collaboration and equitable access especially for low-resource cancer registries.")
              ),
              p("We welcome use by cancer registries and organisations, especially in low-resource settings. If your organisation requires customisations or enhancements to the base product, please contact us (statistics@cancerqld.org.au) to discuss potential cost-recovery arrangements.")
            )
          ),
          
          # Modal Footer
          footer = tagList(modalButton("I Agree"))
        )
        
    )
    
    # ------------------------------------------------------------------------ #
    
    
    
    
    
    # ------------------------------------------------------------------------ #
    # PURE REACTIVES ----
    # ------------------------------------------------------------------------ #
    
    
    
    ## Mortality/Population data reactives ---- 
    
    # Have any population or mortality datasets been uploaded?
    req_mortality_data <- reactive({
      if (is.null(input$select_data)) { return(FALSE) }
      else { return("Mortality" %in% input$select_data) }
    })
    
    req_population_data <- reactive({
      if(is.null(input$select_data)) { return(FALSE) }
      else{ return("Population" %in% input$select_data) }
    })
    
    # ----------------------------------- #
    
    ## Page related reactives ----
    
    # Page numbers help with page specific behaviours
    current_page_index <- reactiveVal(1)
    current_page       <- reactive({ pages[[current_page_index()]] })
    
    # Each page has it's own set of requirements to complete before progressing
    completed_page <- reactive({

      bool <- TRUE

      if (current_page() == "data_upload") { # ------------------------------- #
        
        bool <- if (req_mortality_data() & !req_population_data()){
          !is.null(data_incidence()) && !is.null(data_mortality())
        }else if (!req_mortality_data() & req_population_data()) {
          !is.null(data_incidence()) && !is.null(data_population())
        } else if (req_mortality_data() & req_population_data()){
          !is.null(data_incidence()) && !is.null(data_mortality()) && !is.null(data_population())
        } else {
          !is.null(data_incidence())
        }
        
      } else if (current_page() == "disclaimer") { # ------------------------- #
        
        bool <- input$understand > 0
        
      } else if (current_page() == "variable_select_inc") { # ---------------- #
        
        bool <- !any(
          sapply(
            c(input$var_select_inc_counts,
              input$var_select_inc_year,
              input$var_select_inc_cancer.type,
              input$var_select_inc_sex,
              input$var_select_inc_age.group
            ),
            is_na_char
          )
        )
        
      } else if (current_page() == "variable_select_mrt") { # ---------------- #
        
        if (req_mortality_data()) {
          bool <- !any(
            sapply(
              c(input$var_select_mrt_counts,
                input$var_select_mrt_year,
                input$var_select_mrt_cancer.type,
                input$var_select_mrt_sex,
                input$var_select_mrt_age.group
              ),
              is_na_char
            )
          )
        } else {
          bool <- TRUE
        }
        
      } else if (current_page() == "variable_select_pop") { # ---------------- #

        bool <- if (req_population_data()) {
          !any(
            sapply(
              c(input$var_select_pop_population,
                input$var_select_pop_year,
                input$var_select_pop_sex,
                input$var_select_pop_age.group
              ),
              is_na_char
            )
          )
        } else {
          bool <- TRUE
        }
        
      } else if (current_page() == "supplied_params") { # -------------------- #
    
        bool <- (trimws(input$dashboard_title) != "" && trimws(input$dashboard_location) != "") &
          (if (input$bool_all_canc == "Yes") {input$all_canc_name != "Please specify .."}else{TRUE})
        
      }

      return(bool)
      
    })
    
    # ------------------------------------------------------------------------ #
    
    
    
    
    
    # ------------------------------------------------------------------------ #
    # OBSERVES ----
    # ------------------------------------------------------------------------ #
    
    
    
    ## Show licensing modal on load ----
    observe({ licensing_modal })

    
    ## EVENT: Next Button Clicked ----
    # ------------------------------------------------------------------------ #
    observeEvent(input$next_page, {

      if (current_page_index() == length(pages)) {
        
        ### LAST Page ----
        withProgress(
          message = "Transforming your data...",
          detail  = "Please wait",
          value   = 0,
          
          {
            #### Temp directory check ----
            # ----------------------------------- #
            if(!dir.exists("tmp")) {
              success <- dir.create("tmp", recursive = TRUE)
              if (!success) stop("Failed to create directory: Check Permissions")
            }
            

            ### Save Uploaded Incidence Data ----
            # ----------------------------------- #
            save_error <- NULL

            tryCatch({
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
                    "cancer.type" = stringr::str_to_title(cancer.type),
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
            },
            error = function(e) { save_error <<- conditionMessage(e) }
            )
            
            
            ### Save Uploaded Mortality Data ----
            # ----------------------------------- #
            tryCatch({
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
                    "cancer.type" = stringr::str_to_title(cancer.type),
                    "sex" = case_when(
                      sex == input$male_val_mrt ~ 1,
                      sex == input$female_val_mrt ~ 2,
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
            },
            error = function(e) { save_error <<- conditionMessage(e) }
            )
            
            
            ### Save Uploaded Population Data ----
            # ----------------------------------- #
            tryCatch({
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
            },
            error = function(e) { save_error <<- conditionMessage(e) }
            )
            
            
            ### Save Threshold Value ----
            # ----------------------------------- #
            suppress_threshold <- input$dashboard_suppression_threshold
            aggregate_option   <- input$aggregate_option
            agerange_choice    <- input$age_group_var
            
            
            ### Save All Other Inputs ----
            # ----------------------------------- #
            supplied_params <- list(
              "All cancers" = if(input$bool_all_canc == "No") {"All reported cancers"} else {"All malignant neoplasms"},
              "Dashboard title" = input$dashboard_title,
              "Dashboard catchment" = input$dashboard_location,
              "Suppression threshold" = suppress_threshold,
              "aggregate_option" = aggregate_option,
              "Age range" = agerange_choice,
              "Standard population" = input$std_pop_name
            )
            
            incProgress(1/10, detail = "Validating...")
            
            ### Transform Data ----
            # ----------------------------------- #
            out <- tryCatch({

              if (!is.null(save_error)) stop(save_error, call. = FALSE)

              transform_data(
                req_mortality_data(), req_population_data(),
                standard_pop, input$std_pop_name,
                supplied_params,
                suppress_threshold,
                aggregate_option,
                agerange_choice
              )

            },
            error = function(e) {
              clean_msg <- gsub("\033\\[[0-9;]*m", "", conditionMessage(e))
              structure(list(message = clean_msg), class = "error")
            }
            )
            
            
            ### Error Handling ----
            # ----------------------------------- #
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

              unlink("Shiny App", recursive = TRUE)
              unlink("tmp", recursive = TRUE)

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

          }) # withProgress() end

      } else {

        ### NEXT Page ----
        adder <- 1

        if (current_page() == "variable_select_inc") {
          
          #### Incidence Variable Page ----
          
          # Skip pages if population and/or mortality is NOT required
          # ----------------------------------- #
          if (!req_population_data() & !req_mortality_data()) {
            adder <- 3
          } else if (!req_mortality_data()) {
            adder <- 2
          }
          
          
          # Process the inputs given on this page
          # ----------------------------------- #
          withCallingHandlers({

            columns_incidence <- c("year", "cancer.type", "sex", "age.grp", "counts")

            inputs_inc <- c(input$var_select_inc_year,
                            input$var_select_inc_cancer.type,
                            input$var_select_inc_sex,
                            input$var_select_inc_age.group,
                            input$var_select_inc_counts)
            
            # Warning if sex values aren't correct
            if (input$male_val == input$female_val) {
              rlang::warn("The values for <b>males</b> and <b>females</b> have to be different:::Please select different values")
              return()
            }
            
            # Warning if at least one column has been selected twice
            if(length(unique(inputs_inc)) != length(columns_incidence)) {
              rlang::warn("It looks like you've matched the same variable to multiple labels:::Please match the variables to the corresponding label.")
              return()
            }

          }, warning = function(w) {

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
          # ----------------------------------- #

        } else if (current_page() == "variable_select_mrt") {
          
          #### Mortality Variable Page ----
          
          # Skip pages if population NOT required
          if (!req_population_data()) { adder <- 2 }
          
          # Process the inputs given on this page
          # ----------------------------------- #
          withCallingHandlers({

            columns_mortality <- c("year", "cancer.type", "sex", "age.grp", "counts")

            inputs_mrt <- c(input$var_select_mrt_year,
                            input$var_select_mrt_cancer.type,
                            input$var_select_mrt_sex,
                            input$var_select_mrt_age.group,
                            input$var_select_mrt_counts)

            # Warning if sex values aren't correct
            if (input$male_val_mrt == input$female_val_mrt) {
              rlang::warn("The values for <b>males</b> and <b>females</b> have to be different:::Please select different values")
              return()
            }
            
            # Warning if at least one column has been selected twice
            if(length(unique(inputs_mrt)) != length(columns_mortality)) {
              rlang::warn("It looks like you've matched the same variable to multiple labels:::Please match the variables to the corresponding label.")
              return()
            }

          }, warning = function(w) {

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
          # ----------------------------------- #

        } else if(current_page() == "variable_select_pop") {
          
          #### Population Variable Page ----
          
          # Process the inputs given on this page
          # ----------------------------------- #
          withCallingHandlers({

            columns_population <- c("year", "sex", "age.grp", "population")

            inputs_pop <- c(input$var_select_pop_year,
                            input$var_select_pop_sex,
                            input$var_select_pop_age.group,
                            input$var_select_pop_population)

            # Warning if sex values aren't correct
            if (req_population_data() & length(unique(c(input$male_val_pop, input$female_val_pop))) != 2) {
              rlang::warn("The values for <b>males</b> and <b>females</b> have to be different:::Please select different values")
              return()
            }
            
            # Warning if at least one column has been selected twice
            if(length(unique(inputs_pop)) != length(columns_population)) {
              rlang::warn("It looks like you've matched the same variable to multiple labels:::Please match the variables to the corresponding label.")
              return()
            }

          }, warning = function(w) {

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
          # ----------------------------------- #

        }
        
        #### Select new page ----
        current_page_index(current_page_index() + adder)

        nav_select(
          id = "container",
          selected = current_page()
        )

      }
      
    }) # EVENT: Next Button Clicked End
    # ------------------------------------------------------------------------ #
    
    
    
    ## EVENT: Previous Button Clicked ----
    # ------------------------------------------------------------------------ #
    observeEvent(input$previous_page, {
      # The behaviour of the previous button depends on the what the current
      # page is and whether population or mortality data has been loaded

      req(current_page_index() > 1)

      subtractor <- 1

      if (current_page() == "supplied_params") {
        
        if (!req_population_data()  & !req_mortality_data()) {
          subtractor <- 3
        } else if (!req_population_data()) {
          subtractor <- 2
        }
        
      } else if (current_page() == "variable_select_pop") {
        
        if (!req_mortality_data()) { subtractor <- 2 }
        
      }

      current_page_index(current_page_index() - subtractor)

      nav_select(id = "container", selected = current_page())

    })
    # ------------------------------------------------------------------------ #
    
    
    
    ## EVENT: Licensing Modal ----
    # ------------------------------------------------------------------------ #
    observeEvent(input$license, {

      showModal(
        modalDialog(
          size = "l",
          easyClose = TRUE,
          
          # Modal Body
          tagList(

            div(class = "directory-copy", span("CaRDO Licensing")),
            hr(),  # --- Horizontal Line --- #
            div(
              class = "license-text",
              p("The CaRDO software and associated materials are made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International Licence (CC BY-NC-SA 4.0).", tags$br(),
                "This licence allows users to:", tags$br()),
              tags$ul(
                tags$li("Share - copy and redistribute the material in any medium or format; and/or,"),
                tags$li("Adapt - remix, transform, and build upon the material,")
              ),
              p("Under the following conditions:", tags$br()),
              tags$ul(
                tags$li(tags$b("Attribution"), "- You must give appropriate credit, provide a link to the licence, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use. Please use this reference when providing credit: ", tags$blockquote("Retell, J., Francis, S., Rene, J., & Baade, P. (2025). CaRDO Handbook (Version 06-2025). Cancer Council Queensland: Viertel Cancer Research Centre. https://ccqresearch.github.io/CaRDO-Handbook/")),
                tags$li(tags$b("NonCommercial"), "- You may not use the material for commercial purposes without prior written permission."),
                tags$li(tags$b("ShareAlike"), "- If you remix, transform, or build upon the material, you must distribute your contributions under the same licence (CC BY-NC-SA 4.0). This ensures that any derivative works remain freely available under the same terms, supporting open collaboration and equitable access especially for low-resource cancer registries.")
              ),
              p("We welcome use by cancer registries and organisations, especially in low-resource settings. If your organisation requires customisations or enhancements to the base product, please contact us (statistics@cancerqld.org.au) to discuss potential cost-recovery arrangements.")
            )
          ),
          
          # Modal Footer
          footer = tagList(modalButton("I Agree"))
        )
      )

    })
    # ------------------------------------------------------------------------ #
    
    
    
    ## EVENT: Incorrect File Type ----
    # ------------------------------------------------------------------------ #
    observeEvent(input$data_inc_upload, {
      
      ext <- tools::file_ext(input$data_inc_upload$name)
      
      if (!ext %in% c("csv", "tsv", "dta")) {
        
        showNotification(
          "Incorrect file type - please load a .csv, .tsv or .dta file",
          type = "error",
          duration = 5
        )
        
        output$data_inc_upload_msg <- renderUI({
          tags$p(style = "color: red;", "Incorrect file type.")
        })
        
      } else {
        
        output$data_inc_upload_msg <- renderUI(NULL)
        
      }
      
    })
    # ------------------------------------------------------------------------ #
    
    
    
    ## Disabling Next/Previous ----
    # ------------------------------------------------------------------------ #
    observe({
      
      # LAST PAGE: Change 'Next' to 'Create Dashboard'
      if (current_page_index() == length(pages)) {
        updateActionButton(
          inputId = "next_page",
          label   = "Create Dashboard"
        )
      } else {
        updateActionButton(
          inputId = "next_page",
          label   = "Next"
        )
      }

      # MIDDLE PAGES: Enable 'Previous' to go back when possible
      if (current_page_index() > 1) {
        enable("previous_page")
      } else {
        disable("previous_page")
      }

      # ALL PAGES: Make sure users can't skip ahead without finishing
      if (!completed_page()) {disable("next_page")} else {enable("next_page")}

    })
    # ------------------------------------------------------------------------ #
    
    
    
    # ------------------------------------------------------------------------ #
    
    
    
    
    
    # ------------------------------------------------------------------------ #
    # LOAD DATA ----
    # ------------------------------------------------------------------------ #
    
    
    
    ## Incidence ----
    
    data_incidence <- reactive({

      if (is.null(input$data_inc_upload)) { return(NULL) }

      ext <- file_ext(input$data_inc_upload$name)

      # Save the ID for removal later
      # id_inc <- showNotification(paste("Load completed"), duration = 5)

      data <- switch(
        ext,
        "dta"  = read_dta(input$data_inc_upload$datapath),
        "csv"  = fread(input$data_inc_upload$datapath),
        "tsv"  = vroom(input$data_inc_upload$datapath, delim = "\t"),
        "xlsx" = readxl::read_excel(input$data_inc_upload$datapath),
        "xls"  = readxl::read_excel(input$data_inc_upload$datapath),
        "rds"  = readRDS(input$data_inc_upload$datapath),
        validate("Invalid file; Please load a .csv, .tsv, .dta, .xlsx, .xls or .rds file")
      )

      # removeNotification(id_inc)
      return(data)

    })
    
    # ----------------------------------- #

    ## Mortality ----
    
    data_mortality <- reactive({

      if (is.null(input$data_mrt_upload)) { return(NULL) }

      ext <- file_ext(input$data_mrt_upload$name)

      # Save the ID for removal later
      # id_mrt <- showNotification(paste("Load completed"), duration = 0)

      data <- switch(
        ext,
        "dta"  = read_dta(input$data_mrt_upload$datapath),
        "csv"  = fread(input$data_mrt_upload$datapath),
        "tsv"  = vroom(input$data_mrt_upload$datapath, delim = "\t"),
        "xlsx" = readxl::read_excel(input$data_mrt_upload$datapath),
        "xls"  = readxl::read_excel(input$data_mrt_upload$datapath),
        "rds"  = readRDS(input$data_mrt_upload$datapath),
        validate("Invalid file; Please load a .csv, .tsv, .dta, .xlsx, .xls or .rds file")
      )

      # removeNotification(id_mrt)
      return(data)

    })
    
    # ----------------------------------- #

    ## Population ----
    
    data_population <- reactive({

      if(is.null(input$pop_data_upload)){ return(NULL) }

      ext <- file_ext(input$pop_data_upload$name)

      # Save the ID for removal later
      # id_pop <- showNotification(paste("Load completed"), duration = 0)

      data <- switch(
        ext,
        "dta"  = read_dta(input$pop_data_upload$datapath),
        "csv"  = fread(input$pop_data_upload$datapath),
        "tsv"  = vroom(input$pop_data_upload$datapath, delim = "\t"),
        "xlsx" = readxl::read_excel(input$pop_data_upload$datapath),
        "xls"  = readxl::read_excel(input$pop_data_upload$datapath),
        "rds"  = readRDS(input$pop_data_upload$datapath),
        validate("Invalid file; Please load a .csv, .tsv, .dta, .xlsx, .xls or .rds file")
      )

      # removeNotification(id_pop)
      return(data)

    })
    # ------------------------------------------------------------------------ #
    
    
    
    
    
    # ------------------------------------------------------------------------ #
    # OUTPUTS ----
    # ------------------------------------------------------------------------ #
    
    
    
    ## Hide Previous Button on First Page ----
    # ------------------------------------------------------------------------ #
    
    output$hide_previous_btn <- reactive({
      if (current_page_index() == 1) TRUE else FALSE
    })
    
    outputOptions(output, 'hide_previous_btn', suspendWhenHidden = FALSE)
    
    # ------------------------------------------------------------------------ #
    
    
    
    ## Specify Incidence Outputs ----
    # ------------------------------------------------------------------------ #
    
    # Variable - for users to assign columns to standard variables
    output$select_var_inc <- renderUI({
      
      variable_names <- c(NA, names(data_incidence()))
      
      tagList(
        
        div(
          class = "menu-inline",
          
          # Select COUNT variable
          selectInput(inputId = "var_select_inc_counts",
                      choices = variable_names,
                      label   = HTML("Select your <b>count</b> variable"),
                      NA),
          # Select YEAR variable
          selectInput(inputId = "var_select_inc_year",
                      choices = variable_names,
                      label   = HTML("Select your <b>year</b> variable"),
                      NA),
          # Select CANCER variable
          selectInput(inputId = "var_select_inc_cancer.type",
                      choices = variable_names,
                      label   = HTML("Select your <b>cancer name</b> variable"),
                      NA),
          # Select AGE variable
          selectInput(inputId = "var_select_inc_age.group",
                      choices = variable_names,
                      label   = HTML("Select your <b>age group</b> variable"),
                      NA),
          # Select SEX variable
          selectInput(inputId = "var_select_inc_sex",
                      choices = variable_names,
                      label   = HTML("Select your <b>sex</b> variable"),
                      NA),
          div(
            class = "hint-div",
            p("Here, we standardise the variable names. Please match your variables with these listed above")
          )
        ),
        
        # Select the sex variable - Output below
        div(
          class = "menu-div",
          conditionalPanel(
            condition = "input.var_select_inc_sex != 'NA'",
            uiOutput(outputId = "select_var_inc_sex")
          )
        )
        
      )
    })
    
    # ----------------------------------- #
    
    # Assigning sex values
    output$select_var_inc_sex <- renderUI({
      
      tagList(
        # Pick the MALE category
        selectInput(inputId = "male_val",
                    label   = HTML("Which value in your 'sex' column represents <b>males</b>?"),
                    choices = unique(data_incidence()[[input$var_select_inc_sex]])),
        # Pick the female category
        selectInput(inputId = "female_val",
                    label   = HTML("Which value in your 'sex' column represents <b>females</b>?"),
                    choices = unique(data_incidence()[[input$var_select_inc_sex]]))
      )
      
    })

    # ------------------------------------------------------------------------ #
    
    
    
    ## Specify Mortality Outputs ----
    # ------------------------------------------------------------------------ #
    
    # Variable - for users to assign columns to standard variables
    output$select_var_mrt <- renderUI({
      
      if (req_mortality_data()) {
        
        variable_names <- c(NA, names(data_mortality()))
        
        tagList(
          
          div(
            class = "menu-inline",
            
            # Select COUNT variable
            selectInput(inputId = "var_select_mrt_counts",
                        choices = variable_names,
                        label = HTML("Select your <b>count</b> variable"),
                        NA),
            # Select YEAR variable
            selectInput(inputId = "var_select_mrt_year",
                        choices = variable_names,
                        label = HTML("Select your <b>year</b> variable"),
                        NA),
            # Select CANCER variable
            selectInput(inputId = "var_select_mrt_cancer.type",
                        choices = variable_names,
                        label = HTML("Select your <b>cancer name</b> variable"),
                        NA),
            # Select AGE variable
            selectInput(inputId = "var_select_mrt_age.group",
                        choices = variable_names,
                        label = HTML("Select your <b>age group</b> variable"),
                        NA),
            # Select SEX variable
            selectInput(inputId = "var_select_mrt_sex",
                        choices = variable_names,
                        label = HTML("Select your <b>sex</b> variable"),
                        NA),
            div(
              class = "hint-div",
              p("Here, we standardise the variable names. Please match your variables with these listed above")
            )
          ),
          
          # Select the sex variable - Output below
          div(
            class = "menu-div",
            conditionalPanel(
              condition = "input.var_select_mrt_sex != 'NA'",
              uiOutput(outputId = "select_var_mrt_sex")
            )
          )
          
        )
        
      } else {
        
        h5("Not required, as a mortality file has not been loaded.")
      
      }
    })
    
    # ----------------------------------- #

    # Assigning sex values
    output$select_var_mrt_sex <- renderUI({
      
      tagList(
        # Pick the MALE category
        selectInput(inputId = "male_val_mrt",
                    label = HTML("Which value in your 'sex' column represents <b>males</b>?"),
                    choices = unique(data_mortality()[[input$var_select_mrt_sex]])),
        # Pick the FEMALE category
        selectInput(inputId = "female_val_mrt",
                    label = HTML("Which value in your 'sex' column represents <b>females</b>?"),
                    choices = unique(data_mortality()[[input$var_select_mrt_sex]]))
      )
      
    })
    
    # ------------------------------------------------------------------------ #
    
    
    
    ## Specify Population Outputs ----
    # ------------------------------------------------------------------------ #
    
    # Variable - for users to assign columns to standard variables
    output$select_var_pop <- renderUI({
      
      if (req_population_data()) {
        
        variable_names <- c(NA, names(data_population()))
        
        tagList(
          
          div(
            class = "menu-inline",
            
            # Select POPULATION variable
            selectInput(inputId = "var_select_pop_population",
                        choices = variable_names,
                        label = HTML("Select your <b>population</b> variable"),
                        NA),

            condition = "input.variables_pop.includes('year')",
            
            # Select YEAR variable
            selectInput(inputId = "var_select_pop_year",
                        choices = variable_names,
                        label = HTML("Select your <b>year</b> variable"),
                        NA),
            # Select AGE variable
            selectInput(inputId = "var_select_pop_age.group",
                        choices = variable_names,
                        label = HTML("Select your <b>age group</b> variable"),
                        NA),
            # Select SEX variable
            selectInput(inputId = "var_select_pop_sex",
                        choices = variable_names,
                        label = HTML("Select your <b>sex</b> variable"),
                        NA),
            div(
              class = "hint-div",
              p("Here, we standardise the variable names. Please match your variables with these listed above")
            )
            
          ),
          
          # Select the sex variable - Output below
          div(
            class = "menu-div",
            conditionalPanel(
              condition = "input.var_select_pop_sex != 'NA'",
              uiOutput(outputId = "select_var_pop_sex")
            )
          )

        )
      } else {
        
        h5("Not required, as a population file has not been loaded.")
        
      }
    })

    # ----------------------------------- #
    
    # Assigning sex values
    output$select_var_pop_sex <- renderUI({
      
      tagList(
        # Pick the MALE category
        selectInput(inputId = "male_val_pop",
                    label = HTML("Which value in your 'sex' column represents <b>males?</b>"),
                    choices = unique(data_population()[[input$var_select_pop_sex]])),
        # Pick the FEMALE category
        selectInput(inputId = "female_val_pop",
                    label = HTML("Which value in your 'sex' column represents <b>females?</b>"),
                    choices = unique(data_population()[[input$var_select_pop_sex]]))
      )
      
    })
    
    # ------------------------------------------------------------------------ #
    
    
    
    ## All Cancers Category OPTION ----
    # ------------------------------------------------------------------------ #
    
    # This output requires a variable to exist before running
    output$select_all_canc_var <- renderUI({
      
      req(input$var_select_inc_cancer.type) 
      
      # # Retrieving list of unique cancers from 'Specifying Incidence' page
      # cancer_list  <- data_incidence() %>%
      #   dplyr::select(input$var_select_inc_cancer.type) %>%
      #   unique() %>%
      #   str_to_title() %>%
      #   sort()
      # 
      # variable_names <- c("Please specify ..", cancer_list)
      
      temp_cancer_names <- input$var_select_inc_cancer.type
      
      variable_names <- c("Please specify ..",
                          unique(data_incidence()[[temp_cancer_names]]) %>%
                            stringr::str_to_title() %>%
                            sort())
      
      tagList(
        
        div(
          class = "panel-body",
          
          # All cancers category...?
          prettyRadioButtons(
            inputId  = "bool_all_canc",
            label    = "Do you have a cancer category indicating all cancers?",
            choices  = c("No", "Yes"),
            shape    = "square",
            icon     = icon("check"),
            selected = "Yes"),
          
          # ... If YES, which is it?
          conditionalPanel(
            condition = "input.bool_all_canc == 'Yes'",
            selectInput(inputId = "all_canc_name",
                        label = "Select the cancer category indicating 'all cancers'",
                        choices = variable_names,
                        selected = "Please specify ..")
          ),
          
          # ... If NO, we will create one
          conditionalPanel(
            condition = "input.bool_all_canc == 'No'",
            div(
              class = "hint-div",
              p("CaRDO will report statistics for 'all cancers'.
                If you select NO, CaRDO will calculate 'all cancers reported' by summing the cancer categories you supply.
                Only tick NO if your data does NOT contain any overlapping cancer categories (e.g. cervical cancer and gynaecological cancers) as this will result in double counting.")
            )
          )
          
        )
      )
      
    }) # output$select_all_canc_var
    
    # ----------------------------------- #
    
    # Render this output on app startup
    outputOptions(output, "select_all_canc_var", suspendWhenHidden = FALSE)

    # ------------------------------------------------------------------------ #
    
    
    
    ## Aggregate Years OPTION ----
    # ------------------------------------------------------------------------ #
    
    # For users with very little data, we can turn years into year ranges
    output$aggregation_var <- renderUI({

      tagList(
        
        # Aggregate the years...?
        checkboxInput(
          inputId = "aggregate_option",
          label   = "Do you want to aggregate and report your data by 5-year intervals (eg. 2018-2022)?",
          value   = FALSE),
        
        # ... If YES, this is what we'll do
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
    
    # ------------------------------------------------------------------------ #
    
    
    
    
    
    # ------------------------------------------------------------------------ #
    # FINAL EVENTS ----
    # ------------------------------------------------------------------------ #
    
    
    
    ## Copy filepath button ----
    # The button for copying the filepath once the dashboard has been created
    observeEvent(input$clipbtn, {
      
      updateActionButton(
        session,
        inputId = "clipbtn",
        label   = "Copied!"
      )
      
    })

    ## Close app when finished ----
    # Finalise everything, and exit
    observeEvent(input$confirm, { if (input$confirm) {stopApp()} })
    
    # ------------------------------------------------------------------------ #


  }
  
  # -------------------------------------------------------------------------- #
  # SERVER END ----
  # -------------------------------------------------------------------------- # 

  runApp(shinyApp(ui, server), launch.browser = browserViewer())

}

