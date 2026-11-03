UI_module <- function(id){
  ns <- NS(id)

  most_recent_year <- most_recent_years[[id]]
  if (is.null(most_recent_year)) stop("No most_recent_year defined for module: ", id)

  ltr_period <- switch(
    id,
    "Diagnoses" = unique(inc_averages$year),
    "Deaths"    = unique(mrt_averages$year),
    stop("No ltr_period defined for module: ", id)
  )
  if (length(ltr_period) != 1) stop("Expected one lifetime risk period for module: ", id)

  tagList(
    page_fillable(
      ## Dashboard Header & Controls ----
      div(
        class = "header-options",
        ### Element 1 - Cancer Menu ----
        div(
          class = "cancer-menu",
          selectInput(inputId = ns("cancer.type"),
                      choices = if (id == "Diagnoses") {cancer_choices_inc} else {cancer_choices_mrt},
                      label = "Cancer type")
        ),

        ### Element 2 - Panel Title ----
        h1(id, style = "color: white !important;"),

        ### Element 3 - Radio Buttons ----
        div(
          class = "gender-measure",
          radioGroupButtons(inputId = ns("sex"),
                            # choices = sex_choices,
                            choices = c("Persons" = 3, "Males" = 1, "Females" = 2),
                            label = "Sex"),

          radioGroupButtons(inputId = ns("measure"),
                            choices = setNames(measure_choices,
                                               stringr::str_to_title(measure_choices)),
                            label = "Measure")
        )
      ),
      # ---------------------------------------------------------------------- #

      ## Dashboard Content / Grid ----
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        ### Top-left - Summary ----
        card(
          div(
            class = "summary",

            div(
              class = "summary-content",

              div(
                class = "summary-title",
                div(
                  class = "title",
                  h1(HTML("Summary")),
                  h2(location_name)
                ),
                div(
                  class = "info",
                  h2(all_cancers_name),
                  h2(if(to_aggregate) {"For the years "} else {"For the year "}, most_recent_year),
                  h2(age_scope, style = "opacity: 0.6;font-size: 0.90em;")
                )
              ),
              hr(),
              div(
                class = "summary-stats",
                div(
                  class = "content-row",
                  uiOutput(ns("all_text"))
                ),
                div(
                  class = "content-row",
                  uiOutput(ns("male_text"))
                ),
                div(
                  class = "content-row",
                  uiOutput(ns("female_text"))
                )
              )

            ),
            if (length(measure_choices) != 1) {
              div(
                class = "lifetime-risk",
                div(
                  class = "ltr-vis",
                  uiOutput(ns("ltr_vis"))
                ),
                div(
                  class = "ltr-text",
                  h2("Lifetime Risk",
                     span(paste0("(", ltr_period, ")"),
                          style = "display: block; opacity: 0.6; font-size: 0.6em; font-weight: normal;")),
                  uiOutput(ns("ltr_text"))
                ),
                div(
                  id = "ltr-tooltip",
                  class = "ltr-tooltip"
                )
              )
            } else {
              div(class = "cardo-logo")
            }

          )
        ),
        ### Top-right - Over Time ----
        card(
          card_header(
            uiOutput(outputId = ns("title_topright"))
          ),
          card_body(
            plotlyOutput(outputId = ns("topright"))
          )
        ),
        ### Bottom-left - Cancers ----
        card(
          card_header(
            uiOutput(outputId = ns("title_bottomleft"))
          ),
          card_body(
            plotlyOutput(outputId = ns("bottomleft"))
          )
        ),
        ### Bottom-right - Age groups ----
        card(
          card_header(
            uiOutput(outputId = ns("title_bottomright"))
          ),
          card_body(
            plotlyOutput(outputId = ns("bottomright"))
          )
        )
      )
      # ---------------------------------------------------------------------- #
    ) #page_fillable
  ) #taglist
}

server_module <- function(id){

  most_recent_year <- most_recent_years[[id]]
  if (is.null(most_recent_year)) stop("No most_recent_year defined for module: ", id)

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      counts_rates_labs <- reactive({
        paste0(
          ifelse(id == "Diagnoses",
                 yes = "Diagnoses",
                 no = id),
          ifelse(input$measure == "counts",
                 yes = "",
                 no = " per 100,000 pop.")
        )
      })

      # For X-Unified hover templates
      label_word <- reactive({
        if (id == "Diagnoses") {
          if (input$measure == "rates") "Diagnosis Rates" else "Diagnoses"
        } else {
          if (input$measure == "rates") "Death Rates" else "Deaths"
        }
      })

      hover_fmt <- reactive({ paste0("%Y ", label_word(), "") })

      ## Data loading ----
      # ---------------------------------------------------------------------- #
      
      if (id == "Diagnoses") {
        ### Incidence ----

        data_topleft <- reactive({
          #### top-left data ----
          inc_annual_counts %>%
            filter(
              if ("max_year" %in% names(.)) {
                stringr::str_detect(year, as.character(unique(max_year)))
              } else {
                year == max(year)
              },
              cancer.type == all_cancers_name,
              measure     == input$measure
            ) %>%
            group_by(year, sex) %>%
            summarise(obs = sum(obs), .groups = 'drop')
          
        })

        lifetime_risk <- reactive({
          
          if (!no_ltr) {
            df <- inc_ltr %>%
              filter(
                cancer.type == input$cancer.type,
                sex         == input$sex
              )
          } else {
            return(0)
          }
          
        })

        data_topright <- reactive({
          #### top-right data ----
          inc_annual_counts %>%
            filter(
              cancer.type == input$cancer.type,
              measure     == input$measure
            )
          
        })

        data_bottomleft <- reactive({
          #### bottom-left data ----
          if (input$sex == 3) {
            
            top5_inc %>% filter(measure == input$measure)
            
          } else {
            
            inc_averages %>%
              filter(
                cancer.type != all_cancers_name,
                sex         == input$sex,
                measure     == input$measure
              ) %>%
              slice_max(order_by = obs, n = 5)
            
          }
          
        })

        data_bottomright <- reactive({
          #### bottom-right data ----
          df <- inc_counts %>%
            filter(
              cancer.type == input$cancer.type,
              sex         == input$sex,
              measure     == input$measure
            )
          
          if (nrow(df) == 0) return(df)
          
          df %>%
            group_by(year) %>%
            complete(age.label = age_ranges) %>%
            ungroup() %>%
            mutate(
              "age.label" = if (n() > 0) {
                factor(age.label, levels = sort(age_ranges))
              } else {
                character(0)
              }
            ) %>%
            ungroup()

        })

      } else if (id == "Deaths") {
        ### Mortality ----
        
        data_topleft <- reactive({
          #### top-left data ----
          mrt_annual_counts %>%
            filter(
              if ("max_year" %in% names(.)) {
                stringr::str_detect(year, as.character(unique(max_year)))
              } else {
                year == max(year)
              },
              cancer.type == all_cancers_name,
              measure     == input$measure
            ) %>%
            group_by(year, sex) %>%
            summarise(obs = sum(obs), .groups = 'drop')
          
        })

        lifetime_risk <- reactive({
          
          if (!no_ltr) {
            mrt_ltr %>%
              filter(
                cancer.type == input$cancer.type,
                sex         == input$sex
              )
          } else {
            return(0)
          }
          
        })

        data_topright <- reactive({
          #### top-right data ----
          mrt_annual_counts %>%
            filter(
              measure == input$measure,
              cancer.type == input$cancer.type
            )
          
        })

        data_bottomleft <- reactive({
          #### bottom-left data ----
          if (input$sex == 3) {
            
            top5_mrt %>% filter(measure == input$measure)
            
          } else {
            
            mrt_averages %>%
              filter(
                cancer.type != all_cancers_name,
                sex         == input$sex,
                measure     == input$measure
              ) %>%
              slice_max(order_by = obs, n = 5)
            
          }
          
        })

        data_bottomright <- reactive({
          #### bottom-right data ----
          df <- mrt_counts %>%
            filter(
              cancer.type == input$cancer.type,
              sex         == input$sex,
              measure     == input$measure
            )
          
          if (nrow(df) == 0) return(df)
          
          df %>%
            group_by(year) %>%
            complete(age.label = age_ranges) %>%
            ungroup() %>%
            mutate(
              "age.label" = if (n() > 0) {
                factor(age.label, levels = sort(age_ranges))
              } else {
                character(0)
              }
            ) %>%
            ungroup()
          
        })
      
      }

      # ---------------------------------------------------------------------- #

      ## Outputs ----

      ### TL-Summary Outputs ----
      output$all_text <- renderUI({
        HTML(
          paste0(
            data_topleft() %>%
              filter(sex == 3) %>%
              mutate(obs = round(obs, 1)) %>%
              pull(obs) %>%
              format(big.mark = ",", scientific = FALSE)
          ),
          "<h3>All cancer", tolower(counts_rates_labs()), "</h3>"
        )
      })
      output$male_text <- renderUI({
        HTML(
          paste0(
            data_topleft() %>%
              filter(sex == 1) %>%
              mutate(obs = round(obs, 1)) %>%
              pull(obs) %>%
              format(big.mark = ",", scientific = FALSE)
          ),
          "<h3>All male cancer", tolower(counts_rates_labs()), "</h3>"
        )
      })
      output$female_text <- renderUI({
        HTML(
          paste0(
            data_topleft() %>%
              filter(sex == 2) %>%
              mutate(obs = round(obs, 1)) %>%
              pull(obs) %>%
              format(big.mark = ",", scientific = FALSE)
          ),
          "<h3>All female cancer", tolower(counts_rates_labs()), "</h3>"
        )
      })

      output$ltr_vis <- renderUI({
        
        dataframe <- lifetime_risk()
        no_data <- !(input$sex %in% dataframe$sex)
        validate(need(!no_data, "Data unavailable."))
        ltr_value <- dataframe %>% pull(lifetime.risk)
        
        tags$svg(
          class = "ltr-matrix",
          width = "100%",
          height = "100%",
          viewBox = "0 0 7 100",
          preserveAspectRatio = "xMidYMid meet",
          
          lapply(1:10, function(i) {
            
            colour <- ifelse(
              i <= round(ltr_value * 10),
              if(id == "Diagnoses") "#1C54A8" else "#8E3E39",
              "#E6E6E6"
            )
            
            tags$circle(
              class = "data-circle",
              `data-info` = "Each circle represents 1 in 10 persons", cx = "50%",
              cy = (10 - i) * 10 + 5,
              r = 3,
              fill = colour
            )
          })
          
        )
        
      })

      output$ltr_text <- renderUI({
        
        dataframe <- lifetime_risk()
        no_data <- !(input$sex %in% dataframe$sex)
        validate(need(!no_data, "Data unavailable."))
        
        ltr_value <- dataframe %>% pull(lifetime.risk)
        ltr_stat <- if (ltr_value < 0.1 & ltr_value != 0) "< 1" else round(ltr_value * 10)

        stat_text <- if (ltr_value == 0) {
          "<p>No <b>"
        } else {
          paste0("<p>Approximately 1 in <b>", round(1/ltr_value, 1))
        }

        div(
          p(style = "font-size: clamp(14px, 1vw, 18px); margin-bottom: 1rem;", input$cancer.type),
          HTML(paste("<b>", ltr_stat, "</b> out of 10")),
          div(
            class = "ltr-info",
            HTML(paste(
              stat_text,
              if (input$sex == 3) {"persons"} else {if (input$sex == 1) {"males"} else {"females"}},
              if (id == "Diagnoses") {"</b>are expected to be diagnosed with <b>"} else {"</b>are expected to die from <b>"},
              tolower(input$cancer.type),
              "</b> by age 85</p>"
            )
            )
          )
        )
      })

      # ---------------------------------------------------------------------- #
      ### TR-Time Outputs ----
      title_topright <- reactive({
        heading_title <- if(input$measure == "counts") {
          if (id == "Diagnoses") "Number of diagnoses" else "Number of deaths"
        } else {
          if (id == "Diagnoses") "Diagnosis rates" else "Death rates"
        }
        heading_cancer <- tolower(input$cancer.type)
        heading_sex <- if(input$sex == 3) {
          " "
        } else {
          if(input$sex == 1) " in males" else " in females"
        }

        return (
          paste0(heading_title, " for ", heading_cancer, heading_sex, " (", location_name, ")")
        )
      })

      output$title_topright <- renderUI({

        heading_bracket <- if(input$measure == "counts") "" else " (Age Standarised)"

        div(
          class = "custom-heading",
          span(
            class = "heading-bold",
            title_topright()
          ),
          span(
            class = "heading-bracket",
            span(
              class = 'dl_pos',
              dwnldButtonUI(id = ns("topright_dwnld")) %>%
                tooltip("Download plot data",
                        placement = "bottom",
                        options = list(customClass = "info_tooltip")
                )
            ),
            paste0(heading_bracket)
          )
        )
      })

      output$topright <- renderPlotly({

        dataframe  <- data_topright()
        columns    <- names(dataframe)
        has_trends <- if ("trends" %in% columns) TRUE else FALSE
        
        no_data <- !(input$sex %in% dataframe$sex)
        validate(need(!no_data, "Data unavailable. If cancer is sex-specific, try changing to that sex."))

        categories  <- c("1", "2", "3")
        plot_colour <- if(id == "Diagnoses") "#1C54A8" else "#8E3E39"

        line_styles <- sapply(categories, function(cat) {
          if (cat == input$sex) {
            list(color = "#808080")
          } else {
            list(color = "#DBDBDB", dash = "dash")
          }
        }, simplify = FALSE)

        marker_styles <- sapply(categories, function(cat) {

          if (has_trends & cat == input$sex) {
            list(
              size  = 9,
              color = plot_colour
            )
          } else if (!has_trends & cat == input$sex) {
            list(
              size  = 12,
              color = plot_colour,
              line  = list(color = "white", width = 3)
            )
          } else {
            list(opacity = 0)
          }

        })

        hovertoggle <- sapply(categories, function(cat) {
          if (cat == input$sex) { "all" } else { "skip" }
        })

        legendtoggle <- sapply(categories, function(cat) {
          if (cat == input$sex) { TRUE } else { FALSE }
        })

        plot <- plot_ly(
          hovertemplate = paste0("<span style='font-weight: 500;'>%{y:,.1f}      </span>")
        ) %>%
          config(
            modeBarButtonsToRemove = plotly_btns_rm,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = title_topright()
            ),
            scrollZoom = FALSE
          ) %>%
          layout(
            hovermode = "x unified",
            legend = list(
              itemclick = FALSE,
              itemdoubleclick = FALSE
            ),
            xaxis = list(
              # type = "date",
              # tickformat = "%Y",
              hoverformat = hover_fmt(),
              fixedrange = TRUE,
              title = "Year",
              linewidth = 2,
              linecolor = "#000000",
              tickcolor = "#000000",
              ticks = "outside",
              zeroline = TRUE
            ),
            yaxis = list(
              fixedrange = TRUE,
              title = counts_rates_labs(),
              rangemode = 'tozero',
              zeroline = FALSE
            ),
            showlegend = TRUE,
            paper_bgcolor = 'transparent',
            plot_bgcolor = 'transparent',
            font = list(color = '#111111'),
            modebar = list(
              bgcolor = 'rgba(0,0,0,0)',
              color = 'rgba(0,0,0,1)',
              activecolor = '#111111'
            )
          )

        for(sex_num in unique(dataframe$sex)) {

          if (has_trends) {
            
            plot <- plot %>%
              add_trace(
                data = dataframe %>% filter(sex == sex_num),
                x = ~year,
                y = ~trends,
                name = paste(sex_name[[sex_num]], "trends"),
                type = "scatter",
                mode = "lines",
                line = line_styles[[sex_num]],
                showlegend = FALSE
              ) %>%
              add_trace(
                data = dataframe %>% filter(sex == sex_num),
                x = ~year,
                y = ~obs,
                name = sex_name[[sex_num]],
                type = "scatter",
                mode = "markers",
                marker = marker_styles[[sex_num]]
              )
            
          } else {

            plot <- plot %>%
              add_trace(
                data = dataframe %>% filter(sex == sex_num),
                x = ~year,
                y = ~obs,
                name = sex_name[[sex_num]],
                type = "scatter",
                mode = "markers+lines",
                line = line_styles[[sex_num]],
                marker = marker_styles[[sex_num]]
              )

          }
        }

        plot

      })

      dwnldButtonServer(id = "topright_dwnld",
                        file_name = title_topright,
                        graph_df = data_topright)

      # ---------------------------------------------------------------------- #

      ### BL-Cancer Outputs ----
      title_bottomleft <- reactive({
        heading_title <- if(id == "Diagnoses") "diagnoses" else "deaths"
        heading_sex <- if(input$sex == 3) {
          " "
        } else {
          if(input$sex == 1) " in males" else " in females"
        }

        return(
          paste0("The five most common cancer ", heading_title, heading_sex, " (", location_name, ")")
        )
      })

      output$title_bottomleft <- renderUI({

        if (to_aggregate) {
          heading_bracket <- if(input$measure == "counts") {
            paste0("(5-year Average Counts ", unique(data_bottomleft()$year), ")")
          } else {
            paste0("(5-year Age Standardised Rates ", unique(data_bottomleft()$year), ")")
          }

        } else {

          heading_bracket <- if(input$measure == "counts") {
            paste0("(5-year Average Counts ", most_recent_year-4, "-", most_recent_year, ")")
          } else {
            paste0("(5-year Age Standardised Rates ", most_recent_year-4, "-", most_recent_year, ")")
          }
        }

        div(
          class = "custom-heading",
          span(
            class = "heading-bold",
            title_bottomleft()
          ),
          span(
            class = "heading-bracket",
            span(
              class = 'dl_pos',
              dwnldButtonUI(id = ns("bottomleft_dwnld")) %>%
                tooltip("Download plot data",
                        placement = "bottom",
                        options = list(customClass = "info_tooltip")
                )
            ),
            paste0(heading_bracket)
          )
        )
      })

      output$bottomleft <- renderPlotly({

        plot_colour <- if(id == "Diagnoses") "#335C98" else "#8E3E39"
        hover_colour <- if(id == "Diagnoses") "#24406B" else "#6A2E2A"

        cancer_axis_limit <- if(input$measure == "counts") {counts_limit} else {rates_limit}

        data_bl <- data_bottomleft()

        hover_card <- if (input$measure == "counts") {
          paste0("<i>5-year Average</i><br>",
                 "%{y}: <span style='font-weight: 500;'>%{x:,.1f}</span>",
                 "<extra></extra>")
        } else {
          paste0("<i>5-year Rate</i><br>",
                 "%{y}: <span style='font-weight: 500;'>%{x:,.1f}</span>",
                 "<extra></extra>")
        }

        plot_ly(data = data_bl)%>%
          add_bars(
            x = ~obs,
            y = ~reorder(cancer.type, obs),
            type = 'bar',
            width = 0.5,
            marker = list(color = plot_colour),
            hovertemplate = hover_card
          ) %>%
          config(
            modeBarButtonsToRemove = plotly_btns_rm,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = title_bottomleft()
            ),
            scrollZoom = FALSE
          ) %>%
          layout(
            xaxis = list(
              title = counts_rates_labs(),
              range = list(0, cancer_axis_limit),
              fixedrange = TRUE,
              zeroline = FALSE
            ),
            yaxis = list(
              title = "",
              fixedrange = TRUE,
              rangemode = 'tozero'
            ),
            hoverlabel = list(
              bgcolor = "#FFFFFF",
              bordercolor = "#000000",
              font = list(color = "#000000")
            ),
            margin = list(t = 0, b = 0, l = 0, r = 0, pad = 15),
            paper_bgcolor = 'transparent',
            plot_bgcolor = 'transparent',
            font = list(color = '#111111'),
            modebar = list(
              bgcolor = 'rgba(0,0,0,0)',
              color = 'rgba(0,0,0,1)',
              activecolor = '#111111'
            )
          )

      })

      dwnldButtonServer(id = "bottomleft_dwnld",
                        file_name = title_bottomleft,
                        graph_df = data_bottomleft)

      # ---------------------------------------------------------------------- #

      ### BR-Age Outputs ----
      title_bottomright <- reactive({
        heading_title <- if(input$measure == "counts") {
          if(id == "Diagnoses") "Number of diagnoses" else "Number of deaths"
        } else {
          if(id == "Diagnoses") "Diagnosis rates" else "Death rates"
        }
        heading_cancer <- tolower(input$cancer.type)
        heading_sex <- if(input$sex == 3) {
          " "
        } else {
          if(input$sex == 1) " in males" else " in females"
        }

        return(
          paste0(heading_title, ", by age group, for ", heading_cancer, heading_sex, " (", location_name, ")")
        )
      })

      output$title_bottomright <- renderUI({

        heading_bracket <- if(input$measure == "counts") "" else " (Age Standarised)"

        div(
          class = "custom-heading",
          span(
            class = "heading-bold",
            title_bottomright()
          ),
          span(
            class = "heading-bracket",
            span(
              class = 'dl_pos',
              dwnldButtonUI(id = ns("bottomright_dwnld")) %>%
                tooltip("Download plot data",
                        placement = "bottom",
                        options = list(customClass = "info_tooltip")
                )
            ),
            paste0(heading_bracket)
          )
        )
      })

      output$bottomright <- renderPlotly({

        no_data <- nrow(data_bottomright()) == 0 || all(is.na(data_bottomright()$obs))
        validate(need(!no_data, "Data unavailable. If cancer is sex-specific, try changing to that sex."))

        age_colours <- c("#FF9F1C", "#7DDE9D", "#3A8592", "#042A2B")
        age_groups <- unique(data_bottomright()$age.label) %>%
          sort

        plot <- plot_ly() %>%
          layout(
            xaxis = list(
              title = "Year",
              # type = "date",
              # tickformat = "%Y",
              hoverformat = hover_fmt(),
              fixedrange = TRUE,
              linewidth = 2,
              linecolor = "#000000",
              tickcolor = "#000000",
              ticks = "outside"
            ),
            yaxis = list(
              rangemode = "tozero",
              zeroline = FALSE,
              title = counts_rates_labs(),
              fixedrange = TRUE
            ),
            legend = list(
              title = list(text = "Age group (years)"),
              x = 0, y = 80,
              orientation = 'h'
            ),
            hovermode = "x unified",
            modebar = list(
              bgcolor = 'rgba(0,0,0,0)',
              color = 'rgba(0,0,0,1)',
              activecolor = '#111111'
            )
          ) %>%
          config(
            modeBarButtonsToRemove = plotly_btns_rm,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = title_bottomright()
            ),
            scrollZoom = FALSE
          )


        for (i in rev(seq_along(age_groups))) {

          selected_colour <- age_colours

          filt_df <- data_bottomright() %>%
            filter(age.label == age_groups[i]) %>%
            arrange(year) %>%
            rowwise()


          plot <- plot %>%
            add_trace(
              data = filt_df,
              x = ~year,
              y = ~obs,
              name = paste0(age_groups[i], " years"),
              mode = 'markers+lines',
              type = 'scatter',
              showlegend = TRUE,
              line = list(color = age_colours[i]),
              marker = list(size = 12,
                            color = age_colours[i],
                            line = list(color = "white", width = 3)
              ),
              hovertemplate = paste0("<span style='font-weight: 500;'>%{y:,.1f}      </span>")
            )
        }

        plot
      })

      dwnldButtonServer(id = "bottomright_dwnld",
                        file_name = title_bottomright,
                        graph_df = data_bottomright)


    }
  )
}
