UI_module <- function(id){
  ns <- NS(id)
  
  tagList(
    page_fillable(
      ## Dashboard Header & Controls ----
      div(
        class = "header-options",
        ### Element 1 - Cancer Menu ----
        div(
          class = "cancer-menu",
          selectInput(inputId = ns("cancer.type"),
                      choices = if (id == "Diagnosis") {cancer_choices_inc} else {cancer_choices_mrt},
                      label = "Cancer type")
          # downloadButton(outputId = ns("report"),
          #                label = "Report") %>%
          #   tooltip("Download a '.html' report of the selected cancer",
          #           placement = "right",
          #           options = list(customClass = "info_tooltip")
          #   )
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
                            choices = measure_choices,
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
            class = "summary-container",
            div(
              class = "summary-title",
              div(
                class = "summary",
                h1(HTML("Summary")),
                span(location_name),
                span("For the year ", most_recent_year)
              )
            ),
            div(
              class = "summary-content",
              div(
                class = "content-row",
                # textOutput(ns("all_value")),
                uiOutput(ns("all_text"))
              ),
              div(
                class = "content-row",
                # textOutput(ns("male_value")),
                uiOutput(ns("male_text"))
              ),
              div(
                class = "content-row",
                # textOutput(ns("female_value")),
                uiOutput(ns("female_text"))
              )
            ),
            if(length(measure_choices) != 1) {
              div(
                class = "lifetime-risk",
                div(
                  class = "ltr-vis",
                  uiOutput(ns("ltr_vis"))
                ),
                div(
                  class = "ltr-text",
                  h2("Lifetime Risk"),
                  uiOutput(ns("ltr_text"))
                ),
                div(
                  id = "ltr-tooltip",
                  class = "ltr-tooltip"
                )
              )
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


surv_module <- function(id) {
  ns <- NS(id)
  
  tagList(
    page_fillable(
      ## Dashboard Header & Controls ----
      div(
        class = "header-options",
        ### Element 1 - Cancer Menu ----
        div(
          class = "cancer-menu",
          selectInput(inputId = ns("cancer.type"),
                      choices = if (id == "Diagnosis" || id == "Survival") {cancer_choices_inc} else {cancer_choices_mrt},
                      label = "Cancer type")
          # downloadButton(outputId = ns("report"),
          #                label = "Report") %>%
          #   tooltip("Download a '.html' report of the selected cancer",
          #           placement = "right",
          #           options = list(customClass = "info_tooltip")
          #   )
        ),
        
        ### Element 2 - Panel Title ----
        h1(id, style = "color: white !important;"),
        
        ### Element 3 - Radio Buttons ----
        div(
          class = "gender-measure",
          radioGroupButtons(inputId = ns("sex"),
                            # choices = sex_choices,
                            choices = c("Persons" = 3, "Males" = 1, "Females" = 2),
                            label = "Sex")
        )
      ),
      # ---------------------------------------------------------------------- #
      
      ## Dashboard Content / Grid ----
      layout_columns(
        col_widths = c(2, 5, 5),
        
        # Left column (25%)
        div(
          class = "survival-left-column",
          
          div(
            class = "survival-summary",
            div(
              class = "survival-title",
              span("5-year", style = "font-size: 18px; font-weight: 400; margin-top: -0.5rem;"),
              span("Relative Surival", style = "font-size: 30px; font-weight: 300;")
            ),
            
            div(
              uiOutput(ns("rsurv_text"))
            ),
            
            div(
              uiOutput(ns("rsurv_vis"))
            )
          ),
          
          div(class = "hline"),
          
          div(
            class = "ranks",
            div(
              class = "ranks-heading",
              span("Highest 3", style = "text-align: center; font-size: 24px; font-weight: 500;")
            ),
            div(
              uiOutput(ns("rsurv_high"))
            )
          ),
          
          div(class = "hline"),
          
          div(
            class = "ranks",
            div(
              class = "ranks-heading",
              span("Lowest 3", style = "text-align: center; font-size: 24px; font-weight: 500;")
            ),
            div(
              uiOutput(ns("rsurv_low"))
            )
          )
        ),
        
        # Middle column (37.5%)
        div(
          style = "display: flex; flex-direction: column; gap: 1rem; height: 100%;",
          
          card(
            style = "flex: 1;",
            card_header(uiOutput(outputId = ns("surv_title_km"))),
            card_body(plotlyOutput(outputId = ns("surv_km"), height = "100%"))
          ),
          
          card(
            style = "flex: 1;",
            card_header(uiOutput(outputId = ns("surv_title_agegroup"))),
            card_body(plotlyOutput(outputId = ns("surv_agegroup"), height = "100%"))
          )
          
        ),
        
        # Right column (37.5%)
        card(
          style = "flex: 1",
          card_header(uiOutput(outputId = ns("surv_title_timeperiod"))),
          card_body(plotlyOutput(outputId = ns("surv_timeperiod"), height = "100%"))
        )
      )
      # ---------------------------------------------------------------------- #
    ) #page_fillable
  ) #taglist
  
}

server_module <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      counts_rates_labs <- reactive({
        paste0(
          ifelse(id == "Diagnosis",
                 yes = "Diagnoses",
                 no = id),
          ifelse(input$measure == "Counts",
                 yes = "",
                 no = " per 100,000 pop.")
        )
      })
      
      ## Data loading ----
      
      fiveyear_survival <- reactive({
        survival_data %>%
          filter(site10group == input$cancer.type,
                 sex == input$sex) %>%
          slice_min(abs(time - 5), n = 1) %>%
          pull(survival)
      })
      
      surv_km <- reactive({
        survival_data %>%
          filter(sex == input$sex,
                 site10group == input$cancer.type)
      })
      
      surv_bottomleft <- reactive({
        survival_data %>%
          group_by(sex, site10group) %>%
          slice_min(abs(time - 5), n = 1) %>%
          filter(sex == input$sex) %>%
          arrange(survival) %>%
          mutate(site10group = factor(site10group, levels = site10group))
      })
      
      surv_bottomright <- reactive({
        survival_age %>%
          filter(site10group == input$cancer.type,
                 sex == input$sex)
      })
      
      surv_timeperiod <- reactive({
        survival_time %>%
          filter(sex == input$sex)
      })
      
      if(id == "Diagnosis"){
        
        data_topleft <- reactive({
          inc_annual_counts %>%
            filter(year == max(year),
                   cancer.type == all_cancers_name,
                   measure == input$measure) %>%
            group_by(year, sex) %>%
            summarise(obs = sum(obs), .groups = 'drop')
        })
        
        lifetime_risk <- reactive({
          # browser()
          tmp <- inc_annual_counts %>%
            filter(year == most_recent_year,
                   cancer.type == input$cancer.type,
                   sex == input$sex,
                   measure == "ltr") %>%
            pull(obs)
          
          if(is_empty(tmp)){return(0)}else{return(tmp)}
        })
        
        data_topright <- reactive({
          inc_annual_counts %>%
            filter(cancer.type == input$cancer.type,
                   measure == input$measure)
          # %>%
          #   pivot_wider(names_from = 'sex', values_from = 'obs')
        })
        
        # DELETE IF AVERAGES IS OKAY
        # data_bottomleft <- reactive({
        #   inc_annual_counts %>%
        #     filter(sex == input$sex,
        #            measure == "Counts",
        ### TO UDPATE ----
        #            cancer.type != all_cancers_name,
        #            year == most_recent_year) %>%
        #     slice_max(order_by = obs,
        #               n = 5)
        # })
        
        data_bottomleft <- reactive({
          if (input$sex == 3) {
            top5_inc %>%
              filter(measure == input$measure)
          } else {
            inc_averages %>%
              filter(cancer.type != "All cancers",
                     measure == input$measure,
                     sex == input$sex) %>%
              slice_max(order_by = obs, n = 5)
          }
        })
        
        data_bottomright <- reactive({
          inc_counts %>%
            filter(sex == input$sex,
                   cancer.type == input$cancer.type,
                   measure == input$measure) %>%
            group_by(year) %>%
            complete(age.grp_string = age_ranges) %>%
            ungroup() %>%
            mutate("age.grp_string" =
                     factor(age.grp_string,
                            # to update
                            levels = sort(age_ranges)
                     )
            ) %>%
            ungroup()
          
        })
        
      }
      else if (id == "Deaths"){
        
        data_topleft <- reactive({
          mrt_annual_counts %>%
            filter(year == max(year),
                   cancer.type == all_cancers_name,
                   measure == input$measure) %>%
            group_by(year, sex) %>%
            summarise(obs = sum(obs), .groups = 'drop')
        })
        
        lifetime_risk <- reactive({
          tmp <- mrt_annual_counts %>%
            filter(year == most_recent_year,
                   cancer.type == input$cancer.type,
                   sex == input$sex,
                   measure == "ltr") %>%
            pull(obs)
          
          if(is_empty(tmp)){return(0)}else{return(tmp)}
        })
        
        data_topright <- reactive({
          mrt_annual_counts %>%
            filter(measure == input$measure,
                   cancer.type == input$cancer.type)
          # %>%
          #   pivot_wider(names_from = 'sex', values_from = 'obs')
        })
        
        # DELETE IF AVERAGES IS OKAY
        # data_bottomleft <- reactive({
        #   mrt_annual_counts %>%
        #     filter(sex == input$sex,
        #            measure == "Counts",
        ### TO UDPATE ----
        #            cancer.type != all_cancers_name,
        #            year == most_recent_year) %>%
        #     slice_max(order_by = obs,
        #               n = 5)
        # })
        
        data_bottomleft <- reactive({
          if (input$sex == 3) {
            top5_mrt %>%
              filter(measure == input$measure)
          } else {
            mrt_averages %>%
              filter(cancer.type != "All cancers",
                     measure == input$measure,
                     sex == input$sex) %>%
              slice_max(order_by = obs, n = 5)
          }
        })
        
        data_bottomright <- reactive({
          mrt_counts %>%
            filter(sex == input$sex,
                   cancer.type == input$cancer.type,
                   measure == input$measure) %>%
            group_by(year) %>%
            complete(age.grp_string = age_ranges) %>%
            ungroup() %>%
            mutate("age.grp_string" =
                     factor(age.grp_string,
                            # to update
                            levels = sort(age_ranges)
                     )
            ) %>%
            ungroup()
          
        })
      }
      
      time_hovertemplate <- reactive({
        paste0(
          if_else(input$measure == "Counts", paste0("%{y:,.1f}"), paste0("%{y:,.1f}")),
          if_else(id == "Diagnosis", " diagnoses", " deaths"),
          if_else(input$measure == "Counts", "", " per 100,000 pop.")
        )
      })
      
      # ---------------------------------------------------------------------- #
      
      ## Outputs ----
      
      ### Generate Report ----
      # output$report <- downloadHandler(
      #   filename = paste(input$cancer.type, "-report.html"),
      #   content = function(file) {
      #     tempReport <- file.path(tempdir(), "CaRDO report template.Rmd")
      #
      #     file.copy("CaRDO report template.Rmd", tempReport, overwrite = TRUE)
      #
      #     params <- list(cancer = input$cancer.type,
      #                    location = location_name,
      #                    year = most_recent_year,
      #                    incidence = inc_annual_counts,
      #                    mortality = if (no_mrt) {NA} else {mrt_annual_counts}
      #                    #survival = something
      #     )
      #
      #     rmarkdown::render(
      #       tempReport, output_file = file,
      #       params = params,
      #       envir = new.env(parent = globalenv())
      #     )
      #   }
      # )
      
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
          "<h3>Male cancer", tolower(counts_rates_labs()), "</h3>"
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
          "<h3>Female cancer", tolower(counts_rates_labs()), "</h3>"
        )
      })
      
      output$ltr_vis <- renderUI({
        tags$svg(class = "ltr-matrix",
                 width = "100%",
                 height = "100%",
                 viewBox = "0 0 7 100",
                 preserveAspectRatio = "XMidYMid meet",
                 lapply(1:10, function(i) {
                   color <- ifelse(i <= round(lifetime_risk() * 10),
                                   if(id == "Diagnosis") "#1C54A8" else "#8E3E39",
                                   "#E6E6E6")
                   stroke <- ifelse(i <= round(lifetime_risk() * 10),
                                    if(id == "Diagnosis") "#1C54A8" else "#8E3E39",
                                    "none")
                   width <- ifelse(i <= round(lifetime_risk() * 10), 2, 0)
                   
                   tags$circle(
                     class = "data-circle",
                     `data-info` = "Each circle represents 1 in 10 persons",
                     cx = "50%",
                     cy = (10 - i) * 10 + 5,
                     r = 2,
                     fill = color,
                     stroke = stroke,
                     `stoke-width` = width
                   )
                 })
        )
      })
      
      output$ltr_text <- renderUI({
        
        ltr_stat <- if (lifetime_risk() * 10 < 1 & lifetime_risk() * 10 != 0) {"< 1"} else {round(lifetime_risk() * 10)}
        
        stat_text <- if(lifetime_risk() == 0){
          "<span>No <b>"
        }else{
          paste0(
            "<span>Approximately 1 in <b>",round(10/(10 * lifetime_risk()), 1))
        }
        
        div(
          HTML(paste(span(class = "ltr-stat", ltr_stat), " out of 10")),
          div(
            class = "ltr-info",
            HTML(paste(
              stat_text,
              if (input$sex == 3) {"persons"} else {if (input$sex == 1) {"males"} else {"females"}},
              "</b>are expected to be diagnosed with <b>",
              tolower(input$cancer.type),
              "</b> by age 85</span>"
            )
            )
          )
        )
      })
      
      # ---------------------------------------------------------------------- #
      ### TR-Time Outputs ----
      title_topright <- reactive({
        heading_title <- if(input$measure == "Counts") {
          if(id == "Diagnosis") "Number of diagnoses" else "Number of deaths"
        } else {
          if(id == "Diagnosis") "Diagnosis rates" else "Death rates"
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
        
        heading_bracket <- if(input$measure == "Counts") "" else " (Age Standarised)"
        
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
        
        sufficient_data_cond <- (data_topright() %>%
                                   filter(sex == input$sex) %>%
                                   nrow) == 0
        
        validate(
          need(
            !sufficient_data_cond,
            message = "Data unavailable"
          )
        )
        
        categories <- c("1", "2", "3")
        plot_colour <- if(id == "Diagnosis") "#1C54A8" else "#8E3E39"
        
        line_styles <- sapply(categories, function(cat) {
          if (cat == input$sex) {
            list(color = "#808080")
          } else {
            list(color = "#DBDBDB", dash = "dash")
          }
        }, simplify = FALSE)
        
        marker_styles <- sapply(categories, function(cat) {
          if (cat == input$sex) {
            list(
              size = 9,
              color = plot_colour
            )
          } else {
            list(opacity = 0)
          }
        })
        
        hovertoggle <- sapply(categories, function(cat) {
          if (cat == input$sex) {"all"} else {"skip"}
        })
        
        legendtoggle <- sapply(categories, function(cat) {
          if (cat == input$sex) {TRUE} else {FALSE}
        })
        
        plot <- plot_ly(
          hovertemplate = time_hovertemplate()
        ) %>%
          config(
            modeBarButtonsToRemove = plotly_btns_rm,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = title_topright()
            ),
            # displaylogo = FALSE,
            # displayModeBar = FALSE,
            scrollZoom = FALSE
          ) %>%
          layout(
            hovermode = "x unified",
            legend = list(
              itemclick = FALSE,
              itemdoubleclick = FALSE
            ),
            xaxis = list(
              range = list(earliest_year-1, most_recent_year+1),
              fixedrange = TRUE,
              title = "Year",
              linewidth = 2,
              linecolor = "#000000",
              tickcolor = "#000000",
              ticks = "outside",
              tickvals = seq(earliest_year, most_recent_year, by = 2),
              zeroline = TRUE
            ),
            yaxis = list(
              fixedrange = TRUE,
              title = counts_rates_labs(),
              rangemode = 'tozero',
              zeroline = FALSE
              # ,
              # linewidth = 1
              #linecolor = "#111111"
            ),
            # margin = list(t = 0, b = 0, l = 15, r = 0),
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
        
        # browser()
        
        for(sex_num in unique(data_topright()$sex)){
          plot <- plot %>%
            add_trace(
              data = data_topright() %>% filter(sex == sex_num),
              x = ~year,
              y = ~obs_trend,
              name = paste(sex_name[[sex_num]], "trends"),
              type = "scatter",
              mode = "lines",
              line = line_styles[[sex_num]],
              showlegend = FALSE
            ) %>%
            add_trace(
              data = data_topright() %>% filter(sex == sex_num),
              x = ~year,
              y = ~obs,
              name = sex_name[[sex_num]],
              type = "scatter",
              mode = "markers",
              #line = line_styles[[sex_num]]
              marker = marker_styles[[sex_num]]
            )
        }
        
        plot
        # %>%
        #   add_trace(
        #     y = ~`3`,
        #     name = "Persons",
        #     type = "scatter",
        #     mode = "lines", #add +lines for trends
        #     line = line_styles[[3]]
        #     #marker = marker_styles[[3]]
        #   ) %>%
        #   add_trace(
        #     y = ~`1`,
        #     name = "Male",
        #     type = "scatter",
        #     mode = "lines", #add +lines for trends
        #     line = line_styles[[1]]
        #     #marker = marker_styles[[1]]
        #   ) %>%
        #   add_trace(
        #     y = ~`2`,
        #     name = "Female",
        #     type = "scatter",
        #     mode = "lines", #add +lines for trends
        #     line = line_styles[[2]]
        #     #marker =marker_styles[[2]]
        #   )
        
      })
      
      dwnldButtonServer(id = "topright_dwnld",
                        file_name = title_topright,
                        graph_df = data_topright)
      
      # ---------------------------------------------------------------------- #
      
      ### BL-Cancer Outputs ----
      title_bottomleft <- reactive({
        heading_title <- if(id == "Diagnosis") "diagnoses" else "deaths"
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
        
        heading_bracket <- if(input$measure == "Counts") {
          paste0("(Counts, 5 year average ", most_recent_year-4, "-", most_recent_year, ")")
        } else {
          paste0("(5 years ", most_recent_year-4, "-", most_recent_year, ", Age Standarised)")
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
        
        plot_colour <- if(id == "Diagnosis") "#335C98" else "#8E3E39"
        
        cancer_axis_limit <- if(input$measure == "Counts") {counts_limit} else {rates_limit}
        
        data_bl <- data_bottomleft()
        
        plot_ly(data = data_bl)%>%
          add_bars(
            x = ~obs,
            y = ~reorder(cancer.type, obs),
            type = 'bar',
            width = 0.5,
            marker = list(#line = list(color = "#333333", width = 0.5),
              color = plot_colour),
            hovertemplate = paste0(
              "%{y}: ", if_else(input$measure == "Counts", paste0("%{x:,}"), paste0("%{x:,.1f}")),
              "<extra></extra>"
            )
          ) %>%
          config(
            modeBarButtonsToRemove = plotly_btns_rm,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = title_bottomleft()
            ),
            # displaylogo = FALSE,
            # displayModeBar = FALSE,
            scrollZoom = FALSE
          ) %>%
          layout(
            xaxis = list(
              title = counts_rates_labs(),
              range = list(0, cancer_axis_limit),
              fixedrange = TRUE,
              #linewidth = 1,
              #linecolor = "#111111",
              #tickcolor = "#111111",
              #ticks = "outside",
              zeroline = FALSE
            ),
            yaxis = list(
              title = "",
              fixedrange = TRUE,
              rangemode = 'tozero'
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
        heading_title <- if(input$measure == "Counts") {
          if(id == "Diagnosis") "Number of diagnoses" else "Number of deaths"
        } else {
          if(id == "Diagnosis") "Diagnosis rates" else "Death rates"
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
        
        heading_bracket <- if(input$measure == "Counts") "" else " (Age Standarised)"
        
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
        # paste0(
        #   id, " by age group, for ", tolower(input$cancer.type), " in ", tolower(sex_name[[input$sex]]), " (", location_name, ")"
        # )
      })
      
      output$bottomright <- renderPlotly({
        
        sufficient_data_cond <- sum(is.na(data_bottomright()$obs)) == nrow(data_bottomright())
        
        validate(
          need(
            !sufficient_data_cond,
            message = "Data unavailable"
          )
        )
        
        age_colours <- c("#FF9F1C", "#7DDE9D", "#3A8592", "#042A2B")
        age_groups <- unique(data_bottomright()$age.grp_string) %>%
          sort
        
        plot <- plot_ly() %>%
          layout(
            xaxis = list(
              title = "Year",
              range = list(earliest_year-1, most_recent_year+1),
              fixedrange = TRUE,
              linewidth = 2,
              linecolor = "#000000",
              tickcolor = "#000000",
              ticks = "outside",
              tickvals = seq(earliest_year, most_recent_year, by = 5)
            ),
            yaxis = list(
              rangemode = "tozero",
              zeroline = FALSE,
              title = counts_rates_labs(),
              fixedrange = TRUE
            ),
            legend = list(
              title = list(text = "Age group (years)"),
              #itemclick = "toggleothers",
              #itemdoubleclick = "toggle"
              y=80,x=0,
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
            # displaylogo = FALSE,
            # displayModeBar = FALSE,
            scrollZoom = FALSE
          )
        
        
        for (i in rev(seq_along(age_groups))) {
          
          # browser()
          selected_colour <- age_colours
          
          filt_df <- data_bottomright() %>%
            # cbind(data.frame("tooltip" = tooltip)) %>%
            filter(age.grp_string == age_groups[i]) %>%
            # mutate("tooltip_trend" = paste("Trend: ",trend_private_string)) %>%
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
              hovertemplate = time_hovertemplate()
            )
        }
        
        plot
      })
      
      dwnldButtonServer(id = "bottomright_dwnld",
                        file_name = title_bottomright,
                        graph_df = data_bottomright)
      
      
      ### Survival-Summary Outputs ----
      output$rsurv_text <- renderUI({
        
        survival_stat <- round(fiveyear_survival() * 100, 1)
        
        if (length(survival_stat) != 0) {
          
          div(
            span(
              class = "survival-stat",
              paste0(survival_stat, "%")
            ),
            div(
              class = "rsurv-info",
              span(if(input$sex == 3) {"Persons"} else {if(input$sex == 1) {"Male"} else {"Female"}}),
              span(input$cancer.type)
            )
          )
          
        } else {
          
          div(
            class = "rsurv-info",
            h2("There is no data for this selection"),
            HTML(paste("Try changing one of the options above"))
          )
          
        }
        
      })
      
      output$rsurv_high <- renderUI({
        
        top3 <- survival_data %>%
          filter(site10group != "All cancers", sex == 3) %>%
          group_by(site10group) %>%
          slice_min(abs(time - 5), n = 1) %>%
          ungroup() %>%
          slice_max(survival, n = 3) %>%
          select(survival, site10group)
        
        div(
          style = "display: flex; flex-direction: column; gap: 1rem;",
          div(
            class = "individual-rank",
            style = paste0("background: linear-gradient(to right, #FFDE56 ",
                           round(top3$survival[1] * 100, 0), "%, transparent ",
                           round(top3$survival[1] * 100, 0), "%)"),
            span(paste(top3$site10group[1])),
            span(paste(round(top3$survival[1] * 100, 1), "%"))
          ),
          div(
            class = "individual-rank",
            style = paste0("background: linear-gradient(to right, #FFDE56 ",
                           round(top3$survival[2] * 100, 0), "%, transparent ",
                           round(top3$survival[2] * 100, 0), "%)"),
            span(paste(top3$site10group[2])),
            span(paste(round(top3$survival[2] * 100, 1), "%"))
          ),
          div(
            class = "individual-rank",
            style = paste0("background: linear-gradient(to right, #FFDE56 ",
                           round(top3$survival[3] * 100, 0), "%, transparent ",
                           round(top3$survival[3] * 100, 0), "%)"),
            span(paste(top3$site10group[3])),
            span(paste(round(top3$survival[3] * 100, 1), "%"))
          )
        )
        
      })
      
      output$rsurv_low <- renderUI({
        
        bottom3 <- survival_data %>%
          filter(site10group != "All cancers", sex == 3) %>%
          group_by(site10group) %>%
          slice_min(abs(time - 5), n = 1) %>%
          ungroup() %>%
          slice_min(survival, n = 3) %>%
          select(survival, site10group)
        
        div(
          style = "display: flex; flex-direction: column; gap: 1rem;",
          div(
            class = "individual-rank",
            style = paste0("background: linear-gradient(to right, #FFDE56 ",
                           round(bottom3$survival[1] * 100, 0), "%, transparent ",
                           round(bottom3$survival[1] * 100, 0), "%)"),
            span(paste(bottom3$site10group[1])),
            span(paste(round(bottom3$survival[1] * 100, 1), "%"))
          ),
          div(
            class = "individual-rank",
            style = paste0("background: linear-gradient(to right, #FFDE56 ",
                           round(bottom3$survival[2] * 100, 0), "%, transparent ",
                           round(bottom3$survival[2] * 100, 0), "%)"),
            span(paste(bottom3$site10group[2])),
            span(paste(round(bottom3$survival[2] * 100, 1), "%"))
          ),
          div(
            class = "individual-rank",
            style = paste0("background: linear-gradient(to right, #FFDE56 ",
                           round(bottom3$survival[3] * 100, 0), "%, transparent ",
                           round(bottom3$survival[3] * 100, 0), "%)"),
            span(paste(bottom3$site10group[3])),
            span(paste(round(bottom3$survival[3] * 100, 1), "%"))
          )
        )
        
      })
      
      output$rsurv_vis <- renderUI({
        
        tags$svg(class = "rsurv-matrix",
                 width = "100%",
                 height = "100%",
                 viewBox = "2 5 55 20",
                 preserveAspectRatio = "xMinYMid meet",
                 lapply(1:2, function(row) {
                   lapply(1:5, function(col) {
                     index <- (row - 1) * 5 + col
                     color <- ifelse(index > 10 - (fiveyear_survival() * 10), "#FFDE56", "#E6E6E6")
                     stroke <- ifelse(index > 10 - (fiveyear_survival() * 10), "#FFDE56", "none")
                     width <- ifelse(index > 10 - (fiveyear_survival() * 10), 2, 0)
                     
                     tags$circle(
                       class = "data-circle",
                       `data-info` = "Whatever",
                       cx = col * 10,
                       cy = row * 10,
                       r = 1,
                       fill = color,
                       stroke = stroke,
                       `stroke-width` = width
                     )
                   })
                 })
        )
      })
      
      ### Survival-Central Outputs ----
      
      #### Kaplan Meier Curve ----
      surv_title_km <- reactive({
        heading_title <- "Kaplan-Meier survival curve"
        heading_cancer <- tolower(input$cancer.type)
        heading_sex <- if(input$sex == 3) {
          " "
        } else {
          if(input$sex == 1) " for males" else " for females"
        }
        
        return (
          paste0(heading_title, " for ", heading_cancer, heading_sex, " (", location_name, ")")
        )
      })
      
      output$surv_title_km <- renderUI({
        
        heading_bracket <- paste0("(", most_recent_year-4, "-", most_recent_year, ")")
        
        div(
          class = "custom-heading",
          span(
            class = "heading-bold",
            surv_title_km()
          ),
          span(
            class = "heading-bracket",
            span(
              class = 'dl_pos',
              dwnldButtonUI(id = ns("survkm_dwnld")) %>%
                tooltip("Download plot data",
                        placement = "bottom",
                        options = list(customClass = "info_tooltip")
                )
            ),
            paste0(heading_bracket)
          )
        )
      })
      
      output$surv_km <- renderPlotly({
        
        closest <- surv_km()
        # 
        # closest_index <- which.min(abs(closest$time - 5))
        # closest_x <- closest$time[closest_index]
        # closest_y <- closest$survival[closest_index]
        
        sufficient_data_cond <- sum(is.na(closest$survival)) == nrow(closest)
        
        validate(
          need(
            !sufficient_data_cond,
            message = "Data unavailable"
          )
        )
        
        plot <- plot_ly(
          hovertemplate = "%{y:.1%}"
        ) %>%
          add_trace(
            data = closest,
            x = ~time,
            y = ~up_ci,
            type = 'scatter',
            mode = 'lines',
            fillcolor = '#FFDE5633',
            line = list(color = 'transparent'),
            showlegend = FALSE,
            name = "Upper"
          ) %>%
          add_trace(
            data = closest,
            x = ~time,
            y = ~lo_ci,
            type = 'scatter',
            mode = 'lines',
            fill = 'tonexty',
            fillcolor = '#FFDE5633',
            line = list(color = 'transparent'),
            showlegend = FALSE,
            name = "Lower"
          ) %>%
          add_trace(
            data = closest,
            x = ~time,
            y = ~survival,
            type = 'scatter',
            mode = 'lines',
            line = list(
              color = 'black',
              width = 2
            ),
            showlegend = FALSE,
            name = "Estimate"
          ) %>%
          layout(
            hovermode = "x unified",
            legend = list(
              itemclick = FALSE,
              itemdoubleclick = FALSE
            ),
            xaxis = list(
              fixedrange = TRUE,
              title = "Time",
              linewidth = 2,
              linecolor = "#000000",
              tickcolor = "#000000",
              ticks = "outside",
              zeroline = FALSE
            ),
            yaxis = list(
              fixedrange = TRUE,
              title = "Survival Rate",
              rangemode = 'tozero',
              zeroline = FALSE
            ),
            margin = list(t = 0, b = 0, l = 0, r = 0, pad = 15),
            showlegend = TRUE,
            paper_bgcolor = 'transparent',
            plot_bgcolor = 'transparent',
            font = list(color = '#111111'),
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
              filename = "Kaplan-Meier Survival Curve"
            ),
            scrollZoom = FALSE
          )
        
      })
      
      dwnldButtonServer(id = "survkm_dwnld",
                        file_name = surv_title_km,
                        graph_df = surv_km)
      
      #### Survival by Age Group ----
      surv_title_agegroup <- reactive({
        heading_title <- "Survival rates of broad age groups"
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
      
      output$surv_title_agegroup <- renderUI({
        
        heading_bracket <- paste0("(", most_recent_year-4, "-", most_recent_year, ")")
        
        div(
          class = "custom-heading",
          span(
            class = "heading-bold",
            surv_title_agegroup()
          ),
          span(
            class = "heading-bracket",
            span(
              class = 'dl_pos',
              dwnldButtonUI(id = ns("surv_agegroups_dwnld")) %>%
                tooltip("Download plot data",
                        placement = "bottom",
                        options = list(customClass = "info_tooltip")
                )
            ),
            paste0(heading_bracket)
          )
        )
      })
      
      output$surv_agegroup <- renderPlotly({
        
        sufficient_data_cond <- sum(is.na(surv_bottomright()$survival)) == nrow(surv_bottomright())
        
        validate(
          need(
            !sufficient_data_cond,
            message = "Data unavailable"
          )
        )
        
        age_colours <- c("#FF9F1C", "#7DDE9D", "#3A8592", "#042A2B")
        age_groups <- unique(surv_bottomright()$bragegrp) %>%
          sort
        
        plot <- plot_ly() %>%
          layout(
            hovermode = "x unified",
            legend = list(
              itemclick = FALSE,
              itemdoubleclick = FALSE
            ),
            xaxis = list(
              title = "Time",
              linewidth = 2,
              linecolor = "#000000",
              tickcolor = "#000000",
              ticks = "outside",
              zeroline = TRUE
            ),
            yaxis = list(
              title = "Survival Rate",
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
          ) %>%
          config(
            modeBarButtonsToRemove = plotly_btns_rm,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = "bragegroup"
            ),
            # displaylogo = FALSE,
            # displayModeBar = FALSE,
            scrollZoom = FALSE
          )
        
        for (i in seq_along(age_groups)) {
          
          selected_colour <- age_colours
          
          plot <- plot %>%
            add_trace(
              data = surv_bottomright() %>% filter(bragegrp == i),
              x = ~time,
              y = ~survival,
              name = paste0(age_groups[i], " age group"),
              mode = 'lines',
              type = 'scatter',
              showlegend = TRUE,
              line = list(color = age_colours[i],
                          width = 3)
            )
        }
        
        plot
      })
      
      dwnldButtonServer(id = "surv_agegroups_dwnld",
                        file_name = surv_title_agegroup,
                        graph_df = surv_bottomright)
      
      ### Survival-Right Outputs ----
      output$surv_timeperiod <- renderPlotly({
        
        arrows <- surv_timeperiod() %>%
          filter(riskperiod == "2018-2022")
        
        timeperiod_colours <- c(
          "2013-2017" = "#D6D6D6",
          "2018-2022" = "#FFDE56"
        )
        
        plot_ly(
          data = surv_timeperiod(),
          x = ~relsurv,
          y = ~reorder(site10group, relsurv),
          color = ~riskperiod,
          colors = timeperiod_colours,
          type = 'scatter',
          mode = 'markers',
          size = 8
        ) %>%
          add_annotations(
            data = arrows,
            xref = "x", yref = "y",
            axref = "x", ayref = "y",
            text = "",
            showarrow = TRUE,
            arrowcolor = ifelse(arrows$trend == "Increasing",
                                yes = "#333333",
                                no = "#f44336"),
            ax = ~arrow.x.start,
            x = ~arrow.x.end,
            ay = ~site10group,
            y = ~site10group
          ) %>%
          config(
            modeBarButtonsToRemove = plotly_btns_rm,
            displaylogo = FALSE
          ) %>%
          layout(
            hovermode = "y unified",
            legend = list(
              title = list(text = "Risk period"),
              y=80,x=0,
              orientation = 'h',
              itemclick = FALSE,
              itemdoubleclick = FALSE
            ),
            xaxis = list(
              fixedrange = TRUE,
              title = "Five year relative survival",
              ticksuffix = "%",
              zeroline = FALSE,
              showline = TRUE,
              linewidth = 1,
              linecolor = "#111111",
              tickcolor = "#111111",
              ticks = "outside",
              showgrid = FALSE
            ),
            yaxis = list(
              fixedrange = TRUE,
              title = "",
              showline = FALSE,
              showgrid = FALSE
            ),
            showlegend = TRUE,
            paper_bgcolor = 'transparent',
            plot_bgcolor = 'transparent',
            # hoverlabel = plotly_tooltip_style,
            font = list(color = '#111111'),
            modebar = list(
              bgcolor = 'rgba(0,0,0,0)',
              color = 'rgba(0,0,0,1)',
              activecolor = '#111111'
            ),
            margin = list(b = 60)
          )
          
        
      })
      
    }
  )
}

