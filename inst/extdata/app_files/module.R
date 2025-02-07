UI_module <- function(id){
  ns <- NS(id)

  layout_columns(
    col_widths = c(12,
                   6, 6,
                   6, 6),
    row_heights = c(1, 7, 7),

    div(
      class = "header-options",
      layout_columns(
        col_widths = c(4, 4, 2, 2),
        selectInput(inputId = ns("cancer.type"),
                    choices = if (id == "Diagnosis") {cancer_choices_inc} else {cancer_choices_mrt},
                    label = "Cancer type"),

        h1(id, style = "color: white !important;"),

        radioGroupButtons(inputId = ns("sex"),
                          # choices = sex_choices,
                          choices = c("Persons" = 3, "Males" = 1, "Females" = 2),
                          label = "Sex"),

        radioGroupButtons(inputId = ns("measure"),
                          choices = measure_choices,
                          label = "Measure")
      )
    ),

    card(
      div(
        class = "summary-container",
        div(
          class = "summary-title",
          div(class = "summary", h1(HTML("Summary")), span("For the year ", most_recent_year))
          #img(src = "daffodil.jpg", title = "Daffodil")
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
          )
        )
      )
    ),
    card(
      card_header(
        uiOutput(outputId = ns("title_topright"))
      ),
      card_body(
        plotlyOutput(outputId = ns("topright"))
      )
    ),
    card(
      card_header(
        uiOutput(outputId = ns("title_bottomleft"))
      ),
      card_body(
        plotlyOutput(outputId = ns("bottomleft"))
      )
    ),
    card(
      card_header(
        uiOutput(outputId = ns("title_bottomright"))
      ),
      card_body(
        plotlyOutput(outputId = ns("bottomright"))
      )
    )
  )
}

server_module <- function(id){
  moduleServer(
    id,
    function(input, output, session){

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
          inc_annual_counts %>%
            filter(year == most_recent_year,
                   cancer.type == input$cancer.type,
                   sex == input$sex,
                   measure == "ltr") %>%
            pull(obs)
        })

        data_topright <- reactive({
          inc_annual_counts %>%
            filter(cancer.type == input$cancer.type,
                   measure == input$measure) %>%
            pivot_wider(names_from = 'sex', values_from = 'obs')
        })

        # DELETE IF AVERAGES IS OKAY
        # data_bottomleft <- reactive({
        #   inc_annual_counts %>%
        #     filter(sex == input$sex,
        #            measure == "Counts",
        #            #### TO UDPATE #####
        #            cancer.type != all_cancers_name,
        #            year == most_recent_year) %>%
        #     slice_max(order_by = obs,
        #               n = 5)
        # })

        data_bottomleft <- reactive({
          inc_averages %>%
            filter(sex == input$sex,
                   measure == input$measure,
                   cancer.type != all_cancers_name) %>%
            slice_max(order_by = obs,
                      n = 5)
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
          mrt_annual_counts %>%
            filter(year == most_recent_year,
                   cancer.type == input$cancer.type,
                   sex == input$sex,
                   measure == "ltr") %>%
            pull(obs)
        })

        data_topright <- reactive({
          mrt_annual_counts %>%
            filter(measure == input$measure,
                   cancer.type == input$cancer.type) %>%
            pivot_wider(names_from = 'sex', values_from = 'obs')
        })

        # DELETE IF AVERAGES IS OKAY
        # data_bottomleft <- reactive({
        #   mrt_annual_counts %>%
        #     filter(sex == input$sex,
        #            measure == "Counts",
        #            #### TO UDPATE #####
        #            cancer.type != all_cancers_name,
        #            year == most_recent_year) %>%
        #     slice_max(order_by = obs,
        #               n = 5)
        # })

        data_bottomleft <- reactive({
          mrt_averages %>%
            filter(sex == input$sex,
                   measure == input$measure,
                   cancer.type != all_cancers_name) %>%
            slice_max(order_by = obs,
                      n = 5)
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
          if_else(input$measure == "Counts", paste0("%{y:,}"), paste0("%{y:,.2f}")),
          if_else(id == "Diagnosis", " diagnoses", " deaths"),
          if_else(input$measure == "Counts", "", " per 1 million pop.")
        )
      })

      # output$title_topleft <- renderUI({})
      #
      # output$topleft <- renderPlotly({})

      ##
      output$all_text <- renderUI({
        HTML(
          paste0(
            data_topleft() %>%
              filter(sex == 3) %>%
              mutate(obs = round(obs, 1)) %>%
              pull(obs) %>%
              format(big.mark = ",", scientific = FALSE)
          ),
          "<h2>All cancer", tolower(counts_rates_labs()), "</h2>"
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
          "<h2>Male cancer", tolower(counts_rates_labs()), "</h2>"
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
          "<h2>Female cancer", tolower(counts_rates_labs()), "</h2>"
        )
      })

      output$ltr_vis <- renderUI({
        tags$svg(class = "ltr-matrix",
                 width = "100%",
                 height = "100%",
                 viewBox = "0 0 7 100",
                 preserveAspectRatio = "XMidYMid meet",
                 lapply(1:10, function(i) {
                   color <- ifelse(i <= round(lifetime_risk() * 10), "#FFDE56", "grey")
                   tags$circle(cx = "50%", cy = (10 - i) * 10 + 5, r = 3, fill = color)
                 })
        )
      })

      output$ltr_text <- renderUI({
        div(
          HTML(paste(round(lifetime_risk() * 10), " out of 10")),
          div(
            class = "ltr-info",
            HTML(paste(
              "<span>Approximately 1 in <b>",
              round(10/(10 * lifetime_risk()), 2),
              if (input$sex == 3) {"persons"} else {if (input$sex == 1) {"males"} else {"females"}},
              "</b>are expected to be diagnosed with <b>",
              tolower(input$cancer.type),
              "</b> by age 85</span>"
            )
            )
          )
        )
      })

      ##


      output$title_topright <- renderUI({
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
        heading_bracket <- if(input$measure == "Counts") "" else " (Age Standarised per 100,000 people)"

        div(
          class = "custom-heading",
          span(
            class = "heading-bold",
            paste0(heading_title, " for ", heading_cancer, heading_sex, " (", location_name, ")")
          ),
          span(
            class = "heading-bracket",
            paste0(heading_bracket)
          )
        )
      })

      output$topright <- renderPlotly({

        categories <- c("1", "2", "3")
        plot_colour <- if(id == "Diagnosis") "#1C54A8" else "#A74A43"

        line_styles <- sapply(categories, function(cat) {
          if (cat == input$sex) {
            list(color = plot_colour)
          } else {
            list(color = "#D3D3D3", dash = "dot")
          }
        }, simplify = FALSE)

        marker_styles <- sapply(categories, function(cat) {
          if (cat == input$sex) {
            list(size = 6,
                 color = plot_colour,
                 line = list(color = "white"))
          } else {
            list(size = 6,
                 color = "grey")
          }
        })

        # browser()

        plot_ly(
          data = data_topright(),
          x = ~year,
          hovertemplate = time_hovertemplate()) %>%
          add_trace(
            y = ~`3`,
            name = "Persons",
            type = "scatter",
            mode = "lines", #add +lines for trends
            line = line_styles[[3]]
            #marker = marker_styles[[3]]
          ) %>%
          add_trace(
            y = ~`1`,
            name = "Male",
            type = "scatter",
            mode = "lines", #add +lines for trends
            line = line_styles[[1]]
            #marker = marker_styles[[1]]
          ) %>%
          add_trace(
            y = ~`2`,
            name = "Female",
            type = "scatter",
            mode = "lines", #add +lines for trends
            line = line_styles[[2]]
            #marker =marker_styles[[2]]
          ) %>%
          config(
            displaylogo = FALSE,
            displayModeBar = FALSE,
            scrollZoom = FALSE
          ) %>%
          layout(
            hovermode = "x unified",
            legend = list(
              itemclick = FALSE,
              itemdoubleclick = FALSE
            ),
            xaxis = list(
              fixedrange = TRUE,
              title = "",
              #linewidth = 1,
              #linecolor = "#111111",
              #tickcolor = "#111111",
              #ticks = "outside",
              zeroline = FALSE
            ),
            yaxis = list(
              fixedrange = TRUE,
              title = "", #counts_rates_labs(),
              rangemode = 'tozero',
              zeroline = FALSE
              # ,
              # linewidth = 1
              #linecolor = "#111111"
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
          )
      })


      output$title_bottomleft <- renderUI({
        heading_title <- if(id == "Diagnosis") "diagnoses" else "deaths"
        heading_sex <- if(input$sex == 3) {
          " "
        } else {
          if(input$sex == 1) " in males" else " in females"
        }
        heading_bracket <- if(input$measure == "Counts") "(Counts, 5 year average)" else " (5 years, age Standarised per 100,000 people)"

        div(
          class = "custom-heading",
          span(
            class = "heading-bold",
            paste0("The five most common cancers by ", heading_title, heading_sex, " (", location_name, ")")
          ),
          span(
            class = "heading-bracket",
            paste0(heading_bracket)
          )
        )
        # paste0(
        #   "Top 5 cancers by ", tolower(id), " (5 year average) in ", tolower(sex_name[[input$sex]]), " (", location_name, ")"
        # )
      })

      output$bottomleft <- renderPlotly({

        plot_colour <- if(id == "Diagnosis") "#335C98" else "#A74A43"

        plot_ly(data = data_bottomleft())%>%
          add_bars(
            x = ~obs,
            y = ~reorder(cancer.type, obs),
            type = 'bar',
            width = 0.5,
            marker = list(#line = list(color = "#333333", width = 0.5),
              color = plot_colour),
            hovertemplate = paste0(
              "%{y}: ", if_else(input$measure == "Counts", paste0("%{x:,}"), paste0("%{x:,.2f}")),
              "<extra></extra>"
            )
          ) %>%
          config(
            displaylogo = FALSE,
            displayModeBar = FALSE,
            scrollZoom = FALSE
          ) %>%
          layout(
            xaxis = list(
              title = "", #id,
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


      output$title_bottomright <- renderUI({
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
        heading_bracket <- if(input$measure == "Counts") "" else " (Age Standarised per 100,000 people)"

        div(
          class = "custom-heading",
          span(
            class = "heading-bold",
            paste0(heading_title, ", by age group, for ", heading_cancer, heading_sex, " (", location_name, ")")
          ),
          span(
            class = "heading-bracket",
            paste0(heading_bracket)
          )
        )
        # paste0(
        #   id, " by age group, for ", tolower(input$cancer.type), " in ", tolower(sex_name[[input$sex]]), " (", location_name, ")"
        # )
      })

      output$bottomright <- renderPlotly({

        age_colours <- c("#EB63CA", "#63EB84", "#EBC863", "#6386EB")
        age_groups <- unique(data_bottomright()$age.grp_string) %>%
          sort

        plot <- plot_ly() %>%
          layout(
            xaxis = list(
              title = "", #Year",
              fixedrange = TRUE,
              zeroline = FALSE,
              tickvals = list(1985,1990,1995,2000,2005,2010,2015,2020)
            ),
            yaxis = list(
              rangemode = "tozero",
              zeroline = FALSE,
              title = "", #counts_rates_labs(),
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
            margin = list(t = 0, b = 0, l = 0, r = 0, pad = 15)
          ) %>%
          config(
            displaylogo = FALSE,
            displayModeBar = FALSE,
            scrollZoom = FALSE
          )


        for (i in seq_along(age_groups)) {

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
              # text = ~tooltip,
              # hoverinfo = "text"
            )
        }

        plot
      })


    }
  )
}
