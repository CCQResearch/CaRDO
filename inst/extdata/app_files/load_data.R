# Let's first load our data
inc_annual_counts <- read.csv("Data/data_inc_annual.csv")
inc_counts <- read.csv("Data/data_inc_age.csv")
inc_averages <- read.csv("Data/data_inc_average.csv")
supplied_params <- readRDS("Data/supplied_params.RDS")
all_cancers_name <- supplied_params[["All cancers"]] # <-- Don't Change This
dashboard_title <- supplied_params[["Dashboard title"]] # <-- You can change this
location_name <- supplied_params[["Dashboard catchment"]]
survival_data <- read.csv("Data/data_srv_1822.csv")
survival_age <- read.csv("Data/data_srv_age.csv")
survival_time <- read.csv("Data/data_srv_time.csv")

# Define a variable that says whether mortality files are present, initialise to TRUE
no_mrt <<- FALSE

# Code to suppress errors in the event that mortality is not present
mrt_annual_counts <- tryCatch(
  {
    suppressWarnings(read.csv("Data/data_mrt_annual.csv"))
  },
  error = function(e) {no_mrt <<- TRUE; return(NULL)}
)

mrt_counts <- tryCatch(
  {
    suppressWarnings(read.csv("Data/data_mrt_age.csv"))
  },
  error = function(e) {no_mrt <<- TRUE; return(NULL)}
)

mrt_averages <- tryCatch(
  {
    suppressWarnings(read.csv("Data/data_mrt_average.csv"))
  },
  error = function(e) {no_mrt <<- TRUE; return(NULL)}
)

# Global variables
cancer_choices_inc <- unique(inc_annual_counts$cancer.type)
cancer_choices_mrt <- unique(mrt_annual_counts$cancer.type)
sex_choices <- unique(inc_annual_counts$sex)
measure_choices <- unique(inc_annual_counts$measure) %>%
  setdiff('ltr') # avoid having ltr as a stat measure - remove to include for visualisation
most_recent_year <- max(inc_annual_counts$year)
earliest_year <- min(inc_annual_counts$year)
age_ranges <- unique(inc_counts$age.grp_string)

sex_name <- list("1" = "Males",
                 "2" = "Females",
                 "3" = "Persons")

lifetime_risk <- inc_annual_counts %>%
  filter(year == most_recent_year,
         cancer.type == all_cancers_name,
         sex == 3,
         measure == "ltr") %>%
  pull(obs)

counts_limit <- inc_averages %>%
  filter(cancer.type != all_cancers_name,
         #sex == 3,
         measure == "Counts") %>%
  pull(obs) %>%
  max(., na.rm = TRUE)

rates_limit <- if(length(measure_choices) != 1){
  inc_averages %>%
    filter(cancer.type != all_cancers_name,
           #sex == 3,
           measure == "Rates") %>%
    pull(obs) %>%
    max(., na.rm = TRUE)
} else {
  NULL
}



# Check for sex-specific cancers (then later, remove the persons options from them)
sex_specific_cancers <- inc_annual_counts %>%
  group_by(cancer.type) %>%
  summarise("sex_specific" = if_else(length(unique(sex)) == 3, true = 0, false = 1),
            .groups = 'drop') %>%
  filter(sex_specific == 1) %>%
  pull(cancer.type)

top_5_cancers_inc <- inc_averages %>%
  filter(measure == "Counts",
         cancer.type != "All cancers") %>%
  arrange(desc(obs)) %>%
  pull(cancer.type) %>%
  unique() %>%
  head(5)

top5_inc <- inc_averages %>%
  filter(cancer.type %in% top_5_cancers_inc,
         obs != 0,
         !(!(cancer.type %in% sex_specific_cancers) & sex != 3)
         )

# Handling of sex-specific cancers in top 5 counts/rates

if(!no_mrt){
  sex_specific_cancers <- mrt_annual_counts %>%
    group_by(cancer.type) %>%
    summarise("sex_specific" = if_else(length(unique(sex)) == 3, true = 0, false = 1),
              .groups = 'drop') %>%
    filter(sex_specific == 1) %>%
    pull(cancer.type)

  top_5_cancers_mrt <- mrt_averages %>%
    filter(measure == "Counts",
           cancer.type != "All cancers") %>%
    arrange(desc(obs)) %>%
    pull(cancer.type) %>%
    unique() %>%
    head(5)

  top5_mrt <- mrt_averages %>%
    filter(cancer.type %in% top_5_cancers_mrt,
           obs != 0,
           !(!(cancer.type %in% sex_specific_cancers) & sex != 3)
    )
}



plotly_btns_rm <- c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
                    "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
                    "zoom3d", "pan3d", "resetCameraDefault3d", "resetCameraLastSave3d",
                    "hoverClosest3d", "orbitRotation", "tableRotation", "zoomInGeo",
                    "zoomOutGeo", "resetGeo", "hoverClosestGeo",
                    "sendDataToCloud", "hoverClosestGl2d", "hoverClosestPie",
                    "toggleHover", "resetViews", "toggleSpikelines", "resetViewMapbox")


##### Auxiliary / Helper Functions #####
dwnldButtonUI <- function(id){
  ns <- NS(id)
  tagList(
    downloadButton(
      outputId = ns("dwnldBtn"),
      label = "",
      icon = icon(
        name = NULL,
        style="
               background: url('downArrow5.png');
               background-size: contain;
               background-position: center;
               background-repeat: no-repeat;
               height: 12px;
               width: 12px;
               display: block;
              "
      )
    )
  )
}

dwnldButtonServer <- function(id, file_name, graph_df){

  moduleServer(
    id,
    function(input, output, session){

      output$dwnldBtn <- downloadHandler(

        filename = function() {
          paste(file_name(), ".csv", sep = "")
        },

        content = function(file) {
          write.csv(graph_df(), file, row.names = FALSE)
        }
      )
    }
  )
}
