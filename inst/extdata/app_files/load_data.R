# Setup, Data and Parameters ----
# ---------------------------------------------------------------------------- #

age_choices <- list(
  "All ages (0-85+)" = "all_ages",
  "Adults (15-85+)" = "adults_who",
  "Adults (18-85+)" = "adults_legal"
)

inc_annual_counts <- read.csv("Data/data_inc.annual.csv")
inc_counts        <- read.csv("Data/data_inc.age.csv")
inc_averages      <- read.csv("Data/data_inc.cancer.csv")
supplied_params   <- readRDS("Data/supplied_params.RDS")

all_cancers_name  <- supplied_params[["All cancers"]]         # <-- Don't Change This
dashboard_title   <- supplied_params[["Dashboard title"]]     # <-- You can change this
location_name     <- supplied_params[["Dashboard catchment"]]
to_aggregate      <- supplied_params[["aggregate_option"]]
standard_pop_name <- supplied_params[["Standard population"]]
agerange_choice   <- supplied_params[["Age range"]]

age_scope <- names(age_choices)[age_choices == agerange_choice]

# Mortality Data Logic
# ------------------------------------ #
no_mrt <<- FALSE

# Read in mortality data or suppress errors if it's missing
mrt_annual_counts <- tryCatch(
  { suppressWarnings(read.csv("Data/data_mrt.annual.csv")) },
  error = function(e) { no_mrt <<- TRUE; return(NULL) }
)

mrt_counts <- tryCatch(
  { suppressWarnings(read.csv("Data/data_mrt.age.csv")) },
  error = function(e) { no_mrt <<- TRUE; return(NULL) }
)

mrt_averages <- tryCatch(
  { suppressWarnings(read.csv("Data/data_mrt.cancer.csv")) },
  error = function(e) { no_mrt <<- TRUE; return(NULL) }
)
# ------------------------------------ #

# Lifetime Risk Data Logic
# ------------------------------------ #
no_ltr <<- FALSE

# Read in lifetime risk data or suppress errors if it's missing
inc_ltr <- tryCatch(
  { suppressWarnings(read.csv("Data/data_inc.lifetime.csv")) },
  error = function(e) { no_ltr <<- TRUE; return(NULL) }
)

if (!no_mrt) {
  mrt_ltr <- tryCatch(
    { suppressWarnings(read.csv("Data/data_mrt.lifetime.csv")) },
    error = function(e) { no_ltr <<- TRUE; return(NULL) }
  )
}
# ------------------------------------ #

# Plotly options to be disabled
plotly_btns_rm <- c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
                    "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
                    "zoom3d", "pan3d", "resetCameraDefault3d", "resetCameraLastSave3d",
                    "hoverClosest3d", "orbitRotation", "tableRotation", "zoomInGeo",
                    "zoomOutGeo", "resetGeo", "hoverClosestGeo",
                    "sendDataToCloud", "hoverClosestGl2d", "hoverClosestPie",
                    "toggleHover", "resetViews", "toggleSpikelines", "resetViewMapbox")

# ---------------------------------------------------------------------------- #






# Menu lists, labels and axes ----
# ---------------------------------------------------------------------------- #

# Input lists (cancer menu and other choices)
cancer_choices_inc <- unique(inc_annual_counts$cancer.type)
cancer_choices_mrt <- unique(mrt_annual_counts$cancer.type)
sex_choices        <- unique(inc_annual_counts$sex)
measure_choices    <- unique(inc_annual_counts$measure)


# Labels (for headings and other elements)
age_ranges           <- unique(inc_counts$age.label)

most_recent_year_inc <- max(inc_annual_counts$year)
most_recent_year_mrt <- if (no_mrt) NA else max(mrt_annual_counts$year)

most_recent_years <- list(
  Diagnoses = most_recent_year_inc,
  Deaths    = most_recent_year_mrt
)

sex_name <- list("1" = "Males",
                 "2" = "Females",
                 "3" = "Persons")

# Plot axes limits
counts_limit <- inc_averages %>%
  filter(cancer.type != all_cancers_name,
         measure     == "counts") %>%
  pull(obs) %>%
  max(., na.rm = TRUE)

rates_limit <- if (length(measure_choices) != 1) {
  inc_averages %>%
    filter(cancer.type != all_cancers_name,
           measure     == "rates") %>%
    pull(obs) %>%
    max(., na.rm = TRUE)
} else {
  NULL
}






# Cancer lists (sex-specific and highest counts) ----
# ---------------------------------------------------------------------------- #

# Defining a list of sex-specific cancers
sex_specific_cancers <- inc_annual_counts %>%
  group_by(cancer.type) %>%
  summarise(
    "sex_specific" = if_else(length(unique(sex)) == 3, true = 0, false = 1),
    .groups = 'drop'
  ) %>%
  filter(sex_specific == 1) %>%
  pull(cancer.type)

# Defining a list of cancers with the highest counts
top_5_cancers_inc <- inc_averages %>%
  filter(
    cancer.type != all_cancers_name,
    measure == "counts"
  ) %>%
  arrange(desc(obs)) %>%
  pull(cancer.type) %>%
  unique() %>%
  head(5)

# Handling of sex-specific cancers in top 5 counts/rates
top5_inc <- inc_averages %>%
  filter(
    cancer.type %in% top_5_cancers_inc,
    obs != 0,
    !(!(cancer.type %in% sex_specific_cancers) & sex != 3)
  )

# Same logic but for mortality data if it's present
if (!no_mrt) {
  
  sex_specific_cancers <- mrt_annual_counts %>%
    group_by(cancer.type) %>%
    summarise(
      "sex_specific" = if_else(length(unique(sex)) == 3, true = 0, false = 1),
      .groups = 'drop'
    ) %>%
    filter(sex_specific == 1) %>%
    pull(cancer.type)
  
  top_5_cancers_mrt <- mrt_averages %>%
    filter(
      cancer.type != all_cancers_name,
      measure == "counts"
    ) %>%
    arrange(desc(obs)) %>%
    pull(cancer.type) %>%
    unique() %>%
    head(5)
  
  top5_mrt <- mrt_averages %>%
    filter(
      cancer.type %in% top_5_cancers_mrt,
      obs != 0,
      !(!(cancer.type %in% sex_specific_cancers) & sex != 3)
    )
  
}

# ---------------------------------------------------------------------------- #






# Helper functions ----
# ---------------------------------------------------------------------------- #

## Download data button ----
# ------------------------------------ #
dwnldButtonUI <- function(id) {
  
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

dwnldButtonServer <- function(id, file_name, graph_df) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 output$dwnldBtn <- downloadHandler(
                   filename = function() { paste(file_name(), ".csv", sep = "") },
                   content  = function(file) {
                     write.csv(graph_df(), file, row.names = FALSE)
                   }
                 )
                 
               }
  )
  
}
# ------------------------------------ #

# ---------------------------------------------------------------------------- #
