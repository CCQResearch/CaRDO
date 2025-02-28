# Let's first load our data
inc_annual_counts <- read.csv("Data/data_inc_annual.csv")
inc_counts <- read.csv("Data/data_inc_age.csv")
inc_averages <- read.csv("Data/data_inc_average.csv")
supplied_params <- readRDS("Data/supplied_params.RDS")
all_cancers_name <- supplied_params[["All cancers"]] # <-- Don't Change This
dashboard_title <- supplied_params[["Dashboard title"]] # <-- You can change this
location_name <- supplied_params[["Dashboard catchment"]]


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

counts_limit <- inc_counts %>%
  filter(cancer.type != "All cancers",
         sex == 3,
         measure == "Counts") %>%
  pull(obs) %>%
  max(., na.rm = TRUE)

rates_limit <- if(length(measure_choices) != 1){
  inc_annual_counts %>%
    filter(cancer.type != "All cancers",
           sex == 3,
           measure == "Rates") %>%
    pull(obs) %>%
    max(., na.rm = TRUE)
} else {
  NULL
}


plotly_btns_rm <- c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
                    "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
                    "zoom3d", "pan3d", "resetCameraDefault3d", "resetCameraLastSave3d",
                    "hoverClosest3d", "orbitRotation", "tableRotation", "zoomInGeo",
                    "zoomOutGeo", "resetGeo", "hoverClosestGeo",
                    "sendDataToCloud", "hoverClosestGl2d", "hoverClosestPie",
                    "toggleHover", "resetViews", "toggleSpikelines", "resetViewMapbox")
