#' Standardises the year ranges across the supplied datasets
#' 
#' Establishes a ground truth year range and matches each dataset to the truth
#' and generates 5-year blocks to use for aggregation.
#' 
#' @param data_list List. Contains all the datasets that were supplied.
#' @param aggregate Logical. TRUE if the user wants to aggregate their data.
#'  If TRUE, then it will trim the year range again to make sure that there are
#'  only complete blocks.
#' 
#' @return List. A list of data frames and an end_year variable.
#' 
#' @import dplyr
#' @importFrom tidyr complete
#' @importFrom magrittr %>%

process_years <- function(data_list, aggregate_option = FALSE) {
  
  data_sets <- names(data_list)
  
  if (!("data_mrt" %in% data_sets)) { data_mrt <- NULL }
  if (!("data_pop" %in% data_sets)) { data_pop <- NULL }
  
  list2env(data_list, envir = environment())
  
  # Expand data frames using a yard-stick (incidence or population)
  if (is.null(data_pop)) {
    dataset_name <- "incidence"
    yard_stick   <- unique(data_inc$year)
  } else {
    dataset_name <- "population"
    yard_stick   <- unique(data_pop$year)
  }
  
  start_year <- min(yard_stick)
  end_year   <- max(yard_stick)
  
  missing <- setdiff(seq(start_year, end_year), yard_stick)
  
  if (length(missing) != 0) {
    stop(
      sprintf(
        "The %s data has missing years: %s",
        dataset_name, paste(missing, collapse = ", "),
        call. = FALSE
      )
    )
  }
  
  cancers <- Filter(Negate(is.null), list(data_inc = data_inc,
                                          data_mrt = data_mrt))
  
  cancers <- lapply(cancers, function(df) {
    
    sex_specific_cancers <- df %>%
      group_by(cancer.type) %>%
      filter(n_distinct(sex) < 2) %>%
      distinct(cancer.type, sex)
    
    df <- df %>%
      filter(year %in% yard_stick) %>%
      complete(cancer.type, sex, age.grp,
               year = yard_stick,
               fill = list(counts = 0)) %>%
      arrange(year, cancer.type, sex, age.grp)
    
    to_remove <- df %>%
      semi_join(sex_specific_cancers, by = "cancer.type") %>%
      anti_join(sex_specific_cancers, by = c("cancer.type", "sex"))
    
    df <- anti_join(df, to_remove, by = c("cancer.type", "sex"))
    
  })
  
  # -------------------------- #
  
  list2env(cancers, envir = environment())
  
  data_list <- Filter(Negate(is.null), list(data_inc = data_inc,
                                            data_mrt = data_mrt,
                                            data_pop = data_pop))
  
  # Create a year label for any aggregation that's needed
  if (aggregate_option) {start_year <- start_year + (end_year-start_year+1) %% 5}
  
  data_list <- lapply(data_list, function(df) {
    
    df <- df %>%
      filter(year >= start_year, year <= end_year) %>%
      mutate(
        "block_end"  = end_year - 5 * ((end_year - year) %/% 5),
        "year.label" = paste0(block_end - 4, "-", block_end),
        "last_range" = ifelse(year >= end_year - 4, TRUE, FALSE)
      ) %>%
      select(-block_end)
    
    if (aggregate_option & "counts" %in% names(df)) {
      
      df %>%
        group_by(year) %>%
        mutate("zero_year" = all(counts == 0)) %>%
        group_by(year.label) %>%
        filter(!any(zero_year)) %>%
        ungroup() %>%
        select(-zero_year)
      
    } else if ("counts" %in% names(df)) {
      
      df %>%
        group_by(year) %>%
        mutate("zero_year" = all(counts == 0)) %>%
        filter(!any(zero_year)) %>%
        ungroup() %>%
        select(-zero_year)
      
    } else { df <- df }
    
  })
  
  list(
    data = data_list,
    end_year = end_year
  )
  
}
