#' Transform the data for the dashboard
#' 
#' Validates and standardises the supplied data before calculating measures by
#' year, sex, age and cancer. Once finished, the data is saved to
#' `/Shiny App/Data/` folder.
#' 
#' @param req_mortality_data Logical. TRUE if mortality data is supplied.
#' @param req_population_data Logical. TRUE if population data is supplied.
#' @param standard_pop RDS. A data frame of standard population weights read
#'  from the package.
#' @param std_pop_name String. The name of the population standard to be used.
#' @param supplied_params List. Parameters used in the dashboard.
#' @param suppress_threshold Integer. A number to set the threshold for
#'  low-count privacy.
#' @param aggregate_option Logical. TRUE if the user wants to aggregate their
#'  data by 5 years.
#' @param agerange_choice String. Age range covered by the supplied data.
#' 
#' @return No return value. Writes CSV files.
#' 
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer pivot_wider starts_with last_col
#' @importFrom haven read_dta

transform_data <- function(
    req_mortality_data, req_population_data,
    standard_pop, std_pop_name,
    supplied_params, suppress_threshold,
    aggregate_option, agerange_choice) {
  
  
  # -------------------------------------------------------------------------- #
  # Initialising Directories ----
  # -------------------------------------------------------------------------- #
  # Creating (or replacing) the directory to save all the data files for the app
  
  output_path <- "Shiny App/Data"
  
  if (dir.exists(output_path)) { unlink(output_path, recursive = TRUE) }
  dir.create(output_path, recursive = TRUE)
  
  # Start by popping the parameters in right away
  saveRDS(supplied_params, file.path(output_path, "supplied_params.RDS"))
  
  # -------------------------------------------------------------------------- #
  
  
  
  
  
  # -------------------------------------------------------------------------- #
  # Setup & Load Data ----
  # -------------------------------------------------------------------------- #
  # Variable names have already been standardised
  # ("year", "cancer.type", "sex", "age.grp", "counts", "population")
  
  # Read in all the required datasets up front
  data_inc <- readRDS("tmp/data_inc.RDS")
  data_mrt <- if (req_mortality_data)  readRDS("tmp/data_mrt.RDS") else NULL
  data_pop <- if (req_population_data) readRDS("tmp/data_pop.RDS") else NULL
  
  # Store data in a flexible list (only fills with what is available)
  data_frames <- Filter(Negate(is.null), list(data_inc = data_inc,
                                              data_mrt = data_mrt,
                                              data_pop = data_pop))
  
  # Set up the broad age groups
  # ----------------------------------- #
  age_grps <- if (agerange_choice == "adults_who") {
    list("15-34" = 4:7,
         "35-49" = 8:10,
         "50-64" = 11:13,
         "65+"   = 14:18) %>%
      stack() %>%
      rename("age.grp" = values,
             "age.label"   = ind)
  } else if (agerange_choice == "adults_legal") {
    list("18-34" = 4:7,
         "35-49" = 8:10,
         "50-64" = 11:13,
         "65+"   = 14:18) %>%
      stack() %>%
      rename("age.grp" = values,
             "age.label"   = ind)
  } else {
    list("0-34"  = 1:7,
         "35-49" = 8:10,
         "50-64" = 11:13,
         "65+"   = 14:18) %>%
      stack() %>%
      rename("age.grp" = values,
             "age.label" = ind)
  }
  # ----------------------------------- #
  
  # -------------------------------------------------------------------------- #
  
  
  
  
  
  # -------------------------------------------------------------------------- #
  # Initial Processing ----
  # -------------------------------------------------------------------------- #
  # Checking the year ranges and age ranges for any issues and generating an 
  # 'All persons' category if there isn't one.
  
  
  ## Validate Years ----
  # -------------------------------------------------------------------------- #
  
  years_result <- process_years(data_frames, aggregate_option)
  
  data_frames <- years_result$data
  end_year    <- years_result$end_year
  
  list2env(data_frames, envir = environment())
  
  # # Process the year variable for each dataset
  # validate_years <- lapply(data_frames, function(df) {
  #   
  #   # Finding the most restrictive year range across all datasets
  #   start_year <- max(vapply(data_frames, function(d) min(d$year), numeric(1)))
  #   end_year   <- min(vapply(data_frames, function(d) max(d$year), numeric(1)))
  #   
  #   # Handling unusual cases of year ranges.
  #   if (start_year > end_year) { stop("No overlapping years!") }
  #   if (aggregate_option) {start_year <- start_year + (end_year-start_year+1) %% 5}
  #   
  #   df <- df %>%
  #     filter(year >= start_year, year <= end_year) %>%
  #     mutate(
  #       "block_end"  = end_year - 5 * ((end_year - year) %/% 5),
  #       "year.label" = paste0(block_end - 4, "-", block_end),
  #       "last_range" = ifelse(year >= end_year - 4, TRUE, FALSE)
  #     ) %>%
  #     select(-block_end)
  #   
  #   list(
  #     data = df,
  #     end_year = end_year
  #   )
  #   
  # })
  # 
  # data_frames <- lapply(validate_years, `[[`, "data")
  # end_year    <- max(vapply(validate_years, `[[`, numeric(1), "end_year"))
  # 
  # list2env(data_frames, envir = environment())
  # 
  # # If population is missing years, there's nothing we can do
  # missing_years <- lapply(data_frames, function(df) {
  #   setdiff(min(df$year):max(df$year), df$year)
  # })
  # 
  # if (length(missing_years$data_pop) > 0) {
  #   stop(
  #     sprintf(
  #       "The population has missing year(s): %s",
  #       paste(missing_years$data_pop, collapse = ", "),
  #       call. = FALSE
  #     )
  #   )
  # }
  
  
  
  ## Validate Age Groups ----
  # -------------------------------------------------------------------------- #
  
  # Process the age group variable for each dataset
  validate_ages <- lapply(data_frames, function(df) {
    
    age_range <- if (agerange_choice == "all_ages") 1:18 else 4:18
    missing_ages = setdiff(age_range, df$age.grp)
    
    df <- df %>%
      merge(age_grps) %>%
      relocate(year, year.label, sex, age.grp, age.label) %>%
      arrange(year, sex, age.grp)
    
    list(
      data = df,
      missing_ages = missing_ages
    )
    
  })
  
  data_frames  <- lapply(validate_ages, `[[`, "data")
  missing_ages <- lapply(validate_ages, `[[`, "missing_ages")
  
  list2env(data_frames, envir = environment())
  
  # If age groups are missing across ALL variables
  n_missing <- lengths(missing_ages) > 0
  
  if (any(n_missing)) {
    
    data_names <- missing_ages[n_missing]
    
    message <- vapply(names(data_names), function(n) {
      grps <- data_names[[n]]
      sprintf("`%s` is missing age group(s): %s", n, paste(grps, collapse = ", "))
    }, character(1))
    
    stop(paste(message, collapse = "\n", call. = FALSE))
    
  }
  
  
  
  ## Generating 'Persons' ----
  # -------------------------------------------------------------------------- #
  
  cancer_frames <- Filter(Negate(is.null), list(data_inc = data_inc, data_mrt = data_mrt))
  
  generate_persons <- lapply(cancer_frames, function(df) {
    
    # Calculating and appending the 'Persons' category
    temporary <- df %>%
      mutate('sex' = 3) %>%
      group_by(across(-c(counts))) %>%
      summarise('counts' = sum(counts), .groups = 'drop') %>%
      ungroup()
    
    df <- bind_rows(df, temporary) %>%
      dplyr::select(year, year.label, cancer.type, sex, age.grp, age.label, counts, last_range) %>%
      arrange(year, cancer.type, sex, age.grp, counts, last_range)
    
    # Removes the 'Persons' category for sex-specific cancers
    sex_specific_cancers <- df %>%
      group_by(cancer.type) %>%
      filter(n_distinct(sex) < 3) %>%
      distinct(cancer.type) %>%
      pull(cancer.type)
    
    df <- df %>% filter(!(cancer.type %in% sex_specific_cancers & sex == 3))
    
    list(
      data = df,
      sex_specific_cancers = sex_specific_cancers
    )
    
  })
  
  cancer_frames        <- lapply(generate_persons, `[[`, "data")
  sex_specific_cancers <- lapply(generate_persons, `[[`, "sex_specific_cancers")
  
  list2env(cancer_frames, envir = environment())
  
  # -------------------------------------------------------------------------- #
  
  
  
  
  
  # -------------------------------------------------------------------------- #
  # Population Procesing ----
  # -------------------------------------------------------------------------- #
  # Check to see if the standard population weights match the age range
  # structure and merges the population data with the cancer data.
  
  if (req_population_data) {
    
    ## General Population ----
    # ----------------------------------- #
    standard <- standard_pop %>%
      filter(std_name == std_pop_name) %>%
      rename(
        "population" = "pop",
        "weight"     = "wght",
        "age.grp"    = "ragegrp",
        "age.label"  = "ragegrp_name"
      ) %>%
      dplyr::select(age.grp, age.label, population, weight) %>%
      { if (agerange_choice != "all_ages") filter(., age.grp > 3) else . } %>%
      mutate("weight" = population / sum(population)) %>%
      dplyr::select(age.grp, age.label, population, weight)
    
    # "adults_legal" splits an age-group so weights need to be adjusted.
    if (agerange_choice == "adults_legal") {
      
      data_pop <- adj_adult_legal(data_pop) # adjust population
      
      standard <- standard %>%              # adjust weights
        adj_adult_legal() %>%
        mutate("weight" = population / sum(population))
      
    }
    
    standard <- standard %>%
      dplyr::select(-age.label) %>%
      left_join(age_grps, by = "age.grp") %>%
      dplyr::select(age.grp, age.label, weight)
    # ----------------------------------- #
    
    
    
    # Merging Population ----
    # NOTE: Extracting cancers one-by-one keeps sex-specific cancers isolated
    # ----------------------------------- #
    cancer_frames <- lapply(cancer_frames, function(df) {
      
      merge_pop <- data.frame()
      
      min_year <- min(df$year)
      max_year <- max(df$year)
      
      for (cancer in unique(df$cancer.type)) {
        
        temp_cancer <- df %>% filter(cancer.type == cancer)
        
        temp_pop <- data_pop %>%
          filter(
            year >= min_year & year <= max_year,
            sex %in% unique(temp_cancer$sex)
          ) %>%
          left_join(
            temp_cancer, by = c("year", "year.label", "sex",
                                "age.grp", "age.label", "last_range")
          ) %>%
          mutate(
            "counts" = coalesce(counts, 0),
            "cancer.type" = cancer
          )
        
        # Append the result for each cancer
        merge_pop <- bind_rows(merge_pop, temp_pop) %>%
          relocate(
            any_of(c("year", "year.label")), cancer.type, sex,
            any_of(c("age.grp", "age.label")), counts
          )
        
        rm(temp_cancer, temp_pop)
      }
      
      merge_pop
      
    })
    # ----------------------------------- #
    
    list2env(cancer_frames, envir = environment())
    
  }
  
  # -------------------------------------------------------------------------- #
  
  
  
  
  
  # -------------------------------------------------------------------------- #
  # Delete Original Data ----
  # Now that the data has been read in and screened the data is deleted
  unlink("tmp", recursive = TRUE)
  
  incProgress(2/10, detail = "Calculating...")
  # -------------------------------------------------------------------------- #
  
  
  
  
  
  # -------------------------------------------------------------------------- #
  # RATES ----
  # -------------------------------------------------------------------------- #
  # If population data is supplied then rates and lifetime risk can can be
  # calculated.

  if (req_population_data & aggregate_option) {
    
    ## AGGREGATION ----
    
    data_processing <- lapply(cancer_frames, function(df) {
      
      ### STEP 1: Aggregate years ----
      # ----------------------------------- #
      df <- df %>%
        dplyr::select(-year) %>%
        # Aggregate Years
        group_by(year.label, cancer.type, sex, age.grp, age.label, last_range) %>%
        summarise(across(everything(), sum), .groups = "drop") %>%
        rename("year" = year.label) %>%
        relocate(
          year, cancer.type, sex, age.grp,
          any_of(c("counts", "population"))
        )
      # ----------------------------------- #
      
      
      ### STEP 2: Annual / by Year-Ranges ----
      # ----------------------------------- #
      annual <- df %>%
        merge(standard) %>%
        relocate(
          year, cancer.type, sex,
          age.grp, age.label,
          counts, population, weight
        ) %>%
        arrange(year, cancer.type, sex, age.grp) %>%
        # Calculate Rates
        mutate("rates" = (counts / population) * weight * 100000) %>%
        # Aggregate Age Groups
        group_by(year, cancer.type, sex) %>%
        summarise(
          "counts"    = sum(counts),
          "rates"     = sum(rates),
          .groups     = "drop"
        ) %>%
        pivot_longer(
          cols      = c("counts", "rates"),
          names_to  = "measure",
          values_to = "obs"
        )
      # ----------------------------------- #
      
      
      ### STEP 3: Broad age-groups ----
      # ----------------------------------- #
      age <- df %>%
        merge(standard) %>%
        relocate(
          year, cancer.type, sex,
          age.grp, age.label,
          counts, population, weight
        ) %>%
        arrange(year, cancer.type, sex, age.grp) %>%
        # Calculate Age Specific Rates
        mutate("asr" = (counts / population) * 100000) %>%
        # Re-weight by Broad Age Group
        group_by(year, cancer.type, sex, age.label) %>%
        mutate(
          "broad_weight" = weight / sum(weight),
          "rates" = asr * broad_weight
        ) %>%
        # Aggregate Age Groups
        summarise(
          "counts" = sum(counts),
          "rates"  = sum(rates),
          .groups = "drop"
        ) %>%
        pivot_longer(
          cols      = c("counts", "rates"),
          names_to  = "measure",
          values_to = "obs"
        )
      # ----------------------------------- #
      
      
      ### STEP 4: Cancer ----
      # ----------------------------------- #
      cancer <- df %>%
        # Only report last 5 years
        filter(last_range) %>%
        merge(standard) %>%
        relocate(
          year, cancer.type, sex,
          age.grp, age.label,
          counts, population, weight
        ) %>%
        arrange(year, cancer.type, sex, age.grp) %>%
        # Calculate Rates
        mutate("rates"  = (counts / population) * weight * 100000) %>%
        # Aggregate Age Groups
        group_by(year, cancer.type, sex) %>%
        summarise(
          "counts" = round(sum(counts), 1),  # Only reporting 5-year average
          "rates"  = sum(rates),
          .groups  = "drop"
        ) %>%
        pivot_longer(
          cols      = c("counts", "rates"),
          names_to  = "measure",
          values_to = "obs"
        )
      # ----------------------------------- #
      
      
      ### STEP 5: Lifetime Risk ----
      # ----------------------------------- #
      lifetime <- df %>%
        filter(last_range, age.grp != 18) %>%
        group_by(year, cancer.type, sex) %>%
        summarise(
          "lifetime.risk" = 1 - exp(-5 * sum(counts / population)),
          .groups = "drop"
        )
      # ----------------------------------- #
      
      list(
        annual   = annual,
        age      = age,
        cancer   = cancer,
        lifetime = lifetime
      )
      
    })
    
    dashboard_frames <- unlist(data_processing, recursive = FALSE)
    
  } else if (req_population_data & !aggregate_option) {
    
    ## YEARLY ----
    
    data_processing <- lapply(cancer_frames, function(df) {
      
      ### STEP 1: Annual / by Year-Ranges ----
      # ----------------------------------- #
      annual <- df %>%
        merge(standard) %>%
        relocate(
          year, cancer.type, sex,
          age.grp, age.label,
          counts, population, weight
        ) %>%
        arrange(year, cancer.type, sex, age.grp) %>%
        # Calculate Rates
        mutate("rates" = (counts / population) * weight * 100000) %>%
        # Aggregate Age Groups
        group_by(year, cancer.type, sex) %>%
        summarise(
          "counts"    = sum(counts),
          "rates"     = sum(rates),
          .groups     = "drop"
        ) %>%
        pivot_longer(
          cols      = c("counts", "rates"),
          names_to  = "measure",
          values_to = "obs"
        )
      # ----------------------------------- #
      
      
      ### STEP 2: Broad age-groups ----
      # ----------------------------------- #
      age <- df %>%
        merge(standard) %>%
        relocate(
          year, cancer.type, sex,
          age.grp, age.label,
          counts, population, weight
        ) %>%
        arrange(year, cancer.type, sex, age.grp) %>%
        # Calculate Age Specific Rates
        mutate("asr" = (counts / population) * 100000) %>%
        # Re-weight by Broad Age Group
        group_by(year, cancer.type, sex, age.label) %>%
        mutate(
          "broad_weight" = weight / sum(weight),
          "rates" = asr * broad_weight
        ) %>%
        # Aggregate (Broad) Age Groups
        summarise(
          "counts" = sum(counts),
          "rates"  = round(sum(rates), 1),
          .groups = "drop"
        ) %>%
        pivot_longer(
          cols      = c("counts", "rates"),
          names_to  = "measure",
          values_to = "obs"
        )
      # ----------------------------------- #
      
      
      ### STEP 3: Cancer (5-year average) ----
      # ----------------------------------- #
      cancer <- df %>%
        # Only report last 5 years
        filter(last_range) %>%
        merge(standard) %>%
        relocate(
          year, cancer.type, sex,
          age.grp, age.label,
          counts, population, weight
        ) %>%
        arrange(year, cancer.type, sex, age.grp) %>%
        # Aggregate Years
        group_by(year.label, cancer.type, sex, age.grp, weight) %>%
        summarise(
          "counts"     = sum(counts),
          "population" = sum(population),
          .groups      = "drop"
        ) %>%
        # Calculate Rates
        rename("year" = year.label) %>%
        mutate("rates"  = (counts / population) * weight * 100000) %>%
        # Aggregate Age Groups
        group_by(year, cancer.type, sex) %>%
        summarise(
          "counts" = round(sum(counts) / 5, 1),  # Only reporting 5-year average
          "rates"  = sum(rates),
          .groups  = "drop"
        ) %>%
        pivot_longer(
          cols      = c("counts", "rates"),
          names_to  = "measure",
          values_to = "obs"
        )
      # ----------------------------------- #
      
      
      ### STEP 4: Lifetime Risk ----
      # ----------------------------------- #
      lifetime <- df %>%
        filter(last_range, age.grp != 18) %>%
        group_by(year.label, cancer.type, sex, age.grp) %>%
        # Aggregating Years
        summarise(
          "counts"     = sum(counts),
          "population" = sum(population),
          .groups = "drop"
        ) %>%
        group_by(year.label, cancer.type, sex) %>%
        # Aggregating Age Groups
        summarise(
          "lifetime.risk" = 1 - exp(-5 * sum(counts / population)),
          .groups = "drop"
        )
      # ----------------------------------- #
      
      list(
        annual   = annual,
        age      = age,
        cancer   = cancer,
        lifetime = lifetime
      )
      
    })
    
    dashboard_frames <- unlist(data_processing, recursive = FALSE)
    
  }
  
  # -------------------------------------------------------------------------- #
  
  
  
  
  
  # -------------------------------------------------------------------------- #
  # COUNTS ----
  # -------------------------------------------------------------------------- #
  # When no population data is supplied, only counts need to be generated.
  
  if (!req_population_data & aggregate_option) {
    
    ## AGGREGATION ----
    
    data_processing <- lapply(cancer_frames, function(df) {
      
      ### STEP 1: Aggregate years ----
      # ----------------------------------- #
      df <- df %>%
        dplyr::select(-year) %>%
        # Aggregate Years
        group_by(year.label, cancer.type, sex, age.grp, age.label, last_range) %>%
        summarise(across(everything(), sum), .groups = "drop") %>%
        rename("year" = year.label) %>%
        relocate(
          year, cancer.type, sex, age.grp,
          any_of(c("counts", "population"))
        )
      # ----------------------------------- #
      
      
      ### STEP 2: Annual / by Year-Ranges ----
      # ----------------------------------- #
      annual <- df %>%
        relocate(year, cancer.type, sex,
                 age.grp, age.label, counts) %>%
        arrange(year, cancer.type, sex, age.grp) %>%
        # Aggregate Age Groups
        group_by(year, cancer.type, sex) %>%
        summarise(
          "measure" = "counts",
          "obs"     = sum(counts),
          .groups   = "drop")
      # ----------------------------------- #
      
      
      ### STEP 3: Broad age-groups ----
      # ----------------------------------- #
      age <- df %>%
        relocate(year, cancer.type, sex,
                 age.grp, age.label, counts) %>%
        arrange(year, cancer.type, sex, age.grp) %>%
        # Aggregate Age Groups
        group_by(year, cancer.type, sex, age.label) %>%
        summarise(
          "measure" = "counts",
          "obs"     = sum(counts),
          .groups   = "drop"
        )
      # ----------------------------------- #
      
      
      ### STEP 4: Cancer ----
      # ----------------------------------- #
      cancer <- df %>%
        # Only report last 5 years
        filter(last_range) %>%
        relocate(year, cancer.type, sex,
                 age.grp, age.label, counts) %>%
        arrange(year, cancer.type, sex, age.grp) %>%
        # Aggregate Age Groups
        group_by(year, cancer.type, sex) %>%
        summarise(
          "measure" = "counts",
          "obs"     = round(sum(counts), 1),
          .groups   = "drop"
        )
      # ----------------------------------- #
      
      list(
        annual   = annual,
        age      = age,
        cancer   = cancer
      )
      
    })
    
    dashboard_frames <- unlist(data_processing, recursive = FALSE)
    
  } else if (!req_population_data & !aggregate_option) {
    
    ## YEARLY ----
    
    data_processing <- lapply(cancer_frames, function(df) {
      
      ### STEP 1: Annual / by Year-Ranges ----
      # ----------------------------------- #
      annual <- df %>%
        relocate(year, cancer.type, sex,
                 age.grp, age.label, counts) %>%
        arrange(year, cancer.type, sex, age.grp) %>%
        # Aggregate Age Groups
        group_by(year, cancer.type, sex) %>%
        summarise(
          "measure" = "counts",
          "obs"     = sum(counts),
          .groups   = "drop")
      # ----------------------------------- #
      
      
      ### STEP 2: Broad age-groups ----
      # ----------------------------------- #
      age <- df %>%
        relocate(year, cancer.type, sex,
                 age.grp, age.label, counts) %>%
        arrange(year, cancer.type, sex, age.grp) %>%
        # Aggregate Age Groups
        group_by(year, cancer.type, sex, age.label) %>%
        summarise(
          "measure" = "counts",
          "obs"     = sum(counts),
          .groups   = "drop"
        )
      # ----------------------------------- #
      
      
      ### STEP 3: Cancer (5-year average) ----
      # ----------------------------------- #
      cancer <- data_inc %>%
        # Only report last 5 years
        filter(last_range) %>%
        relocate(year, cancer.type, sex,
                 age.grp, age.label, counts) %>%
        arrange(year, cancer.type, sex, age.grp) %>%
        # Aggregate Years
        group_by(year.label, cancer.type, sex, age.grp) %>%
        summarise("counts" = sum(counts), .groups = "drop") %>%
        rename("year" = year.label) %>%
        # Aggregate Age Groups
        group_by(year, cancer.type, sex) %>%
        summarise(
          "measure" = "counts",
          "obs"     = round(sum(counts) / 5, 1),    # Only reporting 5-year average
          .groups   = "drop"
        )
      # ----------------------------------- #
      
      list(
        annual   = annual,
        age      = age,
        cancer   = cancer
      )
      
    })
    
    dashboard_frames <- unlist(data_processing, recursive = FALSE)
    
  }
  
  # -------------------------------------------------------------------------- #
  
  
  
  
  
  # -------------------------------------------------------------------------- #
  # Trends Calculation ----
  # -------------------------------------------------------------------------- #
  # Specifically for the 'Annual' data frame, calculate the trends by fitting a
  # segmented regression model. NOTE: if the user aggregates, trends cannot be
  # calculated.
  
  if (req_population_data & !aggregate_option) {
    
    ## RATES ----
    
    annual_frames <- dashboard_frames[endsWith(names(dashboard_frames), ".annual")]
    
    annual_frames <- lapply(annual_frames, function(df) {
      
      stratify  <- df %>% distinct(cancer.type, sex)
      temporary <- data.frame()
      
      for (pair in seq_len(nrow(stratify))) {
        
        # Setup
        # ----------------------------------- #
        cancer <- stratify$cancer.type[pair]
        sex_no <- stratify$sex[pair]
        
        trends <- df %>%
          filter(cancer.type == cancer, sex == sex_no) %>%
          pivot_wider(names_from = "measure", values_from = "obs")
        # ----------------------------------- #
        
        
        # Calculating Trends, Confidence Intervals & APC 
        # ----------------------------------- #
        trends_counts <- suppressWarnings(
          fit_trendline(trends, "year", "counts", rates_req = FALSE)
        ) %>%
          dplyr::select(-rates) %>%
          rename(
            "trends_counts" = trend,
            "tc_lower"      = lower,
            "tc_upper"      = upper
          )
        
        trends_rates <- suppressWarnings(
          fit_trendline(trends, "year", "rates", rates_req = TRUE)
        ) %>%
          dplyr::select(-counts) %>%
          rename(
            "trends_rates"  = trend,
            "tr_lower"      = lower,
            "tr_upper"      = upper
          )
        # ----------------------------------- #
        
        
        # Transforming the output for consistency
        # ----------------------------------- #
        data_merge <- merge(trends_counts, trends_rates)
        
        data_merge_long <- data_merge %>%
          dplyr::select(-contains(c("trends", "tc", "tr", "apc", "signif"))) %>%
          pivot_longer(
            cols      = c("counts", "rates"),
            names_to  = "measure",
            values_to = "obs"
          )
        
        trends_merge_long <- data_merge %>%
          dplyr::select(-c("counts", "rates")) %>%
          rename(
            "counts_obs"   = trends_counts, "rates_obs"    = trends_rates,
            "counts_lower" = tc_lower,      "counts_upper" = tc_upper,
            "rates_lower"  = tr_lower,      "rates_upper"  = tr_upper
          ) %>%
          pivot_longer(
            cols = c("counts_obs",   "rates_obs",
                     "counts_lower", "counts_upper",
                     "rates_lower",  "rates_upper"),
            names_to  = c("measure", "type"),
            names_sep = "_",
            values_to = "obs_trend"
          ) %>%
          pivot_wider(names_from = "type", values_from = "obs_trend") %>%
          rename("trends" = obs)
        # ----------------------------------- #
        
        
        # The final rearranging and stitching it back together
        # ----------------------------------- #
        data_long <- merge(data_merge_long, trends_merge_long) %>%
          relocate(
            c(tidyr::starts_with("apc"), "signif"),
            .after = tidyr::last_col()
          )
        
        temporary <- bind_rows(temporary, data_long)
        # ----------------------------------- #
        
      }
      
      temporary
      
    })
    
    dashboard_frames[names(annual_frames)] <- annual_frames
    
  } else if (!req_population_data & !aggregate_option) {
    
    ## COUNTS ----
    
    annual_frames <- dashboard_frames[endsWith(names(dashboard_frames), ".annual")]
    annual_frames <- lapply(annual_frames, function(df) {
      
      stratify  <- df %>% distinct(cancer.type, sex)
      temporary <- data.frame()
      
      for (pair in seq_len(nrow(stratify))) {
        
        # Setup
        # ----------------------------------- #
        cancer <- stratify$cancer.type[pair]
        sex_no <- stratify$sex[pair]
        
        trends <- df %>%
          filter(cancer.type == cancer, sex == sex_no) %>%
          pivot_wider(names_from = "measure", values_from = "obs")
        # ----------------------------------- #
        
        
        # Calculating Trends, Confidence Intervals & APC
        # ----------------------------------- #
        trends_counts <- suppressWarnings(
          fit_trendline(trends, "year", "counts", rates_req = FALSE)
        ) %>%
          rename(
            "trends"   = trend,
            "tc_lower" = lower,
            "tc_upper" = upper
          ) %>%
          pivot_longer(
            cols      = "counts",
            names_to  = "measure",
            values_to = "obs"
          )
        
        temporary <- bind_rows(temporary, trends_counts)
        # ----------------------------------- #
        
      }
      
      temporary
      
    })
    
    dashboard_frames[names(annual_frames)] <- annual_frames
    
  }
  
  incProgress(4/10, detail = "Finalising...")
  
  # -------------------------------------------------------------------------- #
  
  
  
  
  
  # -------------------------------------------------------------------------- #
  # Suppression ----
  # -------------------------------------------------------------------------- #
  # For privacy, low-counts should be 'suppressed' or censored. Using the 
  # threshold, provided, filter the datasets appropriately.
  
  ## Cancer averages ----
  # ----------------------------------- #
  cancer_averages <- dashboard_frames[endsWith(names(dashboard_frames), ".cancer")]
  
  cancer_averages <- lapply(cancer_averages, function(df) {
    
    low_counts <- df %>%
      filter(
        measure == "counts",
        obs * 5 < suppress_threshold
      ) %>%
      dplyr::select(year, sex, cancer.type)
    
    df %>% anti_join(low_counts, by = c("year", "sex", "cancer.type"))
    
  })
  
  dashboard_frames[names(cancer_averages)] <- cancer_averages
  # ----------------------------------- #
  
  
  ## Annual & Age ----
  # ----------------------------------- #
  if (req_population_data) {
    
    ### RATES ----
    
    pattern <- grepl("\\.(annual|age)$", names(dashboard_frames))
    suppression_frames <- dashboard_frames[pattern]
    
    suppression_frames <- lapply(suppression_frames, function(df) {
      
      # Remove trends variables. Age data frames won't have them
      temporary <- df %>% dplyr::select(-contains(c("trends", "lower", "upper", "apc", "signif")))
      
      # Pivot wider to apply suppression to counts and rates (only for Rates)
      df <- temporary %>%
        pivot_wider(
          names_from = measure,
          values_from = obs
        ) %>%
        mutate(
          "suppress" = if_else(counts < suppress_threshold, true = 1, false = 0),
          "counts"   = if_else(suppress == 1, true = NA, false = counts),
          "rates"    = if_else(suppress == 1, true = NA, false = rates)
        ) %>%
        pivot_longer(
          cols      = c("counts", "rates"),
          names_to  = "measure",
          values_to = "obs"
        ) %>%
        merge(df)
      
    })
    
    dashboard_frames[names(suppression_frames)] <- suppression_frames
    
  } else if (!req_population_data) {
    
    ### COUNTS ----
    pattern <- grepl("\\.(annual|age)$", names(dashboard_frames))
    suppression_frames <- dashboard_frames[pattern]
    
    suppression_frames <- lapply(suppression_frames, function(df) {
      
      # Remove trends variables. Age data frames won't have them
      temporary <- df %>% dplyr::select(-contains(c("trends", "lower", "upper", "apc", "signif")))
      
      df <- temporary %>%
        mutate(
          "suppress" = if_else(measure == "counts" & obs < suppress_threshold, true = 1, false = 0),
          "obs"      = if_else(suppress == 1, true = NA, false = obs)
        ) %>%
        merge(df)
      
    })
    
    dashboard_frames[names(suppression_frames)] <- suppression_frames
    
  }
  # ----------------------------------- #
  
  
  incProgress(2/10, detail = "Saving...")
  
  # -------------------------------------------------------------------------- #
  
  
  
  
  
  # -------------------------------------------------------------------------- #
  # Write DATA to File ----
  # -------------------------------------------------------------------------- #
  # Loop through the data frames in the list and write each one to file
  
  for (frames in seq_along(dashboard_frames)) {
    
    df    <- dashboard_frames[[frames]]
    title <- paste0(names(dashboard_frames)[frames], ".csv")
    
    write.csv(
      df,
      file.path(output_path, title),
      row.names = FALSE
    )
    
  }
  
  # -------------------------------------------------------------------------- #
  
  
  
  
  
  # -------------------------------------------------------------------------- #
  # Write DASHBOARD to File ----
  # -------------------------------------------------------------------------- #
  # Copy the dashboard template, saved in the package, and write that to file
  
  from_dir <- "Shiny App"
  path     <- system.file("extdata/app_files", package = "CaRDO")
  
  for (item in list.files(path)) {
    
    from <- file.path(path, item)
    
    # ----------------------------------- #
    if (!file_test("-f", from)) {
      to         <- from_dir
      to_recurse <- TRUE
    } else {
      to         <- file.path(from_dir, item)
      to_recurse <- FALSE
    }
    # ----------------------------------- #
    
    file.copy(
      from, to,
      recursive = to_recurse,
      overwrite = TRUE
    )
    
  }
  
  # -------------------------------------------------------------------------- #
  
  incProgress(1/10)
  
}
