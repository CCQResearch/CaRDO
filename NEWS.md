# CaRDO 1.1.0 [November 2026]

* First major version update.

### Features
* The following features are now implemented and refined:
  * Aggregated years (impl. May 2025)
  * Adult age ranges (impl. July 2026)
  * Different year ranges (this version)
* The program now handles different year ranges between datasets, particularly between incidence and mortality datasets.
  * Note that population data can't be imputed if it's year range is different. Some trimming of data may occur to ensure calculations are made correctly.

### Program
* The program has be restructured from a single script to set of functions with updated documentation for each. See the list of functions below.
* `create_dashboard()` remains the main program function for users to set up their data and parameters.
* `transform_data()` function has been overhauled to include improved data validation as well as list comprehension.
* `fit_trendline()` [helper function] runs a linear model on the data to create yearly trendline.
* `process_years()` [helper function] contains all the logic to correctly handle different year ranges.
* `adj_adult_legal()` [helper function] modifies weights and population to accommodate for adult age ranges.
* `match_variables()` [helper function] attempts to match standard variable names to column names in the supplied dataset.

# CaRDO 1.0.2 [July 2026]

* Added the option to choose adult age ranges (15-85+) and (18-85+).
* Updated the data processing to calculate counts and rates appropriately for modified age ranges.

# CaRDO 1.0.1 [May 2025]

* Aggregation added as an option for registries that need to report in 5-year ranges.
* Added in improved error handling with slightly more informative descriptions on common errors.

# CaRDO 1.0.0 [February 2025]

* Initial GitHub submission.
* Main program and basic dashboard available.
* Bookdown tutorial and documentation written.
* All functions saved to `all_func.R`.
