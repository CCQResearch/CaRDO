#' Adjusts the population and weights
#' 
#' Recalculates the population and weights when a specific age-group requires
#' splitting to match the required age range of the data.
#'
#' @param data Dataframe. Must have `age.grp` and a `population` column.
#' @param group Integer. The age-group code that needs to be split
#'  i.e. group 4 is 15-19.
#' @param share Integer. The number of single ages that remain in the `age.grp`
#'  after the split i.e. 2 = ages 18 and 19 remain from the 15-19 `age.grp`.
#'
#' @return Dataframe with population and weights adjusted for new age range.
#'
#' @import dplyr

adj_adult_legal <- function(data, group = 4, share = 2) {
  
  data %>% mutate("population" = if_else(age.grp == group,
                                         population * share / 5,
                                         population))
}
