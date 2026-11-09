#' Fits a trend-line to the 'annual' data set
#' 
#' Runs a segmented regression model fitting a trend line to the yearly or
#' annual data set.
#'
#' @param data Dataframe. The data with measures by individual year.
#' @param x_val Vector. Independent variable, this will mostly be "year".
#' @param y_val Vector. The response will either be "counts" or "rates".
#' @param rates_req Logical. TRUE if `y_val` is "rates" and it will calculate APC.
#' 
#' @return Dataframe with the results of the regression model included.
#' 
#' @import segmented
#' @importFrom dplyr filter ensym
#' @importFrom magrittr %>%
#' @importFrom strucchange breakpoints

fit_trendline <- function(data, x_val, y_val, rates_req) {
  
  # ---------------------------------------------------------------------- #
  # MODEL SETUP
  
  data <- data %>% arrange(!!ensym(x_val))
  
  # Trendline parameters
  # ----------------------------------- #
  n_obs     <- nrow(data)
  seg_size  <- max(2, floor(n_obs / 3))
  max_joins <- floor(length(unique(data[[x_val]])) / 5)
  # ----------------------------------- #
  
  
  # Convert rates to log scale for regression
  # ----------------------------------- #
  if (rates_req) {
    data[[y_val]] <- log(data[[y_val]])
    data[[y_val]][data[[y_val]] == -Inf] <- 0
  }
  
  
  # Specify the breakpoints (strucchange)
  # ----------------------------------- #
  bp <- breakpoints(
    as.formula(paste0(y_val, " ~ ", x_val)),
    data   = data,
    breaks = max_joins,
    h      = seg_size
  )
  
  breakpoints <- data[[x_val]][bp$breakpoints]
  
  # ---------------------------------------------------------------------- #
  
  
  
  
  
  
  # ---------------------------------------------------------------------- #
  # INITIAL LINEAR MODEL
  
  o <- lm(as.formula(paste0(y_val, " ~ ", x_val)), data = data)
  
  if (any(is.na(breakpoints))) {
    # ----------------------------------- #
    # If there are no breakpoints, go straight to prediction...
    
    prediction <- predict(
      o,
      newdata  = data,
      se.fit   = TRUE,
      type     = "response",
      level    = 0.95,
      interval = "confidence"
    )
    
    ci_values <- data.frame(
      prediction$fit,
      "se" = prediction$se.fit
    )
    
  } else {
    # ----------------------------------- #
    # Otherwise, fit a segmented model first...
    
    os <- segmented(
      obj     = o,
      psi     = breakpoints,
      control = seg.control(it.max = 0)
    )
    
    prediction <- predict(
      os,
      newdata  = os$model,
      se.fit   = TRUE,
      type     = "response",
      level    = 0.95,
      interval = "confidence"
    )
    
    ci_values <- data.frame(
      prediction$fit,
      "se" = prediction$se.fit
    )
    
  }
  
  # Save results back to dataframe
  data$trend <- ci_values$fit
  data$lower <- ci_values$lwr
  data$upper <- ci_values$upr
  
  # ---------------------------------------------------------------------- #
  
  
  
  
  
  
  # ---------------------------------------------------------------------- #
  # ANNUAL PERCENTAGE CHANGE
  
  if (rates_req) {
    
    # APC on only the last segment, if there is one
    cutoff <- if (any(is.na(breakpoints))) min(data[[x_val]]) else max(breakpoints)
    
    tmp <- data %>% filter(!!ensym(x_val) >= cutoff)
    n   <- nrow(tmp)
    
    # Caulcating the slope and APC
    beta <- ( tmp$trend[1] - tmp$trend[n] ) / ( tmp[[x_val]][1] - tmp[[x_val]][n] )
    apc  <- ( exp(beta) - 1 ) * 100
    
    # Then calculate our standard error of our beta
    y_hat     <- tmp$trend
    residuals <- tmp[[y_val]] - y_hat
    SSE       <- sum(residuals^2)
    SXX       <- sum((tmp[[x_val]] - mean(tmp[[x_val]]))^2)
    
    se_beta <- sqrt((1/(n - 2)) * (SSE / SXX))
    
    df <- prediction$df
    
    apc_lower_ci <- 100 * (exp(beta - se_beta * qt(p = 0.975, df = df)) - 1)
    apc_upper_ci <- 100 * (exp(beta + se_beta * qt(p = 0.975, df = df)) - 1)
    
    data <- data %>%
      mutate(
        "apc"       = apc,
        "apc_time"  = paste0(cutoff, "-", max(data[[x_val]])),
        "apc_lower" = apc_lower_ci,
        "apc_upper" = apc_upper_ci,
        "signif"    = if_else(apc_lower_ci * apc_upper_ci < 0, NA, "*")
      )
    
    # Revert the log transform of the data
    data[[y_val]] <- exp(data[[y_val]])
    data$trend    <- exp(data$trend)
    data$lower    <- exp(data$lower)
    data$upper    <- exp(data$upper)
    
  }
  
  # ---------------------------------------------------------------------- #
  
  
  return(data)
  
}
