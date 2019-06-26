#' Compute forecast errors via cross-validation or train/test sets
#'
#' This function compute forecast errors via cross-validation or train/test sets.
#' @param data univariate time series (ts object).
#' @param mod_fun a list of models to be assessed
#' @param h_cv forecast horizon to compute accuracy by cross-validation.
#' @param window length of the rolling window for cross-validation. If NULL, a rolling window will not be used.
#' @return A list of data frames with errors of the various models used
#' @export
#' @examples
#' models <- rafa_list_models(exclude = c("nnetar", "hw", "arfima", "holt", "thetaf", "ses", "meanf", "splinef", "StructTS", "elm"))
#' modelos
#' errors <- rafa_compute_error(USAccDeaths, mod_fun = models)
#' errors

rafa_compute_error <- function(data, mod_fun, window = NULL, test = NULL, h_cv = 1){
  if(!is.null(test)){
    data_train <- window(data, end = test)
    data_test <- window(data, start = test)
    mod_fit <- purrr::invoke_map(.x = list(data_train), .f = mod_fun, h = length(data_test))
    mod_erro <- purrr::map(.x = mod_fit, .f = function(x){
      x$mean - data_test
    })
  } else {
    mod_cv <- purrr::map2(.f = forecast::tsCV, .x = list(data), .y = mod_fun, h = h_cv, window = window)
    if(h_cv > 1){
      mod_cv <- purrr::map(.x = mod_cv, .f = function(x){x[, h_cv]})
    }
    mod_cv_aux <- purrr::map(.x = mod_cv, .f = function(x, ...){
      y <- timetk::tk_tbl(x) %>% tidyr::drop_na()
      z <- y$value %>% ts(start = xts::first(y$index), end = xts::last(y$index), frequency = stats::frequency(data))
      return(z)
    })
    names(mod_cv_aux) <- names(mod_fun)
    mod_erro <- mod_cv_aux
  }
}
