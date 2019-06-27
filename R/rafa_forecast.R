#' Automatic forecasting from univariate time series
#'
#' @param data univariate time series (ts object).
#' @param h forecast horizon.
#' @param h_cv forecast horizon to compute accuracy by cross-validation.
#' @param window length of the rolling window for cross-validation. If NULL, a rolling window will not be used.
#' @param acc accuracy measure to determine the best model. Either "MAE" for mean absolute error, "RMSE" for root mean squared error or "dir" for directional.
#' @param n number of bootstrap simulations used to compute both mean forecasts (bagging) and confidence intervals.
#' @param level confidence level for prediction intervals.
#' @param exclude one or more models to exclude from evaluation, eg. "auto.arima" or c("tbats", "nnetar").
#' @param test first observation in the test set. Either a single number or a vector of two integers. If not NULL, overrides cross-validation arguments.
#' @author J. Renato Leripio
#' @details Available models are: "auto.arima", "tbats", "ets", "nnetar", "hw", "arfima", "holt", "thetaf", "ses", "meanf", "splinef", "StrucTS". For more details about these models, see the documentation from forecast package.
#' @export auto_forecast
#' @return An object of class list containing the following elements:
#' \item{fc}{A tibble containing point forecasts and lower/upper limits from the bootstrapped distribution.}
#' \item{error}{A tibble containing out-of-sample forecast errors.}
#' \item{acc}{A tibble containing RMSE and MAE values for each model ordered according to the selected criteria.}
#' \item{dir}{A tibble containing the ordered directional accuracy for each model.}
#' \item{model}{A character vector with the selected model.}
#' \item{plot}{A ggplot object with graphical representation of both the point forecasts and confidence intervals.}
#' @description This function provides an algorithm to compute the best possible forecast from the available set of univariate time series. Additionaly, bootstrap methods are employed to both refine point forecasts and compute confidence intervals.
#' @importFrom magrittr %>%
#' @importFrom stats ts var
#' @examples
#'\dontrun{
#'library(rafa)
#'rafa_forecast(USAccDeaths)
#'}

rafa_forecast <- function(data, h = 12, h_cv = 1, window = NULL, acc = "MAE", n = 100, level = 0.05, exclude = NULL, test = NULL){
  # List models to be used
  mod_fun <- rafa_list_models(exclude = exclude, h = h)

  # Compute forecast errors via cross-validation or train/test sets
  mod_erro <- rafa_compute_error(data = data, mod_fun = mod_fun, window = window, test = test, h_cv = h_cv)

  # Calculates the accuracy measures and order accordingly to the acc input
  mod_acc_order_aux <- rafa_order_acc(
    rafa_mae_rmse(mod_erro = mod_erro),
    rafa_dir_acc(data = data, mod_erro = mod_erro),
    acc = acc)

  # Names the best model
  mod_best <- mod_acc_order_aux %>%
    dplyr::slice(1) %>%
    dplyr::select(Model) %>%
    as.character()

  # Atributes the best model
  mod_best_eval <- eval(parse(text = mod_best))

  # Compute point forecasts and confidence intervals through bootstrapped forecasts.
  prev <- rafa_boot_pred(data, h = h, n = n, level = level, mod_best_eval = mod_best_eval)

  # Plot forecasts
  plot_fc <- rafa_plot_forecast(prev = prev, h = h, n = n, level = level, mod_best = mod_best)

  # Return items
  return(list("fc" = prev, "error" = mod_erro, "acc" = mod_acc_order_aux, "model" = mod_best, "plot" = plot_fc))
}
