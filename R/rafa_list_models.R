#' Creates list of forecast models to use
#'
#' This function creates a list of models to later use in automatic forecast
#' @param exclude One or more models to exclude from evaluation, eg. "auto.arima" or c("tbats", "nnetar").
#' @param h forecast horizon.
#' @return A list of models.
#' @author J. Renato Leripio
#' @details Available models are: "auto.arima", "tbats", "ets", "nnetar", "hw", "arfima", "holt", "thetaf", "ses", "meanf", "splinef", "StrucTS". For more details about these models, see the documentation from forecast package.
#' @examples
#'\dontrun{
#' library(rafa)
#' rafa_list_functions(exclude = NULL)
#' rafa_list_functions(exclude = "ses")
#' }

rafa_list_models <- function(exclude = NULL, h = 12){
  force(h)
  mod_fun <- list("auto.arima"  = function(x, h) forecast::forecast(forecast::auto.arima(x), h = h),
                  "tbats"  = function(x, h) forecast::forecast(forecast::tbats(x), h = h),
                  "ets"    = function(x, h) forecast::forecast(forecast::ets(x), h = h),
                  "nnetar" = function(x, h) forecast::forecast(forecast::nnetar(x), h = h),
                  "hw"     = function(x, h) forecast::hw(x, h = h),
                  "arfima" = function(x, h) forecast::forecast(forecast::arfima(x), h = h),
                  "holt" = function(x, h) forecast::holt(x, h = h),
                  "thetaf" = function(x, h) forecast::thetaf(x, h = h),
                  "ses" = function(x, h) forecast::ses(x, h = h),
                  "meanf" = function(x, h) forecast::meanf(x, h = h),
                  "splinef" = function(x, h) forecast::splinef(x, h = h),
                  "StructTS" = function(x,h) forecast::forecast(stats::StructTS(x), h = h),
                  "elm" = function(x,h) forecast::forecast(nnfor::elm(x), h = h))

  # Allow the exclusion of specific models
  if(!is.null(exclude)){
    for(i in seq_along(exclude))
    {mod_fun[[exclude[[i]]]] <- NULL}
  }
  mod_fun
}
