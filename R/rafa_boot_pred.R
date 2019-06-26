#' Compute point forecasts and confidence intervals through bootstrapped forecasts.
#'
#' @param data univariate time series (ts object).
#' @param h forecast horizon.
#' @param n number of bootstrap simulations used to compute both mean forecasts (bagging) and confidence intervals.
#' @param level confidence level for prediction intervals.
#' @author J. Renato Leripio
#' @return A tibble containing point forecasts and lower/upper limits from the bootstrapped distribution.
#' @description
#' @importFrom magrittr %>%
#' @importFrom stats ts var
#' @examples
#'\dontrun{
#'library(rafa)
#'}

rafa_boot_pred <- function(data, h, n, level, mod_best_eval){
  mod_boot <- forecast::bld.mbb.bootstrap(data, n)

  mod_boot_fc <- purrr::map(.x = mod_boot, .f = function(x) forecast::forecast(mod_best_eval(x), h = h))

  mod_boot_fc_aux <- purrr::map(.x = mod_boot_fc, .f = function(x){x[["mean"]] %>% as.numeric()})

  names(mod_boot_fc_aux) <- paste("Series", 1:length(mod_boot_fc_aux), sep = "_")

  mod_boot_fc_aux_df <- plyr::ldply(mod_boot_fc_aux, rbind)

  prev <- mod_boot_fc_aux_df %>%
    tidyr::gather(key = h, value = value, -.id) %>%
    dplyr::mutate(h = as.numeric(h)) %>%
    dplyr::group_by(h) %>%
    dplyr::summarise(Point = mean(value),
                     !! paste("Lower", (1-level), sep = "_") := stats::quantile(value, prob = level/2),
                     !! paste("Higher", (1-level), sep = "_") := stats::quantile(value, prob = (1-level/2)))
  }
