#' Automatic forecasting from univariate time series
#'
#' @param prev A tibble containing point forecasts and lower/upper limits from the bootstrapped distribution.
#' @author J. Renato Leripio
#' @details Available models are: "auto.arima", "tbats", "ets", "nnetar", "hw", "arfima", "holt", "thetaf", "ses", "meanf", "splinef", "StrucTS". For more details about these models, see the documentation from forecast package.
#' @export auto_forecast
#' @return A ggplot object with graphical representation of both the point forecasts and confidence intervals.
#' @description
#' @examples
#'\dontrun{
#'library(rafa)
#'}

rafa_plot_forecast <- function(prev, level, mod_best, n, h){
  prev %>% ggplot2::ggplot(ggplot2::aes(x = h, y = Point)) +
    ggplot2::geom_line(lwd = 1.2, color = "steelblue3") +
    ggplot2::geom_ribbon(ggplot2::aes_string(ymin = paste("Lower", (1-level), sep = "_"),
                                             ymax = paste("Higher", (1-level), sep = "_")),
                         alpha = 0.3,
                         fill = "steelblue2") +
    ggplot2::labs(title = paste("Forecasts from model: ", mod_best, sep = ""),
                  subtitle = paste("Values based on", n, "simulations", sep = " "),
                  y = "",
                  x = "Horizon") +
    ggplot2::scale_x_continuous(breaks = 1:h)
}
