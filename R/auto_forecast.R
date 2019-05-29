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
#' @details Available models are: "auto.arima", "tbats", "ets", "nnetar", "hw", "arfima", "holt", "thetaf", "ses", "meanf", "splinef", "StrucTS", "elm". For more details about these models, see the documentation from forecast and nnfor packages.
#' @export auto_forecast
#' @return An object of class list containing the following elements:
#' \item{fc}{A tibble containing point forecasts and lower/upper limits from the bootstrapped distribution.}
#' \item{error}{A tibble containing out-of-sample forecast errors.}
#' \item{acc}{A tibble containing RMSE and MAE values for each model ordered according to the selected criteria.}
#' \item{dir}{A tibble containing the ordered directional accuracy for each model.}
#' \item{model}{Character vector with the selected model.}
#' \item{plot}{A ggplot object with graphical representation of both the point forecasts and confidence intervals.}
#' @description This function provides an algorithm to compute the best possible forecast from the available set of univariate time series. Additionaly, bootstrap methods are employed to both refine point forecasts and compute confidence intervals.
#' @importFrom magrittr %>%
#' @importFrom stats ts var
#' @examples
#'\dontrun{
#'library(rafa)
#'auto_forecast(USAccDeaths)
#'}

auto_forecast <- function(data, h = 12, h_cv = 1, window = NULL, acc = "MAE", n = 100, level = 0.05, exclude = NULL, test = NULL){
  
  Equal <- Model <- Point <- valor <- value <- `.id` <- `:=` <- NULL
  
  
  # First step: fit the built-in models in forecast package and compute accuracy measures. ----
  
  mod_fun <- list("auto.arima"  = function(x, h) forecast::forecast(forecast::auto.arima(x), h= h),
                  "tbats"  = function(x, h) forecast::forecast(forecast::tbats(x), h= h),
                  "ets"    = function(x, h) forecast::forecast(forecast::ets(x), h= h),
                  "nnetar" = function(x, h) forecast::forecast(forecast::nnetar(x), h= h),
                  "hw"     = function(x, h) forecast::hw(x, h= h),
                  "arfima" = function(x, h) forecast::forecast(forecast::arfima(x), h= h),
                  "holt" = function(x, h) forecast::holt(x, h= h),
                  "thetaf" = function(x, h) forecast::thetaf(x, h= h),
                  "ses" = function(x, h) forecast::ses(x, h= h),
                  "meanf" = function(x, h) forecast::meanf(x, h= h),
                  "splinef" = function(x, h) forecast::splinef(x, h= h),
                  "StrucTS" = function(x,h) forecast::forecast(stats::StructTS(x), h= h),
                  "elm" = function(x,h) forecast::forecast(nnfor::elm(x), h = h))
  
  # Allow the exclusion of specific models
  
  if(!is.null(exclude)){for(i in seq_along(exclude)){mod_fun[[exclude[[i]]]] <- NULL}}
  
  # Compute forecast errors via cross-validation or train/test sets
  
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
  
  # Compute the directional accuracy of models
  
  sign_obs <- data %>% diff() %>% sign() %>% timetk::tk_tbl() %>% magrittr::set_colnames(c("date", "sign_obs"))
  
  sign_pred <- purrr::map(.x = mod_erro, .f = function(x, ...){
    
    data %>% diff() %>% `-`(x) %>% sign()
    
  })
  
  sign_pred_aux <- purrr::map2(.x = sign_pred,
                               .y = as.list(names(sign_pred)),
                               .f = function(x,y, ...){
                                 
                                 timetk::tk_tbl(x) %>% magrittr::set_colnames(c("date", y))
                                 
                               }) %>% purrr::reduce(dplyr::inner_join, by = "date")
  
  sign_acc <- dplyr::inner_join(sign_obs, sign_pred_aux, by = "date")
  
  dir_W <- function(x, ...){table(x == sign_acc$sign_obs)["FALSE"]}
  dir_R <- function(x, ...){table(x == sign_acc$sign_obs)["TRUE"]}
  
  dir_acc <- sign_acc %>%
    
    dplyr::summarise_at(dplyr::vars(-date, -sign_obs), list("Right" = dir_R, "Wrong" = dir_W)) %>%
    
    dplyr::mutate_if(is.na, function(x){x = 0}) %>%
    
    dplyr::mutate_all(as.integer) %>%
    
    tidyr::gather(key = var, value = valor) %>%
    
    tidyr::separate(col = var, into = c("Model", "Equal"), sep = "_") %>%
    
    tidyr::spread(key = Equal, value = valor) %>%
    
    dplyr::arrange(dplyr::desc(Right))
  
  # Compute MAE and RMSE
  
  mae <- function(x){mean(abs(x), na.rm = TRUE)}
  
  rmse <- function(x){sqrt(mean(x^2, na.rm = TRUE))}
  
  mod_acc <- dplyr::tibble("Model" = names(mod_erro),
                           "MAE" = purrr::map_dbl(.x = mod_erro, .f = mae),
                           "RMSE" = purrr::map_dbl(.x = mod_erro, .f = rmse))
  
  ## Merge all accuracy measures
  
  mod_acc_order <- dplyr::inner_join(mod_acc, dir_acc, by = "Model")
  
  # Pick the best model based on the chosen measure (RMSE, MAE or ACC) and compute forecasts ----
  
  mod_acc_order_aux <- if(acc == "dir"){
    
    dplyr::arrange(mod_acc_order, desc(Right))
    
  } else {
    
    dplyr::arrange(mod_acc_order, eval(parse(text = acc)))
    
  }
  
  mod_best <- mod_acc_order_aux %>%
    
    dplyr::slice(1) %>%
    
    dplyr::select(Model) %>%
    
    as.character()
  
  mod_best_eval <- eval(parse(text = mod_best))
  
  # Compute point forecasts and confidence intervals through bootstrapped forecasts.
  
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
  
  # Plot forecasts.
  
  plot_fc <- prev %>% ggplot2::ggplot(ggplot2::aes(x = h, y = Point)) +
    
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
                            
  return(list("fc" = prev, "error" = mod_erro, "acc" = mod_acc_order_aux, "model" = mod_best, "plot" = plot_fc))
  
}
