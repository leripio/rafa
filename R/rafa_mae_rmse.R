#' Compute MAE and RMSE
#'
#' This function compute MAE and RMSE
#' @param mod_erro A list of data frames with errors of the various models used
#' @author J. Renato Leripio
#' @return An data frame with the MAE and RMSE values of each model used
#' @description
#' @examples
#'\dontrun{
#'library(rafa)
#' models <- rafa_list_models(exclude = c("nnetar", "hw", "arfima", "holt", "thetaf", "ses", "meanf", "splinef", "StructTS", "elm"))
#' modelos
#' errors <- rafa_compute_error(USAccDeaths, mod_fun = models)
#' errors
#' print(rafa_mae_rmse(errors))
#'}

rafa_mae_rmse <- function(mod_erro){
  mae <- function(x){mean(abs(x), na.rm = TRUE)}
  rmse <- function(x){sqrt(mean(x^2, na.rm = TRUE))}
  mod_acc <- dplyr::tibble("Model" = names(mod_erro),
                           "MAE" = purrr::map_dbl(.x = mod_erro, .f = mae),
                           "RMSE" = purrr::map_dbl(.x = mod_erro, .f = rmse))
}
