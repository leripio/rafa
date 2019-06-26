#' Merge and order the accuracy measures
#'
#' This function merge and order the accuracy measures (RMSE, MAE or ACC)
#' @param mod_acc A data frame with the RMSE and MAE accuracy measures
#' @param dir_acc A data frame with directional accuracy measure
#' @param acc accuracy measure to determine the best model. Either "MAE" for mean absolute error, "RMSE" for root mean squared error or "dir" for directional.
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

rafa_order_acc <- function(mod_acc, dir_acc, acc = "MAE"){
  mod_acc_order <- dplyr::inner_join(mod_acc, dir_acc, by = "Model")

  if(acc == "dir"){
    dplyr::arrange(mod_acc_order, desc(Right))
  } else {
    dplyr::arrange(mod_acc_order, eval(parse(text = acc)))
    }
}
