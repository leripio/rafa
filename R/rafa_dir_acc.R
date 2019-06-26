#' Compute the directional accuracy of models
#'
#' This function compute the directional accuracy of models
#' @param data univariate time series (ts object).
#' @param mod_erro list of data frames containing the prediction error of various models
#' @author J. Renato Leripio
#' @return
#' @importFrom magrittr %>%
#' @importFrom stats ts var
#' @examples
#'\dontrun{
#'library(rafa)
#' models <- rafa_list_models(exclude = c("nnetar", "hw", "arfima", "holt", "thetaf", "ses", "meanf", "splinef", "StructTS", "elm"))
#' modelos
#' errors <- rafa_compute_error(USAccDeaths, mod_fun = models)
#' errors
#' print(rafa_dir_acc(errors))
#'}

rafa_dir_acc <- function(data, mod_erro){
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
}
