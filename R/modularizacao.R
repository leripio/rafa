library(rafa)

#Funcoes Rafa
rafa_list_functions <- function(exclude = NULL){
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
                  "StructTS" = function(x,h) forecast::forecast(stats::StructTS(x), h= h),
                  "elm" = function(x,h) forecast::forecast(nnfor::elm(x), h=h))

  # Allow the exclusion of specific models
  if(!is.null(exclude)){
    for(i in seq_along(exclude))
    {mod_fun[[exclude[[i]]]] <- NULL}
  }
  mod_fun
}

# Compute forecast errors via cross-validation or train/test sets
rafa_compute_error <- function(data, test, h_cv){
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

# Compute the directional accuracy of models
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

# Compute MAE and RMSE
rafa_mae_rmse <- function(mod_erro){
  mae <- function(x){mean(abs(x), na.rm = TRUE)}
  rmse <- function(x){sqrt(mean(x^2, na.rm = TRUE))}
  mod_acc <- dplyr::tibble("Model" = names(mod_erro),
                           "MAE" = purrr::map_dbl(.x = mod_erro, .f = mae),
                           "RMSE" = purrr::map_dbl(.x = mod_erro, .f = rmse))
}

#Testes
my_fc <- auto_forecast(USAccDeaths, test = c(1976,12))
mod_fun <- rafa_lista_funcoes()
mod_erro <- rafa_compute_error(USAccDeaths, test = c(1976,12), h_cv = 1)
x <- rafa_acc_dir(USAccDeaths, mod_erro)
y <- rafa_mae_rmse(mod_erro = mod_erro)
y %>% arrange(MAE)
my_fc$acc

