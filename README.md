# rafa - R Automatic Forecasting Algorithm

Installing rafa beta:

```r

library(devtools)

devtools::install_github("leripio/rafa")

library(rafa)

```


This project is intended to develop an automatic forecasting algorithm in R language with robust specification and confidence intervals calculations. The main goal of this algorithm is to provide the best possible forecast for a time series based on univariate models to serve either as a benchmark for variables which we have a reference model or the reference itself when we do not have a reference model. 

More specifically, it relies heavily on the R forecast package by Rob Hyndman and it is divided in 4 steps:

1. Fit several univariate models to the time series sample.
2. Calculate forecast errors for each model in (1) through cross-validation and compute accuracy statistics (RMSE, MAE, etc).
3. Select the best model based on (2) and compute forecasts on the bootstrapped versions of the time series (bagging).
4. Compute mean and confidence intervals.

New features:

1. Output now contains a data frame with directional accuracy.
2. Directional accuracy may be used as a selection criterion. 
3. It is now possible to exclude some models from evaluation.
4. It is now possible to choose between cross-validation and train/test set approaches to compute forecast errors.

Things to improve/include:

1. When choosing the train/test set approach to compute accuracy, return the post- rather than the pre-bagging measures.
2. Merge all the accuracy measures in a single data fram (done)

Future developments:

I intend to incorporate the "rectify" strategy proposed by Hyndman & Taieb (2012) so as the prediction errors can be used to further improve forecasts' accuracy.

Acknowlegment: This tool has been developed at Codeplan <www.codeplan.df.gov.br> as an effort to obtain reliable and timely forecasts from economic variables. However, it is important to highlight that it serves only as a guide to economic analysis and should not be taken as an official tool neither from Codeplan nor from Distrito Federal Government.

