library(tidyverse)
library(fpp3)
library(BVAR)

### Part 2: Horserace

#Read in Data 
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double())
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))
has_gaps(liquor)

#Plots
liquor |>
  autoplot(sales) +
  labs(
    title= "Liquor Sales in Millions of Dollars",
    x= "Month",
    y= "Sales"
  ) +
  theme_minimal()
liquor |>
  gg_season(log(sales)) +
  labs(
    title= "Seasonal Plot of Log Sales",
    x= "Month",
    y= "Log Sales") +
  theme_minimal()
#Liquor sales have a strong seasonal and trend pattern with peaks in 
#December and troughs in January. The seasonal swings grow with the level
#suggesting that a log transformation is appropriate. 

#Univariate Models
liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()
#An ARIMA(1,0,2)(2,1,1) model and a ETS(A,A,A) model were chosen. The ARIMA
#model is seasonally differenced once and the ETS model has additive components 
#for its trend, seasonality, and error. 

liquor_fit |> select(arima) |> gg_tsresiduals()
liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)
#With a p-value of 0 and the acf graph it can be determined that the residuals
#aren't white noise, ie there is still sturctule left to be captured in the data

#Horse Race
liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |>
  model(snaive = SNAIVE(sales),
        ets    = ETS(log(sales)),
        arima  = ARIMA(log(sales)))

saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

