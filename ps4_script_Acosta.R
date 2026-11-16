library(tidyverse)
library(fpp3)
library(BVAR)
library(knitr)

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

#liquor_cv_fit <- liquor_cv |>
#  model(snaive = SNAIVE(sales),
#        ets    = ETS(log(sales)),
#        arima  = ARIMA(log(sales)))

#saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

liquor_cv_fit <- readRDS("results/liquor_cv_fit.rds")
#There are 216 different forecast origins in the TS-CV.

liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |>
  kable(digits = 2)
#The first forecast is for January 2002, and the last is December of 2019. The 
#ARIMA model has the lowest RMSE and MAE values, suggesting that it is the best 
#model for forecasting liquor sales.

#DM Test
liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

t1 <- forecast::dm.test(liquor_errs$arima, liquor_errs$ets,    h = 1, power = 2)
t2 <- forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)

kable(
  data.frame(
    Comparison   = c("ARIMA vs ETS", "ARIMA vs SNAIVE"),
    `DM statistic` = c(t1$statistic, t2$statistic),
    `p-value`      = c(t1$p.value,   t2$p.value),
    check.names = FALSE
  ),
  digits = 3
)
#From the results of the DM test, which show that the ARIMA model does not
#have equal forecast accuracy to the ETS and SNAIVE models, it can be concluded
#that the ARIMA model is best for forecasting liquor sales. 


### Forecast Combination 

liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |>
  kable(digits = 2)
#The combo model does worse than the base ARIMA but performs better than the
#ETS and snaive models. 

cor(liquor_errs$arima, liquor_errs$ets)
#The correlation between the ARIMA and ETS forecast errors is 0.88, 
#which is a strong positive correlation. This suggests that the two models are 
#capturing similar patterns in the data, and combining them may not provide 
#significant improvements in forecast accuracy.


### VAR and BVAR




