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

qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))

growth <- function(x) 100 * difference(log(x))
qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()
nrow(y)
head(y)

N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]
#There are 120 forecasts produced for each variable the first forecast will be
#1990 Q1 and the last is 2019 Q4.

var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))
rmse_by_var(fc_var1)
rmse_by_var(fc_var4)
#The smaller VAR forecasts better for all variables except inflation. The smaller
#model forecasts better because it has a much lower estimation error. The larger
#VAR has a large number of variables, increasing the estimation error such that
#it overshoots the potential approximation error of using a smaller model.

bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
    fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
  }
  fc
}
set.seed(422)
fc_bvar_03 <- bvar_cv(0.3)
saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")

set.seed(422)
fc_bvar_005 <- bvar_cv(0.05)

var_results <- tibble(
  Variable = colnames(y),
  `VAR(1)` = rmse_by_var(fc_var1),
  `VAR(4)` = rmse_by_var(fc_var4),
  `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
)

saveRDS(var_results, "results/var_results.rds")

var_results |> knitr::kable(digits = 3)
#The BVAR(4) model with lambda = 0.3 performs better than the VAR(4), this is 
#because the BVAR model shrinks the coefficients towards zero which helps clear
#some of the estimation error that is present in the VAR(4) model. Compared to
#the VAR(1) model, the BVAR model does better at modeling investment, inflation,
#and the 10-year treasury yield, but does worse at the other variables. 

#When lambda is set at 0.05 the model performs worse, indicating that there is
#too much shrinkage and the model is not able to capture the dynamics of the 
#data. QUESTION

e_var4_gdp <- actual[, "gdp"] - fc_var4[, 1]
e_bvar_gdp <- actual[, "gdp"] - fc_bvar_03[, 1]

forecast::dm.test(e_var4_gdp, e_bvar_gdp, h = 1, power = 2)

e_var4_infl <- actual[, "infl"] - fc_var4[, 5]
e_bvar_infl <- actual[, "infl"] - fc_bvar_03[, 5]

forecast::dm.test(e_var4_infl, e_bvar_infl, h = 1, power = 2)

#QUESTION

### Factor Model 





