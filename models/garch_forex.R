library(rugarch)
source("C:/Users/yusuf/OneDrive/Desktop/github_projects/forex_modeling/data/loading_data.R")

#using weekly_data
weekly_close <- weekly_data$close
weekly_log <- diff(log(weekly_close)) # input data for model fit
plot(weekly_close, type = "l", xlab = "closing price",
     ylab = "weeks since 2014-11-14", main = "USD/KWD")
plot(weekly_log, type = "l", xlab = "closing price",
     ylab = "weeks since 2014-11-14", main = "USD/KWD (log-normalized)")

#consult garch spec documentation
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

print(garch_spec)

#consult garch fit documentation
garch_fit <- ugarchfit(spec = garch_spec, data = weekly_log)
print(garch_fit)
plot(garch_fit, type = "l")


# extracting volatility
volatility <- sigma(garch_fit)
print(volatility)
#line plot(specified by type) of volatility
plot(volatility, type = "l")


#forecasting the volatility for the next year(52 weeks)
garch_forecast <- ugarchforecast(
  fitORspec = garch_fit,
  n.ahead = 52, # forecasts for a year ahead
  n.roll = 0, #read more on n.roll documentation(forecasting is only done once)
              # only one forecast is done (no rolling)
  out.sample = 52 # this would exclude the past year of data from/
  # being used for model fitting, and instead use it to evaluate the models/
  # forecasting based off of all of the data before that
)

print(garch_forecast)
plot(garch_forecast, type = "l")





