#re-rerun this script to get the most up-to-date exchange rate
#the rate is refreshed in UCT (universal coordinated time)


#load necessary package
library(alphavantager)

# Set your Alpha Vantage API key
# pick one of the two keys below depending on how many requests are/
# available on each key

api_key1 <- "________________"
av_api_key(api_key1)

#api_key2 <- "________________"
#av_api_key(api_key2)



# Getting the current exchange rate for USD and KWD
exchange_rate <- av_get(symbol = "USD/KWD", av_fun = "CURRENCY_EXCHANGE_RATE")


# open, close, high, low, rates for past 100 days
#daily_data_compact <- av_get(av_fun = "FX_DAILY", symbol = "USD/KWD", 
 #                            outputsize = "compact")

# open, close, high, low, rates since recording started
#daily_data_full <- av_get(av_fun = "FX_DAILY", symbol = "USD/KWD", 
#                          outputsize = "full")

#weekly data
weekly_data <- av_get(av_fun = "FX_WEEKLY", symbol = "USD/KWD")

#monthly data
#monthly_data <- av_get(av_fun = "FX_MONTHLY", symbol = "USD/KWD")












