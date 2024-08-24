library(reticulate)
use_condaenv("C:/Users/yusuf/anaconda3/envs/r-env", required = TRUE)
source("C:/Users/yusuf/OneDrive/Desktop/github_projects/forex_modeling/data/loading_data.R")
library(keras3)
library(tensorflow)
library(tidyverse)
library(dplyr)
library(TTR)


#DATA

close_data <- weekly_data[, c("timestamp", "close")]
close_matrix <- as.matrix(weekly_data$close)



#FEATURES

#lagging the data by one year to identify trends that occur annually
close_data$annual_lag <- lag(close_data$close, 52)

#simple moving average - average over specified time periods
close_data$sma <- SMA(close_data$close, 52)#annual SMA (each value is the/ 
#average of the entire year leading up to it)

#exponential moving average - assigns exponentially decreasing weights to/
#                             past values, giving more weight to recent values
close_data$ema <- EMA(close_data$close, 52) #annual

#removing NA values from data
close_data <- na.omit(close_data)



#NORMALIZATION - min/max scaling
# scales everything to be between 0 and 1

normalizing <- function(x) {
  
  (x - min(x)) / (max(x) - min(x))
}

# applying normalization to features
close_data$close <- normalizing(close_data$close)
close_data$annual_lag <- normalizing(close_data$annual_lag)
close_data$sma <- normalizing(close_data$sma)
close_data$ema <- normalizing(close_data$ema)

#the columns of close_data are now normalized
#undo at the end for predictions

#creating keras-compatible data
no_time <- close_data[, c("close", "annual_lag", "sma", "ema")]
timeless_matrix <- as.matrix(no_time)




# CREATING SEQUENCES

create_sequences <- function(close_data, seq_length) {
  inputs <- lapply(seq_len(nrow(close_data) - seq_length), function(i) {
    close_data[i:(i + seq_length - 1), ]
  })
  targets <- lapply(seq_len(nrow(close_data) - seq_length), function(i) {
    close_data$close[i + seq_length]
  })
  list(inputs = array(unlist(inputs), dim = c(seq_length, ncol(close_data), length(inputs))), 
       targets = unlist(targets))
}

# Define sequence length
seq_length <- 52

# Create sequences
sequences <- create_sequences(close_data, seq_length)



# SPLIT TRAINING AND TEST SETS

train_size <- floor(0.8 * length(sequences$targets))
train_inputs <- sequences$inputs[ , , 1:train_size]
train_targets <- sequences$targets[1:train_size]
test_inputs <- sequences$inputs[ , , (train_size + 1):length(sequences$targets)]
test_targets <- sequences$targets[(train_size + 1):length(sequences$targets)]



# DEFINE THE LSTM MODEL
 l1 <-  layer_lstm(units = 50, return_sequences = TRUE) 
 l2 <-  layer_lstm(units = 50) 
 l3 <- layer_dense(units = 1)
 model <- keras_model_sequential(layers = list(l1, l2, l3), 
                                 input_shape = c(52, 5))
#model_shape <- tf$ones(shape(52, 4)) 


#compile the model
compile(model, loss = "mean_squared_error", optimizer = optimizer_adam())


train_inputs <- array_reshape(train_inputs, dim = c(320, 52, 5))
#train the model
fit(
  object = model,
  x = train_inputs,
  y = train_targets,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)




#MODEL EVALUATION
test_inputs <- array_reshape(test_inputs, c(80, 52, 5))
#generate predictions
predictions <- predict(model, test_inputs)
predictions


#performance metrics
rmse <- sqrt(mean((predictions - test_targets)^2))
print(paste("RMSE:", rmse))





#FORECASTING FUTURE VALUES
# Use the last sequence from the test set for forecasting
last_sequence <- test_inputs[ncol(test_inputs), , ]

# Predict the next value
forecast_array <- array(last_sequence, dim = c(52, seq_length, 
                                               ncol(close_data)))
forecast <- predict(model, forecast_array)
forecast

# Reverse normalization to get the actual value
# (Assuming you stored the min and max values used for normalization)
denormalize <- function(x, min, max) {
  x * (max - min) + min
}


forecast_actual <- denormalize(forecast, min(close_data$close), max(close_data$close))
print(forecast_actual)


#Visualize predictions
plot(x = 1:52, y = forecast_actual,
     xlab = "weeks since last closing price", ylab = "predicted closing price", 
     main = "predicted closing price each week since last recorded",
     type = "l")








