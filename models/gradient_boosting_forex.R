library(xgboost)
library(caret)
library(lubridate)
library(dplyr)
source("C:/Users/yusuf/OneDrive/Desktop/github_projects/forex_modeling/data/loading_data.R")

# using the closing prices for weekly data
close_data <- weekly_data[, c("timestamp", "close")]


#turns close_data into a data frame that includes the lagged values/
#of each closing price, for up to 4 weeks
close_data$lag1 <- lag(close_data$close, 1)
close_data$lag2 <- lag(close_data$close, 2)
close_data$lag3 <- lag(close_data$close, 3)
close_data$lag4 <- lag(close_data$close, 4)


#removing rows with NA values created by lagging
#effectively serves as the final data frame
close_data <- na.omit(close_data) #removed 4 weeks due to 4 lags


#splitting the data
row_num <- nrow(close_data)
train_data <- close_data[1:(row_num - 52),]
test_data <- close_data[(row_num - 52):row_num,]

#sets the training set to all the data up to one year before the present
train_matrix <- as.matrix(train_data[,c("close", "lag1", "lag2", "lag3", "lag4")])
                                      #addition of col names makes sure to
                                      #exclude the timestamp to make the
                                      #matrix readable to the Dmatrix object below
train_labels <- train_data$close #train data using close values
#test_data is the past year of data
test_matrix <- as.matrix(test_data[,c("close", "lag1", "lag2", "lag3", "lag4")])
test_labels <- test_data$close  #test data using close values



# all the data/features have been created
# now training the XGboost model



#5 TRAIN THE XGBOOST MODEL

#convert training data to Dmatrix format
dmatrix <- xgb.DMatrix(data = train_matrix, label = train_labels)

#set parameters for the model training & train it

xgb_fitting <- xgboost(
  
  #params contains the parameters for the booster itself
  params = list(booster = "gbtree", #using a decision-tree booster
                eta = 0.05, #the weight of every decision tree on the final/
                            #learner
                max_depth = 6, #max number of splits in each tree
                               #using default value
                subsample = 0.8, # % of rows to use for fitting/
                                 # each tree
                colsample_bytree = 0.1, #percentage of features(cols)/
                                      #to use for fitting each tree
                objective = "reg:squarederror"), 
  #these parameters below are general model-fitting parameters
  data = dmatrix,
  nrounds = nrow(train_data) / 2, #one boosting round(tree) for every two weeks
  verbose = 2    #prints the max amount of performance info
)

print(summary(xgb_fitting))

#6 EVALUATE THE MODEL ON THE TEST SET

#predict on the test set
predictions <- predict(object = xgb_fitting, newdata = test_matrix)
#not using dmatrix because that contains train data. predictions use/
#test data, and hence, we use test_matrix
print(predictions)

#calculate rmse for evaluation
rmse <- sqrt(mean((predictions - test_labels)^2)) 
                     #diff between values predicted by xgboost, and observed/
                     #values in the test set
cat("RMSE on the test set:", rmse, "\n")



#7 PREDICT FUTURE PRICES

#initialize a vector to store future predictions
future_pred <- numeric(52)
#use the last row of the test set as the starting point(only lagged features)
#lastrow <- as.numeric(test_data[nrow(test_data), c( "close","lag1","lag2","lag3","lag4")])
last <- nrow(test_data)
lastrow <- c(test_data$close[last], test_data$lag1[last],
                      test_data$lag2[last], test_data$lag3[last],
                      test_data$lag4[last])

#predict week-by-week for the next year
for (i in 1:52) {
  
  new_pred <- predict(object = xgb_fitting, newdata = matrix(lastrow, nrow = 1))
  future_pred[i] <- new_pred
#shift lagged features
  
  #lastrow <- c(new_pred, lastrow[1:2])
  lastrow <- c(new_pred, lastrow[1:(length(lastrow) - 1)])
  
}

#output the future predictions
print(future_pred)


#8 VISUALIZE THE PREDICTIONS
plot(test_data$timestamp, test_data$close, type = "l", col = "blue", ylim = range(c(test_data$close, predictions)), ylab = "Closing Price", xlab = "Date")
lines(test_data$timestamp, predictions, col = "red")


future_dates <- seq.Date(from = max(test_data$timestamp), by = "week", length.out = 52)
plot(future_dates, future_pred, type = "l", col = "green", 
     xlab = "dates", ylab = "close price")
lines(future_dates, future_pred, col = "green")



