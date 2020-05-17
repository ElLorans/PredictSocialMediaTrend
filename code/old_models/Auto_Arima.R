library(data.table)
library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)
library(Metrics)
library(ggplot2)

engage_twitter <- read.csv("C:/Users/Lorenzo/Desktop/MyStuff/Master/Paris/Bloom/data/emerging_risks_doc_count_twitter.csv")
engage_twitter$date= as.Date(as.character(engage_twitter$date), format = "%Y-%m-%d")

engage_twitter <- engage_twitter[1:381, ] # last three dates are 0, so remove them

arima_predict <- function(X, train_size, only_error=FALSE){
  # find best arima and return dataframe with test and predictions columns
  # if only_error is set to TRUE, only the MAPE is returned.
    size <- round(length(X)*train_size)
    train <- X[1: size]
    test <- X[size:length(X)]
    history <- train
    predictions = c()
    
    for (ind in 1:length(test))
    {
      model <- auto.arima(history, seasonal=TRUE)
      output <- forecast(model)
      yhat <- output$mean[1]
      predictions <- append(predictions, yhat)
      obs <- test[ind]
      history <- append(history, obs)
    }
    
    # mean_squared_error = mse(test, predictions)
    mean_percentage_error <- mape(predictions, test) # error as percentage is more relevant
    if (only_error == FALSE)
      {
      print("MAPE:")
      print(mean_percentage_error)
      final <- data.frame(cbind(test, predictions))
      return(final)}
    
    return(mean_percentage_error)
  }


plot_arima_predict <- function(df)
  # plot df, a dataframe with test and predictions column
  {
    mean_percentage_error <- mape(df$predictions, df$test) # error as percentage is more relevant
    print(mean_percentage_error)
    
    p <- ggplot() +
      geom_line(data=df, aes(x = as.numeric(row.names(df)), y = df$test), color="blue") + 
      geom_line(data=df, aes(x = as.numeric(row.names(df)), y = df$predictions), color="red")
    print(p)
    #plot(df$test, type="l")
    #lines(df$test)
    #lines(df$predictions,col="red")
  }


arima_predict_result <- arima_predict(engage_twitter$Pesticides, 0.5)
plot_arima_predict(arima_predict_result)
#write.csv(arima_predict_result, "PesticidesPrediction.csv")

# plot confidence interval
engage_twitter$Pesticides %>%
  auto.arima() %>%
  forecast(h=10) %>%
  autoplot

# get MAPE for each column
mapes <- c()
for (i in colnames(engage_twitter)){
  if (i!='date'){
    new <- arima_predict(engage_twitter[[i]], 0.66, only_error=TRUE)
    mapes <- append(mapes, new)
    print(i)
    print(new)
  }
}

precise = auto.arima(engage_twitter$Pesticides, seasonal=FALSE, stepwise=FALSE, 
                     approximation=FALSE)
summary(precise)

simple_arima = arima(engage_twitter$Pesticides, order=c(1, 1, 1))
summary(simple_arima)

plot(precise$x,col="blue")
lines(fitted(precise),col="red")
legend("topleft",
       c("Pesticides","Auto ARIMA"),
       fill=c("blue","red")
)

plot(precise$x,col="blue")
lines(fitted(precise),col="red")
legend("topleft",
       c("Pesticides","Auto ARIMA"),
       fill=c("blue","red")
)


plot(engage_twitter$Pesticides, col="blue", type="l")
lines(fitted(simple_arima),col="red")

library(smooth)
ma = sma(engage_twitter$Pesticides, h=14, silent=FALSE)
perfect = auto.arima(ma$fitted, seasonal=TRUE, stepwise=FALSE, 
           approximation=FALSE)
