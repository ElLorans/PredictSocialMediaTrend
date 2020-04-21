library(tidyverse)  # to convert date to date object (?)
library(forecast)
library(Metrics)

count_tw <- read.csv(
  "C:/Users/Lorenzo/Desktop/MyStuff/Master/Paris/Bloom/emerging_risks_doc_count_twitter.csv")


# hw_test = HoltWinters(demand)
# > hw_forecast = forecast(hw_test, h=36)
# > plot(hw_forecast)
# > arima_test = auto.arima(demand)
# > arima_forecast = forecast(arima_test, h=36)
# > plot(arima_forecast)
# > tbats_test = tbats(demand)
# > tbats_forecast = forecast(tbats_test, h=36)
# > plot(tbats_forecast)
# > nn_test = nnetar(demand)
# > nn_forecast = forecast(nn_test, h=36)
# > plot(nn_forecast)

h = ts(risks_count_insta, start=c(2018, 8, 13))
counts_insta <- ts(risks_count_insta$date, start=c(2018, 8, 13), end=c(2019, 8, 31), 
                       frequency=365)
hw = HoltWinters(counts_insta)




dane_modified <- risks_count_insta %>%
  mutate(date_index = as.Date(date))
dane_modified
hw = HoltWinters(dane_modified)



# auto arima
train = count_tw$Pesticides[1:150]
valid = count_tw$Pesticides[151:300]

# training model
model = auto.arima(train)

# model summary
summary(model)

# forecasting
forecast = predict(model,150)

# evaluation
rmse(valid, forecast$pred)

