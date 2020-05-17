library(forecast)


dc_tw <- read.csv("C:/Users/Lorenzo/Desktop/MyStuff/Master/Paris/Bloom/data/emerging_risks_doc_count_twitter.csv")
dc_tw$date= as.Date(as.character(dc_tw$date), format = "%Y-%m-%d")
rownames(dc_tw) = dc_tw$date
dc_tw <- dc_tw[1:381, ] # last three dates are 0, so remove them

train = dc_tw$Pesticides[1:200]
test = dc_tw$Pesticides[201:length(dc_tw$Pesticides)]

plot(train)
lines(test)

best_ma = sma(train, h=14, silent=FALSE)    # get best moving average parameter

acf(best_ma$fitted, lag.max=34)
pacf(best_ma$fitted, lag.max=34)

ar = Arima(best_ma$fitted, order=c(4,0,9))
plot(forecast(ar))
lines(ar$fitted, col="red")
lines(dc_tw$Pesticides[201:length(dc_tw$Pesticides)], col="orange")
rmse(ar$fitted, )

automatic = auto.arima(best_ma$fitted, seasonal=TRUE, stepwise=FALSE, 
                       approximation=FALSE)


plot(forecast(automatic)) # plots original data and forecast
lines(automatic$fitted, col="red")   # plot model
