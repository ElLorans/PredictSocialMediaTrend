library(zoo)
library(forecast)
#library(Metrics)


doc_twitter <- read.csv("C:/Users/Lorenzo/Desktop/MyStuff/Master/Paris/Bloom/data/emerging_risks_doc_count_twitter.csv")
doc_twitter$date= as.Date(as.character(doc_twitter$date), format = "%Y-%m-%d")
doc_twitter=doc_twitter[1:381,]
rownames(doc_twitter) <- doc_twitter$date
doc_twitter=doc_twitter[,-1]


weekly_mean=as.data.frame(rollmean(doc_twitter, k = 7))
#smoothed = as.data.frame(rollmean(doc_twitter, k = 9))

#df = doc_twitter
df = weekly_mean

mean_absolute_percentage_error=function(y_true, y_pred)
{
  if(length(y_true)!=length(y_pred))
  {
    print(paste0("len y_true is"+ length(y_true) + "and len y_pred is" + length(y_pred)))
    return (NULL)
  }

  return(mean(abs((y_true - y_pred) / y_true)) * 100) 
}


arima_models = list()
horizon = 10
forecasts = data.frame(seq(1:horizon))
for(i in colnames(df))
{
  train = df[1:200,][[i]]
 
  #best_ma = sma(train, h=14)$fitted      # get best moving average parameter DOES NOT WORK??
  #model = auto.arima(best_ma)
  
  model = auto.arima(train, seasonal = FALSE) #, stepwise=FALSE, approximation=FALSE) # USING THIS ACTUALLY LOWERS AVG MAPE by 6%. HOW???
  
  # model <- tryCatch(
  #   {
  #     Arima(train, order=c(7, 0, 4))
  #   },
  #   error=function(cond) {
  #     print(cond)
  #     print(paste0("Switching to auto.arima for ", i))
  #     auto.arima(train)
  #   }
  # )
  
  arima_models = append(arima_models, list(model))
  
  predictions = forecast(model, horizon)
  forecasts = cbind(forecasts, as.data.frame(predictions$mean))
}

#arima_models = arima_models[,-1]
arima_models = setNames(arima_models, names(df))

forecasts = forecasts[,-1]
forecasts = setNames(forecasts, names(doc_twitter))

rmses = c()

for(i in colnames(forecasts))
{
  test = weekly_mean[201:210,][[i]]
  rms=rmse(forecasts[[i]],test)
  rmses=rbind(rmses,rms)
}

mapes=c()
for(i in colnames(forecasts))
{
  real = df[[i]][201:202]
  mape_r = mean_absolute_percentage_error(real, forecasts[[i]][1:2])
  if (!is.infinite(mape_r)) {
    mapes = rbind(mapes,mape_r)
  }
}

print(mean(mapes))
#mapes = setNames(mapes, names(doc_twitter))
