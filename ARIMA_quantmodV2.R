#                             ---Arima---                                   ####
#1. SETUP WORKING ENVIRONMENT####-----------------------------------------------
install.packages("quantmod")
install.packages("highcharter")
install.packages("gridExtra")
install.packages("ggplot2")

library(base)
library(quantmod)       #--> assist explore and build trading models
library(highcharter)    #--> Interactive Plot
library(forecast)       #--> Predicciones

#Forecasting Time-Series Data with ARIMA
library(forecast) 
library(ggplot2)
library(gridExtra)



#2. LOAD DATASET####------------------------------------------------------------


#Obtaining Stock Price
price_oneq = getSymbols("ONEQ", auto.assign=FALSE, from="1999-12-29", to="2023-12-29")
price_oneq = getSymbols("ONEQ", auto.assign=FALSE, from="2009-01-02", to="2018-12-29")
price_oneq = getSymbols("ONEQ", auto.assign=FALSE, from="2019-01-02", to="2023-12-29")





#3. DATA PRE PROCESSING####-----------------------------------------------------
head(price_oneq)


#Stock Price Visualization
chartSeries(price_oneq)
plot(price_oneq)
boxplot(price_oneq$ONEQ.Close)

dc_price_oneq <- as.ts(Lo(price_oneq))

dc = decompose(dc_price_oneq)


dc$seasonal   #Seasonal component 



#Interactive Plot
highchart(type="stock") %>% 
  hc_add_series(price_oneq) %>% 
  hc_add_series(SMA(na.omit(Cl(price_oneq)),n=50),name="SMA(50)") %>% 
  hc_add_series(SMA(na.omit(Cl(price_oneq)),n=200),name="SMA(200)") %>% 
  hc_title(text="<b>ONEQ Price Candle Stick Chart 2009-2018</b>")









#4. MODELING####----------------------------------------------------------------

  #Splitting The Data
#n = 100
n = as.integer(nrow(price_oneq)/3)   # number of period we want to forecast

#oneq.lower
train = head(price_oneq$ONEQ.Low, length(price_oneq$ONEQ.Low)-n)
test = tail(price_oneq$ONEQ.Low, n)
model = auto.arima(train)    # Create the Model
fc = forecast(model, h=n)                # Forecast n periods of the data
plot1 = autoplot(fc, main = "Forecasts from ONEQ price LOW")+                                 # Plot the result
  autolayer(ts(test, start= length(train)), series="Test Data")


#oneq.high
train2 = head(price_oneq$ONEQ.High, length(price_oneq$ONEQ.High)-n)
test2 = tail(price_oneq$ONEQ.High, n)
model2 = auto.arima(train2)    # Create the Model
fc2 = forecast(model2, h=n)                # Forecast n periods of the data
plot2 = autoplot(fc2, main = "Forecasts from ONEQ price HIGH")+                                 # Plot the result
  autolayer(ts(test2, start= length(train2)), series="Test Data")


#oneq.open
train3 = head(price_oneq$ONEQ.Open, length(price_oneq$ONEQ.Open)-n)
test3 = tail(price_oneq$ONEQ.Open, n)
model3 = auto.arima(train3)    # Create the Model
fc3 = forecast(model3, h=n)                # Forecast n periods of the data
plot3 = autoplot(fc3, main = "Forecasts from ONEQ price OPEN")+
  autolayer(ts(test3, start= length(train3)), series="Test Data")


#oneq.close
train4 = head(price_oneq$ONEQ.Close, length(price_oneq$ONEQ.Close)-n)
test4 = tail(price_oneq$ONEQ.Close, n)
model4 = auto.arima(train4)    # Create the Model
fc4 = forecast(model4, h=n)                # Forecast n periods of the data
plot4 = autoplot(fc4, main = "Forecasts from ONEQ price CLOSE")+                                 # Plot the result
  autolayer(ts(test4, start= length(train4)), series="Test Data")


grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)


#5. RESULTADOS####--------------------------------------------------------------
me_low <- accuracy(fc, test)["Test set", "ME"]
mpe_low <- accuracy(fc, test)["Test set", "MPE"]
rmse_low <- accuracy(fc, test)["Test set", "RMSE"]

me_high <- accuracy(fc2, test2)["Test set", "ME"]
mpe_high <- accuracy(fc2, test2)["Test set", "MPE"]
rmse_high <- accuracy(fc_non2, test2)["Test set", "RMSE"]

me_open <- accuracy(fc3, test3)["Test set", "ME"]
mpe_open <- accuracy(fc3, test3)["Test set", "MPE"]
rmse_open <- accuracy(fc3, test3)["Test set", "RMSE"]

me_close <- accuracy(fc4, test4)["Test set", "ME"]
mpe_close <- accuracy(fc4, test4)["Test set", "MPE"]
rmse_close <- accuracy(fc4, test4)["Test set", "RMSE"]

results <- data.frame(Test = c("low", "high", "open", "close"), 
                      ME = c(me_low, me_high, me_open, me_close), 
                      MPE = c(mpe_low, mpe_high, mpe_open, mpe_close),
                      RMSE = c(rmse_low, rmse_high, rmse_open, rmse_close))

colnames(results) <- c("Test", "Mean Error", "% Mean Error", "Root Mean Squared Error")

results
