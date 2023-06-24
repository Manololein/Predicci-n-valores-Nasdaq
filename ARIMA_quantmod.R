#                             ---Arima---                                   ####
#1. SETUP WORKING ENVIRONMENT####-----------------------------------------------
install.packages("quantmod")
install.packages("highcharter")

library(quantmod)       #--> assist explore and build trading models
library(highcharter)    #--> Interactive Plot
library(forecast)       #--> Predicciones

#Forecasting Time-Series Data with ARIMA
library(forecast) 


install.packages("gridExtra")
install.packages("ggplot2")
library(ggplot2)
library(gridExtra)



#2. LOAD DATASET####------------------------------------------------------------
oneq.base = read.csv(file.choose())


#Obtaining Stock Price
library(quantmod)
price_oneq = getSymbols("ONEQ", auto.assign=FALSE, from="2009-01-02", to="2019-12-29")
price_oneq_slim = getSymbols("ONEQ", auto.assign=FALSE, from="2009-01-02", to="2009-03-29")





#3. DATA PRE PROCESSING####-----------------------------------------------------
head(price_oneq)


#Stock Price Visualization
chartSeries(price_oneq, name = "ONEQ Price 2019-2023")
chartSeries(price_oneq_slim, name = "ONEQ Price 2009-2009")


lower_pr <- Lo(to.monthly(price_oneq))  # Take only the lower price monthly
plot(lower_pr, main = "ONEQ Low price monthly 2009-2018")
boxplot(lower_pr)


dc = decompose(as.ts(lower_pr, start=c(2009,1)))  # Decompose it's ts components
plot(dc)

dc$seasonal   #Seasonal component 



#Interactive Plot
highchart(type="stock") %>% 
  hc_add_series(price_oneq) %>% 
  hc_add_series(SMA(na.omit(Cl(price_oneq)),n=50),name="SMA(50)") %>% 
  hc_add_series(SMA(na.omit(Cl(price_oneq)),n=200),name="SMA(200)") %>% 
  hc_title(text="<b>ONEQ Price Candle Stick Chart 2009-2018</b>")









#3. MODELING####----------------------------------------------------------------

  #Splitting The Data
#n = 100

#oneq.lower
n = 2500   # number of period we want to forecast
train = head(price_oneq$ONEQ.Low, length(price_oneq$ONEQ.Low)-n)
test = tail(price_oneq$ONEQ.Low, n)

#oneq.high
train2 = head(price_oneq$ONEQ.High, length(price_oneq$ONEQ.High)-n)
test2 = tail(price_oneq$ONEQ.High, n)

#oneq.open
train3 = head(price_oneq$ONEQ.Open, length(price_oneq$ONEQ.Open)-n)
test3 = tail(price_oneq$ONEQ.Open, n)

#oneq.close
train4 = head(price_oneq$ONEQ.Close, length(price_oneq$ONEQ.Close)-n)
test4 = tail(price_oneq$ONEQ.Close, n)

  #Non-seasonal ARIMA

model_non = auto.arima(train, seasonal=FALSE)    # Create the Model
fc_non = forecast(model_non, h=n)                # Forecast n periods of the data

model_non2 = auto.arima(train2, seasonal=FALSE)    # Create the Model
fc_non2 = forecast(model_non2, h=n)                # Forecast n periods of the data

model_non3 = auto.arima(train3, seasonal=FALSE)    # Create the Model
fc_non3 = forecast(model_non3, h=n)                # Forecast n periods of the data

model_non4 = auto.arima(train4, seasonal=FALSE)    # Create the Model
fc_non4 = forecast(model_non4, h=n)                # Forecast n periods of the data





plot1 = autoplot(fc_non, main = "Forecasts from ARIMA LOW")+                                 # Plot the result
  autolayer(ts(test, start= length(train)), series="Test Data")


plot2 = autoplot(fc_non2, main = "Forecasts from ARIMA HIGH")+                                 # Plot the result
  autolayer(ts(test2, start= length(train2)), series="Test Data")

plot3 = autoplot(fc_non3, main = "Forecasts from ARIMA OPEN")+
  autolayer(ts(test3, start= length(train3)), series="Test Data")

plot4 = autoplot(fc_non4, main = "Forecasts from ARIMA CLOSE")+                                 # Plot the result
  autolayer(ts(test4, start= length(train4)), series="Test Data")


grid.arrange(plot1, plot2, plot3, plot4, nrow = 3)









  #Seasonal ARIMA
model_s <- auto.arima(train)                      # Create the Model
fc_s <- forecast(model_s, h=n)                    # Forecast n periods of the data
autoplot(fc_s, main = "Forecasts from ARIMA")+                                   # Plot the result
  autolayer(ts(test, start= length(train)), series="Test Data")









#4. RESULTADOS####--------------------------------------------------------------

summary(model_non)
summary(model_non2)
summary(model_non3)
summary(model_non4)


summary(model_s)
S

accuracy(fc_non)
accuracy(fc_non2)
accuracy(fc_non3)
accuracy(fc_non4)
