elecytricity_consumption_ts <- ts(daily_data[,"electricity_consumption"] )

daily_data$clean_electricity_consumption = tsclean(elecytricity_consumption_ts)

ggplot() +
  geom_line(data = daily_data, 
            aes(x = datetime, 
                y = clean_electricity_consumption)) + 
  ylab('Cleaned Electricity consumption')

daily_data$cnt_ma = ma(daily_data$clean_electricity_consumption, order=24) # using the clean count with no outliers


ggplot() +
  geom_line(data = daily_data, aes(x = datetime, y = clean_electricity_consumption, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = datetime, y = cnt_ma,   colour = "Daily Moving Average"))  +
  ylab('Electricity consumption')

count_ma = ts(na.omit(daily_data$cnt_ma), frequency=24)

decomp = stl(count_ma, s.window="periodic")

deseasonal_cnt <- seasadj(decomp)

plot(decomp)

adf.test(count_ma, alternative = "stationary")


Acf(count_ma, main='')


Pacf(count_ma, main='')


count_d1 = diff(deseasonal_cnt, differences = 1)

plot(count_d1)

adf.test(count_d1, alternative = "stationary")
Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')


auto.arima(deseasonal_cnt, seasonal=FALSE)

fit<-auto.arima(deseasonal_cnt, seasonal=F)
tsdisplay(residuals(fit), lag.max=45, main='(2,0,1) Model Residuals')


fit2 = arima(deseasonal_cnt, order=c(2,0,1))

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

fcast <- forecast(fit2, h=48)

plot(fcast)
