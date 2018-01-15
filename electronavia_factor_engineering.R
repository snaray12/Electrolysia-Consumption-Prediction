factor_engineering <- function(data) {
  library(lubridate)
  
  data$datetime <- as.POSIXct(data$datetime)
  
  data$var2 <- as.factor(data$var2)
  
  data$temp_pres_var1 <- (data$temperature/data$pressure)*data$var1
  #daily_data$temp_var1 <- daily_data$temperature/daily_data$var1
  
  data$months <- months(data$datetime)
  data$weekdays <- weekdays(data$datetime)
  
  
  breaks <- c(0, 12, 18, 24)
  
  labels <- c("morning", "daylight", "evening")
  
  data$ind <- cut(hour(data$datetime), breaks, labels, include.lowest = TRUE)
  
  factor.vars <- c("months", "weekdays", "ind")
  
  for(v in factor.vars) {
    data[,v] <- as.factor(data[,v])
  }
  
#  data$ind <- as.factor(data$ind)
  
  return(data)
}