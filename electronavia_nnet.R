nnmodel <- function(train, test) {
  library(neuralnet)
  
#  allVars <-colnames(train)
#  predictorVars<-allVars[!allVars%in%"electricity_consumption"]
  
#  predictorVars <- c("temperature", "var1", "pressure", "windspeed", "var2", "temp_pres_var1_ws")
  
#  predictorVars<-paste(predictorVars,collapse="+")
#  form<-as.formula(paste("electricity_consumption"~predictorVars,collapse="+"))
  
  m <- model.matrix(~ electricity_consumption + temperature + var1 + pressure + windspeed + var2 + temp_pres_var1_ws, data=train)
  
  form <- as.formula("electricity_consumption ~ temperature + var1 + pressure + windspeed + var2B + var2C + temp_pres_var1_ws")
  
  
  
  neuralModel<-neuralnet(formula=form,
                         hidden=c(3),
                         linear.output=T,
                         data=m)
  
  plot(neuralModel)
  
  m1 <- model.matrix(~ temperature + var1 + pressure + windspeed + var2 + temp_pres_var1_ws, data=test)
  
  predicted <- compute(neuralModel, m1[,-8])
  print(predicted)
  
  RMSE.baseline <- sqrt(mean((predicted-test$electricity_consumption)^2))
  
  print(paste("RMSE",RMSE.baseline))
  
  MAE.baseline <- mean(abs(predicted-test$electricity_consumption))
  print(paste("MAE",MAE.baseline))
  
  return(neuralModel)
}