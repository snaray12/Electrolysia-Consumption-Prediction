rfmodel <- function(train, test) {
  library(randomForest)
  
  fit <- randomForest(electricity_consumption~.-ID, 
                      data=train, 
                      ntree=30,
                      mtry=10)
  
  summary(fit)
  
  predicted <- predict(fit, newdata = test)
  
  RMSE.baseline <- sqrt(mean((predicted-test$electricity_consumption)^2))
  
  print(paste("RMSE",RMSE.baseline))
  
  MAE.baseline <- mean(abs(predicted-test$electricity_consumption))
  print(paste("MAE",MAE.baseline))
  # bestmtry <- tuneRF(train, train$electricity_consumption, 
  #                   ntreeTry =20, stepFactor = 0.9, 
  #                   improve = 0.01, trace = T, plot = T)
  # print(bestmtry)
  return(fit)
}