linearmodel <- function(train) {
  null.model <- lm(data=train, 
                   electricity_consumption~1)
  
  full.model <- lm(data=train[,-c(1,2)], 
                   electricity_consumption~.)
  
  
  step.model <- step(data=train[,-c(1,2)], 
                     full.model, 
                     direction = "forward")
  
}