#library('ggplot2')
#library('forecast')
#library('tseries')

source(file = './electronavia_factor_engineering.R')

#source(file = './electronavia_lm.R')

source(file = './electronavia_rf.R')

#source(file = './electronavia_nnet.R')

#source(file = './electronavia_arima.R')

daily_data <- read.csv('./01_Datasets/11_av/electricity-consumption/train.csv',
                 stringsAsFactors = F,
                 header = T)

daily_data <- factor_engineering(daily_data)

set.seed(1234)
indexes = sample(1:nrow(daily_data), 
                 size=0.2*nrow(daily_data))
validation<- daily_data[indexes,]
train<- daily_data[-indexes,]

#linearmodel(train)
fit <- rfmodel(train, validation)
#fit <- nnmodel(train, validation)

## ------ ##


test <- read.csv('01_Datasets/11_av/electricity-consumption/test.csv',
                 stringsAsFactors = F,
                 header = T)

test <- factor_engineering(test)

test$electricity_consumption <- predict(fit, newdata = test)

write.csv(test[,c("ID", "electricity_consumption")], 
          file='01_Datasets/11_av/electricity-consumption/submission.csv',
          row.names = F)
