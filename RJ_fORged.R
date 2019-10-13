setwd("...")

######Packages checking, installing, and librarying#####
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "xgboost", #for random forest
  "forecast"
)

#####The function for removing variables#####
ProcessRemoveVars <- function(DF, drops){
  return(DF[ , !(names(DF) %in% drops)])
}


#####Imprting and preparing the historical data (year 1-10)#####
data<-read.csv("Ten-Year-Demand.csv")
names(data) <- c("year", "month", "demand")
n = length(data$demand)
for (i in 1:n){
  if ((i-1)%%12 != 0){
    data$year[i] = data$year[i-(i-1)%%12]
  }
}
data$month12 = NA
for (i in 1:n){
  data$month12[i] = i%%12
  if(i%%12 == 0){
    data$month12[i] = 12
  }
}


data$demand12 = NA
data$demand11 = NA
data$demand10 = NA
data$demand9 = NA
data$demand8 = NA
data$demand7 = NA
data$demand6 = NA
data$demand5 = NA
data$demand4 = NA
data$demand3 = NA
data$demand2 = NA
data$demand1 = NA
data$demand24 = NA
data$demand23 = NA
data$demand22 = NA
data$demand21 = NA
data$demand20 = NA
data$demand19 = NA
data$demand18 = NA
data$demand17 = NA
data$demand16 = NA
data$demand15 = NA
data$demand14 = NA
data$demand13 = NA


for (i in 25:n){
  data$demand12[i] = data$demand[i-12]
  data$demand11[i] = data$demand[i-11] 
  data$demand10[i] = data$demand[i-10]
  data$demand9[i] = data$demand[i-9]
  data$demand8[i] = data$demand[i-8]
  data$demand7[i] = data$demand[i-7]
  data$demand6[i] = data$demand[i-6]
  data$demand5[i] = data$demand[i-5]
  data$demand4[i] = data$demand[i-4]
  data$demand3[i] = data$demand[i-3]
  data$demand2[i] = data$demand[i-2]
  data$demand1[i] = data$demand[i-1]
  data$demand24[i] = data$demand[i-24]
  data$demand23[i] = data$demand[i-23] 
  data$demand22[i] = data$demand[i-22]
  data$demand21[i] = data$demand[i-21]
  data$demand20[i] = data$demand[i-20]
  data$demand19[i] = data$demand[i-19]
  data$demand18[i] = data$demand[i-18]
  data$demand17[i] = data$demand[i-17]
  data$demand16[i] = data$demand[i-16]
  data$demand15[i] = data$demand[i-15]
  data$demand14[i] = data$demand[i-14]
  data$demand13[i] = data$demand[i-13]
}


##################Building the XGBoost model################## 
train <- ProcessRemoveVars(data, c('month'))
train.y <- train$demand[25:n]
train.y = as.matrix(train.y)
train.x <- ProcessRemoveVars(train, c('demand'))
train.x = train.x[25:n,]
train.x =as.matrix(train.x)
set.seed(100)
xgb <- xgboost(data = train.x, label = train.y, max.depth=5, eta=0.05, subsample=0.5, nrounds=500, verbose = FALSE)
train.pred.y.xgb = predict(xgb, train.x)
error.xgb = train.pred.y.xgb - train.y



##################Building the ARIMA model##################
dataTrainOriginal<-read.csv("Ten-Year-Demand.csv")
names(dataTrainOriginal) <- c("year", "month", "demand") 
tsDataTrain<-ts(dataTrainOriginal$demand, start=c(1996, 1), end=c(2005, 12), frequency=12)
arima.ts<- auto.arima(tsDataTrain) 
ErrorTerms<-data.frame(error=fitted(arima.ts)-tsDataTrain)



######Importing and preparing the test data (year 11-12)#####
test <- read.csv("Two-Year-Demand.csv")
names(test) <- c("year", "month", "demand")
m = length(test$demand)
for (i in 1:m){
  if ((i-1)%%12 != 0){
    test$year[i] = test$year[i-(i-1)%%12]
  }
}
test$month12 = NA
for (i in 1:m){
  test$month12[i] = i%%12
  if(i%%12 == 0){
    test$month12[i] = 12
  }
}

test$demand12 = NA
test$demand11 = NA
test$demand10 = NA
test$demand9 = NA
test$demand8 = NA
test$demand7 = NA
test$demand6 = NA
test$demand5 = NA
test$demand4 = NA
test$demand3 = NA
test$demand2 = NA
test$demand1 = NA
test$demand24 = NA
test$demand23 = NA
test$demand22 = NA
test$demand21 = NA
test$demand20 = NA
test$demand19 = NA
test$demand18 = NA
test$demand17 = NA
test$demand16 = NA
test$demand15 = NA
test$demand14 = NA
test$demand13 = NA

data = rbind(data, test)



##################Building the prediction model by combining ARIMA and XGBoost##################
pred_xgb = rep(0,m)
pred_arima = rep(0,m)
pred.mixed = rep(0,m)
order_xgb = rep(0,m)
order_arima = rep(0,m)
order.mixed = rep(0,m)

InitialInvantory=rep(0,m+1)
EndInventory=rep(0,m)
InitialInvantory[1]=73


initial_inventory = 73
C1_arima = 0.3
C2_arima = -1.8
C1_xgb = -0.57
C2_xgb = 3.38
cost_arima = 0
cost_xgb = 0
cost.mixed = 0

for (i in 1:m){
  #predict the demand for month 120+i by xgb
  data$demand12[n+i] = data$demand[n+i-12]
  data$demand11[n+i] = data$demand[n+i-11] 
  data$demand10[n+i] = data$demand[n+i-10]
  data$demand9[n+i] = data$demand[n+i-9]
  data$demand8[n+i] = data$demand[n+i-8]
  data$demand7[n+i] = data$demand[n+i-7]
  data$demand6[n+i] = data$demand[n+i-6]
  data$demand5[n+i] = data$demand[n+i-5]
  data$demand4[n+i] = data$demand[n+i-4]
  data$demand3[n+i] = data$demand[n+i-3]
  data$demand2[n+i] = data$demand[n+i-2]
  data$demand1[n+i] = data$demand[n+i-1]
  data$demand24[n+i] = data$demand[n+i-24]
  data$demand23[n+i] = data$demand[n+i-23] 
  data$demand22[n+i] = data$demand[n+i-22]
  data$demand21[n+i] = data$demand[n+i-21]
  data$demand20[n+i] = data$demand[n+i-20]
  data$demand19[n+i] = data$demand[n+i-19]
  data$demand18[n+i] = data$demand[n+i-18]
  data$demand17[n+i] = data$demand[n+i-17]
  data$demand16[n+i] = data$demand[n+i-16]
  data$demand15[n+i] = data$demand[n+i-15]
  data$demand14[n+i] = data$demand[n+i-14]
  data$demand13[n+i] = data$demand[n+i-13]
  test.x = data[n+i,]
  test.y = data$demand[n+i]
  test.x <- ProcessRemoveVars(test.x, c('month', 'demand'))
  test.x = as.matrix(test.x)
  pred_xgb[i] = predict(xgb, test.x)
  
  #determine order quantity by xgb
  if(pred_xgb[i] - initial_inventory>0){
    order_xgb[i] = pred_xgb[i] - initial_inventory + C1_xgb*sd(data$demand[(n+i-13):(n+i-1)]) + C2_xgb*sd(error.xgb[(n+i-13-24):(n+i-1-24)])
  }
  error.xgb = rbind(error.xgb, pred_xgb[i]-test.y)
  
  if(i==1){
    #determine the order quantity (for the month 121, we use xgboost)
    order.mixed[i] = order_xgb[i]
    pred.mixed[i] = pred_xgb[i]
    
    #predict the demand for month 120+i by auto.arima
    pred_arima[i] = forecast(arima.ts, h=1, level=0)$mean
    #determine order quantity by arima
    if(pred_arima[i] - initial_inventory>0){
      order_arima[i] = pred_arima[i] - initial_inventory + C1_arima*sd(data$demand[(n+i-13):(n+i-1)]) + C2_arima*sd(ErrorTerms$error[(n+i-13):(n+i-1)])
    }
    #cost of arima just for month 120+i
    inventory_arima = initial_inventory + order_arima[i] - data$demand[n+i]
    if(inventory_arima > 0 && inventory_arima <= 90){
      cost_arima = cost_arima + inventory_arima
    }
    else if(inventory_arima > 90){
      cost_arima = cost_arima + 2*inventory_arima - 90
    }
    else{
      cost_arima = cost_arima - 3*inventory_arima
    }
    
    #cost of xgb for just month 120+i
    inventory_xgb = initial_inventory + order_xgb[i] - data$demand[n+i]
    if(inventory_xgb > 0 && inventory_xgb <= 90){
      cost_xgb = cost_xgb + inventory_xgb
    }
    else if(inventory_xgb > 90){
      cost_xgb = cost_xgb + 2*inventory_xgb - 90
    }
    else{
      cost_xgb = cost_xgb - 3*inventory_xgb
    }
    
    #update the initial inventory
    initial_inventory = initial_inventory + order.mixed[i] - data$demand[n+i]
    
    EndInventory[i]=InitialInvantory[i]+order.mixed[i]-data$demand[n+i]
    InitialInvantory[i+1]=EndInventory[i]
    
    #calculate the cummulative cost by the mixed strategy
    if(initial_inventory > 0 && initial_inventory <= 90){
      cost.mixed = cost.mixed + initial_inventory
    }
    else if(initial_inventory > 90){
      cost.mixed = cost.mixed + 2*initial_inventory - 90
    }
    else{
      cost.mixed = cost.mixed - 3*initial_inventory
    }
  }
  
  else{
    #update the arima model after demand n+i-1 realized
    tsDataTrain<-ts(data$demand[1:(n+i-1)], start=c(1996, 1), end=c(data$year[n+i-1], data$month12[n+i-1]), frequency=12)
    arima.ts<- auto.arima(tsDataTrain)
    ErrorTerms<-data.frame(error=fitted(arima.ts)-tsDataTrain)
    #predict the demand for month 120+i by auto.arima
    pred_arima[i] = forecast(arima.ts, h=1, level=0)$mean
    #determine order quantity by arima
    if(pred_arima[i] - initial_inventory>0){
      order_arima[i] = pred_arima[i] - initial_inventory + C1_arima*sd(data$demand[(n+i-13):(n+i-1)]) + C2_arima*sd(ErrorTerms$error[(n+i-13):(n+i-1)])
    }
    
    #determine the order quantity by choosing the smaller cost between xgb and arima in month n+i-1
    if(cost_xgb < cost_arima){
      order.mixed[i] = order_xgb[i]
      pred.mixed[i] = pred_xgb[i]
    }
    else{
      order.mixed[i] = order_arima[i]
      pred.mixed[i] = pred_arima[i]
    }
    
    #cost of arima for month 120+i
    inventory_arima = initial_inventory + order_arima[i] - data$demand[n+i]
    if(inventory_arima > 0 && inventory_arima <= 90){
      cost_arima = cost_arima + inventory_arima
    }
    else if(inventory_arima > 90){
      cost_arima = cost_arima + 2*inventory_arima - 90
    }
    else{
      cost_arima = cost_arima - 3*inventory_arima
    }
    
    #cost of xgb for month 120+i
    inventory_xgb = initial_inventory + order_xgb[i] - data$demand[n+i]
    if(inventory_xgb > 0 && inventory_xgb <= 90){
      cost_xgb = cost_xgb + inventory_xgb
    }
    else if(inventory_xgb > 90){
      cost_xgb = cost_xgb + 2*inventory_xgb - 90
    }
    else{
      cost_xgb = cost_xgb - 3*inventory_xgb
    }
    
    #update the initial inventory
    initial_inventory = initial_inventory + order.mixed[i] - data$demand[n+i]
    
    EndInventory[i]=InitialInvantory[i]+order.mixed[i]-data$demand[n+i]
    InitialInvantory[i+1]=EndInventory[i]
    
    #calculate the cummulative cost
    if(initial_inventory > 0 && initial_inventory <= 90){
      cost.mixed = cost.mixed + initial_inventory
    }
    else if(initial_inventory > 90){
      cost.mixed = cost.mixed + 2*initial_inventory - 90
    }
    else{
      cost.mixed = cost.mixed - 3*initial_inventory
    }
  }
}


#####preparing output files#####
OptimalHoldingCost<-rep(0,m)
OptimalBackOrderCost<-rep(0,m)
OptimaltotalMonthlyCost<-rep(0,m)


for (i in 1:m){
  if (EndInventory[i]>=0 && EndInventory[i]<=90) {
    OptimalHoldingCost[i]=EndInventory[i]
  } else if (EndInventory[i]>90) {
    OptimalHoldingCost[i]=90+(EndInventory[i]-90)*2
  } else {
    OptimalBackOrderCost[i]=3*abs(EndInventory[i])
  }
  OptimaltotalMonthlyCost[i]=OptimalHoldingCost[i]+OptimalBackOrderCost[i]
}


Final.Output1<-cbind(1:m,InitialInvantory[1:m],order.mixed,EndInventory,OptimalHoldingCost,OptimalBackOrderCost,OptimaltotalMonthlyCost)
Final.Output1<-data.frame(Final.Output1)
names(Final.Output1)=c("Month","Initial.Inventory","Order.Quantity","Ending.Inventory","Holding.Cost","Backorder.Cost","Total.Monthly.Cost")
write.csv(Final.Output1, file = "inventoryOutputMonthly.csv", row.names = FALSE)


Final.Output2<-cbind(sum(OptimaltotalMonthlyCost),sum(OptimalHoldingCost),mean(OptimalHoldingCost),sum(OptimalBackOrderCost),mean(OptimalBackOrderCost))
Final.Output2<-data.frame(Final.Output2)
names(Final.Output2)=c("Total.Inventory.Cost","Total.Holding.Cost","Avg.Holding.Cost","Total.Backorder.Cost","Avg.Backorder.Cost")
write.csv(Final.Output2, file = "inventoryOutputSummary.csv", row.names = FALSE)






