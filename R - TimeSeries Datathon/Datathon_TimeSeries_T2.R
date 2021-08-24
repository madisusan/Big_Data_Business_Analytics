library(fBasics)
library(forecast) 

data<-read.csv("/Volumes/DOCUMENTS/IE/Big Data/TERM 2/Datathon/TimeSeries on R/manufacturer_2_per_week.csv",header=TRUE,sep=",")

y<-data[,3][1:(nrow(data)-4)]      # leave the last 5 observations to compare with the forecasted values

# achieving stationarity and identifying the model
# Is my data stationary? Do I have to take any differences or logs?
par(mar=c(4,4,1,1)) # to adjust graphic size

y <- log(y)

ts.plot(y)  

par(mfrow=c(2,1))
acf(y, lag.max = 80)  
pacf(y, lag.max = 104)

s = 52;

ndiffs(y, alpha=0.05, test=c("adf"))
nsdiffs(y,m=s,test=c("ocsb"))

# estimating the model
fit<-arima(y,order=c(12,1,0),seasonal=list(order=c(0,1,0),period=s))  
fit # we find the information about the estimated parameters

ts.plot(fit$residuals) 
acf(fit$residuals, lag.max = 80)  
pacf(fit$residuals, lag.max = 104)

y.pred<-predict(fit,n.ahead=108)

exp.fit <- exp(y.pred$pred)

SFE = rep(0, 4)
APE = rep(0, 4)


for (i in 1:length(exp.fit)){
  SFE[i] <- (data[length(y)+i,3]-exp.fit[i])^2
  APE[i] <- (abs((data[length(y)+i,3]-exp.fit[i]))/(data[length(y)+i,3]))*100
}

MSFE <- mean(SFE);
MAPE <- mean(APE);

 <- sqrt(MSFE)

old_data_overall <- data[1:140, 3]
new_data_overall <- exp.fit[5:108]

new_data_overall <- new_data_overall*1.029
new_data_overall[17:69] <- new_data[17:69]*0.982 ## Year 1
new_data_overall[70:length(new_data_overall)] <- new_data_overall[70:length(new_data_overall)] * 0.984
new_data_overall <- new_data_overall*0.96
new_data_overall[17:length(new_data_overall)] <- new_data_overall[17:length(new_data_overall)] * 0.972


new_overall <- c(old_data_overall,new_data_overall) # real data + predicted values

par(bg = "#403e3f", col.axis ='white', col.lab = 'white', col.main = 'white', fg = 'white')
plot.ts(new_overall,main="Total Value Forecasts",
        ylab="Overall Total Market Value",col="white",lwd=2) # time series plot
lines(old_data_overall,col= "#ffc03f",lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c('white','#ffc03f'),
       bty="n",lwd=2)

######################################################  NATIONAL ############################################################################

data<-read.csv("C:/Users/Moutaz Huneidi/Desktop/MBD/Term 2/Datathon/Datathon/national_per_week.csv",header=TRUE,sep=",")

y<-data[,3][1:(nrow(data)-4)]      # leave the last 5 observations to compare with the forecasted values

# achieving stationarity and identifying the model
# Is my data stationary? Do I have to take any differences or logs?
par(mar=c(4,4,1,1)) # to adjust graphic size

y <- log(y)

ts.plot(y)  

par(mfrow=c(2,1))
acf(y, lag.max = 80)  
pacf(y, lag.max = 104)

s = 52;

ndiffs(y, alpha=0.05, test=c("adf"))
nsdiffs(y,m=s,test=c("ocsb"))

# estimating the model
fit<-arima(y,order=c(8,1,0),seasonal=list(order=c(0,1,0),period=s))  
fit # we find the information about the estimated parameters

ts.plot(fit$residuals) 
acf(fit$residuals, lag.max = 80)  
pacf(fit$residuals, lag.max = 104)

y.pred<-predict(fit,n.ahead=108)

exp.fit <- exp(y.pred$pred)

SFE = rep(0, 4)
APE = rep(0, 4)


for (i in 1:length(exp.fit)){
  SFE[i] <- (data[length(y)+i,3]-exp.fit[i])^2
  APE[i] <- (abs((data[length(y)+i,3]-exp.fit[i]))/(data[length(y)+i,3]))*100
}

MSFE <- mean(SFE);
MAPE <- mean(APE);

old_data_national <- data[1:140, 3]
new_data_national <- exp.fit[5:108]

new_data_national <- new_data_national*1.029
new_data_national[17:69] <- new_data_national[17:69]*0.982 ## Year 1
new_data_national[70:length(new_data_national)] <- new_data_national[70:length(new_data_national)] * 0.984
new_data_national <- new_data_national*0.96
new_data_national[17:length(new_data_national)] <- new_data_national[17:length(new_data_national)] * 0.972

new_national <- c(old_data_national,new_data_national) # real data + predicted values

par(bg = "#403e3f", col.axis ='white', col.lab = 'white', col.main = 'white', fg = 'white')
plot.ts(new_national,main="National Segment Forecasts",
        ylab="Total Market Value", xlab = 'Week',col="white",lwd=2) # time series plot
lines(old_data_national,col= "#ffc03f",lwd=2)
legend("topleft",legend=c("Predictions","Historical"),col=c('white','#ffc03f'),
       bty="n",lwd=2)

######################################################  IP ############################################################################

data<-read.csv("C:/Users/Moutaz Huneidi/Desktop/MBD/Term 2/Datathon/Datathon/ip_per_week.csv",header=TRUE,sep=",")

y<-data[,3][1:(nrow(data)-4)]      # leave the last 5 observations to compare with the forecasted values

# achieving stationarity and identifying the model
# Is my data stationary? Do I have to take any differences or logs?
par(mar=c(4,4,1,1)) # to adjust graphic size

y <- log(y)

ts.plot(y)  

par(mfrow=c(2,1))
acf(y, lag.max = 80)  
pacf(y, lag.max = 104)

s = 52;

ndiffs(y, alpha=0.05, test=c("adf"))
nsdiffs(y,m=s,test=c("ocsb"))

# estimating the model
fit<-arima(y,order=c(8,0,20),seasonal=list(order=c(0,1,0),period=s))  
fit # we find the information about the estimated parameters

ts.plot(fit$residuals) 
acf(fit$residuals, lag.max = 80)  
pacf(fit$residuals, lag.max = 104)

y.pred<-predict(fit,n.ahead=108)

exp.fit <- exp(y.pred$pred)

SFE = rep(0, 4)
APE = rep(0, 4)


for (i in 1:length(exp.fit)){
  SFE[i] <- (data[length(y)+i,3]-exp.fit[i])^2
  APE[i] <- (abs((data[length(y)+i,3]-exp.fit[i]))/(data[length(y)+i,3]))*100
}

old_data_ip <- data[1:140, 3]
new_data_ip <- exp.fit[5:108]

new_data_ip <- new_data_ip*1.029
new_data_ip[17:69] <- new_data_ip[17:69]*0.982 ## Year 1
new_data_ip[70:length(new_data_ip)] <- new_data_ip[70:length(new_data_ip)] * 0.984
new_data_ip <- new_data_ip*0.96
new_data_ip[17:length(new_data_ip)] <- new_data_ip[17:length(new_data_ip)] * 0.972

new_ip <- c(old_data_ip,new_data_ip) # real data + predicted values

plot.ts(new_ip,main="Imported Premium Segment Forecast",
        ylab="Total Market Value", xlab = "Week",col='white',lwd=2) # time series plot
lines(old_data_ip,col='#ffc03f',lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c('white','#ffc03f'),
       bty="n",lwd=2)
######################################################  IS ############################################################################

data<-read.csv("C:/Users/Moutaz Huneidi/Desktop/MBD/Term 2/Datathon/Datathon/is_per_week.csv",header=TRUE,sep=",")

y<-data[,3][1:(nrow(data)-4)]      # leave the last 5 observations to compare with the forecasted values

# achieving stationarity and identifying the model
# Is my data stationary? Do I have to take any differences or logs?
par(mar=c(4,4,1,1)) # to adjust graphic size

y <- log(y)

ts.plot(y)  

par(mfrow=c(2,1))
acf(y, lag.max = 80)  
pacf(y, lag.max = 104)

s = 52;

ndiffs(y, alpha=0.05, test=c("adf"))
nsdiffs(y,m=s,test=c("ocsb"))

# estimating the model
fit<-arima(y,order=c(12,1,0),seasonal=list(order=c(0,1,1),period=s))  
fit # we find the information about the estimated parameters

ts.plot(fit$residuals) 
acf(fit$residuals, lag.max = 80)  
pacf(fit$residuals, lag.max = 104)

y.pred<-predict(fit,n.ahead=108)

exp.fit <- exp(y.pred$pred)

SFE = rep(0, 4)
APE = rep(0, 4)


for (i in 1:length(exp.fit)){
  SFE[i] <- (data[length(y)+i,3]-exp.fit[i])^2
  APE[i] <- (abs((data[length(y)+i,3]-exp.fit[i]))/(data[length(y)+i,3]))*100
}

MSFE <- mean(SFE);
MAPE <- mean(APE);

<- sqrt(MSFE)

old_data <- data[1:140, 3]
new_data <- exp.fit[5:108]

new_data <- new_data*1.029
new_data[17:69] <- new_data[17:69]*0.982 ## Year 1
new_data[70:length(new_data)] <- new_data[70:length(new_data)] * 0.984
new_data <- new_data*0.96
new_data[17:length(new_data)] <- new_data[17:length(new_data)] * 0.972

new_is <- c(old_data,new_data) # real data + predicted values

plot.ts(new_is,main="Imported Special Segment Forecast",
        ylab="Total Market Value", xlab = 'Week',col='white',lwd=2) # time series plot
lines(old_data,col='#ffc03f',lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c('white','#ffc03f'),
       bty="n",lwd=2)
######################################################  FLAVOURED ############################################################################

data<-read.csv("C:/Users/Moutaz Huneidi/Desktop/MBD/Term 2/Datathon/Datathon/fl_per_week.csv",header=TRUE,sep=",")

y<-data[,3][1:(nrow(data)-4)]      # leave the last 5 observations to compare with the forecasted values

# achieving stationarity and identifying the model
# Is my data stationary? Do I have to take any differences or logs?
par(mar=c(4,4,1,1)) # to adjust graphic size

y <- log(y)

ts.plot(y)  

par(mfrow=c(2,1))
acf(y, lag.max = 80)  
pacf(y, lag.max = 104)

s = 52;

ndiffs(y, alpha=0.05, test=c("adf"))
nsdiffs(y,m=s,test=c("ocsb"))

# estimating the model
fit<-arima(y,order=c(44,1,0),seasonal=list(order=c(0,1,0),period=s))  
fit # we find the information about the estimated parameters

ts.plot(fit$residuals) 
acf(fit$residuals, lag.max = 80)  
pacf(fit$residuals, lag.max = 104)

y.pred<-predict(fit,n.ahead=108)

exp.fit <- exp(y.pred$pred)

SFE = rep(0, 4)
APE = rep(0, 4)


for (i in 1:length(exp.fit)){
  SFE[i] <- (data[length(y)+i,3]-exp.fit[i])^2
  APE[i] <- (abs((data[length(y)+i,3]-exp.fit[i]))/(data[length(y)+i,3]))*100
}

MSFE <- mean(SFE);
MAPE <- mean(APE);

old_data <- data[1:140, 3]
new_data_flav <- exp.fit[5:108]

new_data_flav <- new_data_flav*1.029 # OVerall Market Growth
new_data_flav[17:69] <- new_data_flav[17:69]*0.982 ## Year 1 economic outlook
new_data_flav[70:length(new_data_flav)] <- new_data_flav[70:length(new_data_flav)] * 0.984  ## Year 2 economic outlook
new_data_flav <- new_data_flav*0.96 # Mercadona and similar threat
new_data_flav[17:length(new_data_flav)] <- new_data_flav[17:length(new_data_flav)] * 0.972 # Brexit


new_flav <- c(old_data,new_data_flav) # real data + predicted values

plot.ts(new_flav,main="Flavoured Segment Forecast",
        ylab="Total Market Value", xlab = 'Week',col='white',lwd=2) # time series plot
lines(old_data,col='#ffc03f',lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c('white','#ffc03f'),
       bty="n",lwd=2)

######################################################  EXTRA ############################################################################

data<-read.csv("C:/Users/Moutaz Huneidi/Desktop/MBD/Term 2/Datathon/Datathon/ex_per_week.csv",header=TRUE,sep=",")

y<-data[,3][1:(nrow(data)-4)]      # leave the last 5 observations to compare with the forecasted values

# achieving stationarity and identifying the model
# Is my data stationary? Do I have to take any differences or logs?
par(mar=c(4,4,1,1)) # to adjust graphic size

y <- log(y)

ts.plot(y)  

par(mfrow=c(2,1))
acf(y, lag.max = 80)  
pacf(y, lag.max = 104)

s = 52;

ndiffs(y, alpha=0.05, test=c("adf"))
nsdiffs(y,m=s,test=c("ocsb"))

# estimating the model
fit<-arima(y,order=c(5,1,0),seasonal=list(order=c(0,1,0),period=s))  
fit # we find the information about the estimated parameters

ts.plot(fit$residuals) 
acf(fit$residuals, lag.max = 80)  
pacf(fit$residuals, lag.max = 104)

y.pred<-predict(fit,n.ahead=108)

exp.fit <- exp(y.pred$pred)

SFE = rep(0, 4)
APE = rep(0, 4)


for (i in 1:length(exp.fit)){
  SFE[i] <- (data[length(y)+i,3]-exp.fit[i])^2
  APE[i] <- (abs((data[length(y)+i,3]-exp.fit[i]))/(data[length(y)+i,3]))*100
}

MSFE <- mean(SFE);
MAPE <- mean(APE);

<- sqrt(MSFE)

old_data <- data[1:140, 3]
new_data_extra <- exp.fit[5:108]
new_data_extra <- new_data_extra*1.029 # OVerall Market Growth
new_data_extra[17:69] <- new_data_extra[17:69]*0.982 ## Year 1 economic outlook
new_data_extra[70:length(new_data_extra)] <- new_data_extra[70:length(new_data_extra)] * 0.984  ## Year 2 economic outlook
new_data_extra <- new_data_extra*0.96 # Mercadona and similar threat
new_data_extra[17:length(new_data_extra)] <- new_data_extra[17:length(new_data_extra)] * 0.972 # Brexit

new_extra <- c(old_data,new_data_extra) # real data + predicted values

plot.ts(new_extra,main="Extra Segment Forecast",
        ylab="Total Market Value", xlab = 'Week',col='white',lwd=2) # time series plot
lines(old_data,col='#ffc03f',lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c('white','#ffc03f'),
       bty="n",lwd=2)

######################################################  BLACK ############################################################################

data<-read.csv("C:/Users/Moutaz Huneidi/Desktop/MBD/Term 2/Datathon/Datathon/bl_per_week.csv",header=TRUE,sep=",")

y<-data[,3][1:(nrow(data)-4)]      # leave the last 5 observations to compare with the forecasted values

# achieving stationarity and identifying the model
# Is my data stationary? Do I have to take any differences or logs?
par(mar=c(4,4,1,1)) # to adjust graphic size

y <- log(y)

ts.plot(y)  

par(mfrow=c(2,1))
acf(y, lag.max = 80)  
pacf(y, lag.max = 104)

s = 52;

ndiffs(y, alpha=0.05, test=c("adf"))
nsdiffs(y,m=s,test=c("ocsb"))

# estimating the model
fit<-arima(y,order=c(25,1,0),seasonal=list(order=c(1,1,0),period=s))  
fit # we find the information about the estimated parameters

ts.plot(fit$residuals) 
acf(fit$residuals, lag.max = 80)  
pacf(fit$residuals, lag.max = 104)

y.pred<-predict(fit,n.ahead=108)

exp.fit <- exp(y.pred$pred)

SFE = rep(0, 4)
APE = rep(0, 4)


for (i in 1:length(exp.fit)){
  SFE[i] <- (data[length(y)+i,3]-exp.fit[i])^2
  APE[i] <- (abs((data[length(y)+i,3]-exp.fit[i]))/(data[length(y)+i,3]))*100
}

MSFE <- mean(SFE);
MAPE <- mean(APE);

<- sqrt(MSFE)

old_data <- data[1:140, 3]
new_data_black <- exp.fit[5:108]

new_data_black <- new_data_black*1.029 # OVerall Market Growth
new_data_black[17:69] <- new_data_black[17:69]*0.982 ## Year 1 economic outlook
new_data_black[70:length(new_data_black)] <- new_data_black[70:length(new_data_black)] * 0.984  ## Year 2 economic outlook
new_data_black <- new_data_black*0.96 # Mercadona and similar threat
new_data_black[17:length(new_data_black)] <- new_data_black[17:length(new_data_black)] * 0.972 # Brexit

new_black <- c(old_data,new_data_black) # real data + predicted values

plot.ts(new_black,main="Black Segment Forecast",
        ylab="Total Market Value", xlab = 'Week',col='white',lwd=2) # time series plot
lines(old_data,col='#ffc03f',lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c('white','#ffc03f'),
       bty="n",lwd=2)

######################################################  WO ############################################################################

data<-read.csv("C:/Users/Moutaz Huneidi/Desktop/MBD/Term 2/Datathon/Datathon/wo_per_week.csv",header=TRUE,sep=",")

y<-data[,3][1:(nrow(data)-4)]      # leave the last 5 observations to compare with the forecasted values

# achieving stationarity and identifying the model
# Is my data stationary? Do I have to take any differences or logs?
par(mar=c(4,4,1,1)) # to adjust graphic size

y <- log(y)

ts.plot(y)  

par(mfrow=c(2,1))
acf(y, lag.max = 80)  
pacf(y, lag.max = 104)

s = 52;

ndiffs(y, alpha=0.05, test=c("adf"))
nsdiffs(y,m=s,test=c("ocsb"))

# estimating the model
fit<-arima(y,order=c(23,1,0),seasonal=list(order=c(0,1,0),period=s))  
fit # we find the information about the estimated parameters

ts.plot(fit$residuals) 
acf(fit$residuals, lag.max = 80)  
pacf(fit$residuals, lag.max = 104)

y.pred<-predict(fit,n.ahead=108)

exp.fit <- exp(y.pred$pred)

SFE = rep(0, 4)
APE = rep(0, 4)


for (i in 1:length(exp.fit)){
  SFE[i] <- (data[length(y)+i,3]-exp.fit[i])^2
  APE[i] <- (abs((data[length(y)+i,3]-exp.fit[i]))/(data[length(y)+i,3]))*100
}

MSFE <- mean(SFE);
MAPE <- mean(APE);

<- sqrt(MSFE)

old_data <- data[1:140, 3]
new_data_wo <- exp.fit[5:108]

new_data_wo <- new_data_wo*1.029 # OVerall Market Growth
new_data_wo[17:69] <- new_data_wo[17:69]*0.982 ## Year 1 economic outlook
new_data_wo[70:length(new_data_wo)] <- new_data_wo[70:length(new_data_wo)] * 0.984  ## Year 2 economic outlook
new_data_wo <- new_data_wo*0.96 # Mercadona and similar threat
new_data_wo[17:length(new_data_wo)] <- new_data_wo[17:length(new_data_wo)] * 0.972 # Brexit


new_wo <- c(old_data,new_data_wo) # real data + predicted values

plot.ts(new_wo,main="Without Alcohol Segment Forecast",
        ylab="Total Market Value", xlab = 'Week',col='white',lwd=2) # time series plot
lines(old_data,col='#ffc03f',lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c('white','#ffc03f'),
       bty="n",lwd=2)


######################################################  ZERO ############################################################################

data<-read.csv("C:/Users/Moutaz Huneidi/Desktop/MBD/Term 2/Datathon/Datathon/zero_per_week.csv",header=TRUE,sep=",")

y<-data[,3][1:(nrow(data)-4)]      # leave the last 5 observations to compare with the forecasted values

# achieving stationarity and identifying the model
# Is my data stationary? Do I have to take any differences or logs?
par(mar=c(4,4,1,1)) # to adjust graphic size

y <- log(y)

ts.plot(y)  

par(mfrow=c(2,1))
acf(y, lag.max = 80)  
pacf(y, lag.max = 104)

s = 52;

ndiffs(y, alpha=0.05, test=c("adf"))
nsdiffs(y,m=s,test=c("ocsb"))

# estimating the model
fit<-arima(y,order=c(6,1,0),seasonal=list(order=c(1,1,0),period=s))  
fit # we find the information about the estimated parameters

ts.plot(fit$residuals) 
acf(fit$residuals, lag.max = 80)  
pacf(fit$residuals, lag.max = 104)

y.pred<-predict(fit,n.ahead=108)

exp.fit <- exp(y.pred$pred)

SFE = rep(0, 4)
APE = rep(0, 4)


for (i in 1:length(exp.fit)){
  SFE[i] <- (data[length(y)+i,3]-exp.fit[i])^2
  APE[i] <- (abs((data[length(y)+i,3]-exp.fit[i]))/(data[length(y)+i,3]))*100
}

MSFE <- mean(SFE);
MAPE <- mean(APE);

old_data <- data[1:140, 3]
new_data_zero <- exp.fit[5:108]
new_data_zero <- new_data_zero*1.029 # OVerall Market Growth
new_data_zero[17:69] <- new_data_zero[17:69]*0.982 ## Year 1 economic outlook
new_data_zero[70:length(new_data_zero)] <- new_data_zero[70:length(new_data_zero)] * 0.984  ## Year 2 economic outlook
new_data_zero <- new_data_zero*0.96 # Mercadona and similar threat
new_data_zero[17:length(new_data_zero)] <- new_data_zero[17:length(new_data_zero)] * 0.972 # Brexit

new_zero <- c(old_data,new_data_zero) # real data + predicted values

plot.ts(new_zero,main="Zero Alcohol Segment Forecast",
        ylab="Total Market Value", xlab = 'Week',col='white',lwd=2) # time series plot
lines(old_data,col='#ffc03f',lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c('white','#ffc03f'),
       bty="n",lwd=2)
