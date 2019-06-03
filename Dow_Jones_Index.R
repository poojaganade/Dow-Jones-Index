#Load library
library(ggplot2)
library(forecast)
library(fpp)
library(fpp2)
library(urca)
########################################read and create a time series#####################

Dow_jones <- read.csv("dow_jones.csv")
Dow_jones1 <- ts(Dow_jones$Adj.Close, start=c(2000, 1), freq=12)


#############################plot the timeseries graph and acf graph#########################

autoplot(Dow_jones1, col="blue",main="Dow Jones Index",ylab="Adujusted Closing Price", xlab="Year")
ggAcf(Dow_jones1)
ggPacf(Dow_jones1)

#we can see the trend but seasonality is not clear

#############################test stationary or non satationary##############################

test_stationary = ur.kpss(Dow_jones1)
summary(test_stationary)

#found the data is non stationary its dependent on time

######################Decomposition understand the trend and seasonality in the data############
#Performed Decomposition to see with clarity the trend and seasonality
fit_stl <- stl(Dow_jones1, s.window=10)

autoplot(fit_stl)

################################Creating training and Testing Set###############################
dowjones_train<-window(Dow_jones1, start=c(2000, 1), end=c(2017, 12), frequency=12)
dowjones_test <- window(Dow_jones1,start=c(2018,1),end=c(2018,12),frequncy=12)

############################### Simple Forecasting Techniques ###################################
dow_mean <-meanf(dowjones_train,h=12)
dow_mean

autoplot(dowjones_train) +
  autolayer(dow_mean, series="Mean", PI=TRUE) +
  xlab("Year") + ylab("Adjusted Closing Price Monthly") +
  ggtitle("DOW JONES INEDEX") +
  guides(colour=guide_legend(title="Forecast"))


dow_naive <- naive(dowjones_train, h=12)
dow_naive

autoplot(dowjones_train) +
  
  autolayer(dow_naive, series="Naïve", PI=TRUE) +
  xlab("Year") + ylab("Adjusted Closing Price Monthly")+
  ggtitle("DOW JONES INEDEX") +
  scale_color_manual(values = c("orange"))+ 
  guides(colour=guide_legend(title="Forecast"))

dow_snaive <- snaive(dowjones_train, h=12)
dow_snaive

autoplot(dowjones_train) +
  autolayer(dow_snaive, series="Seasonal Naive", PI=TRUE) +
  xlab("Year") + ylab("Adjusted Closing Price Monthly") +
  ggtitle("DOW JONES INEDEX") +
  scale_color_manual(values = c("Blue"))+ 
  guides(colour=guide_legend(title="Forecast"))

dow_drift<-rwf(dowjones_train,h=12,drift=TRUE)
dow_drift

autoplot(dowjones_train) +
  autolayer(dow_drift, series="drift", PI=TRUE) +
  xlab("Year") + ylab("Adjusted Closing Price Monthly") +
  ggtitle("DOW JONES INEDEX") +
  scale_color_manual(values = c("#FF1BB3"))+ 
  guides(colour=guide_legend(title="Forecast"))

Ac_mean<-accuracy(dow_mean, dowjones_test)

i=1
r_mean=0
k=1
j=1

while(j<=8)
{
  a<-Ac_mean[i]
  
  b<-Ac_mean[i+1]
  a<-abs(a)
  b<-abs(b)
  c<-(a+b)/2
  r_mean[k]=c
  k=k+1
  i=i+2
  j=j+1
}
Ac_naive<-accuracy(dow_naive,dowjones_test)


i=1
r_naive=0
k=1
j=1

while(j<=8)
{
  a<-Ac_naive[i]
  b<-Ac_naive[i+1]
  a<-abs(a)
  b<-abs(b)
  c<-(a+b)/2
  r_naive[k]=c
  k=k+1
  i=i+2
  j=j+1
}

Ac_snaive<-accuracy(dow_snaive,dowjones_test)


i=1
r_snaive=0
k=1
j=1

while(j<=8)
{
  a<-Ac_snaive[i]
  b<-Ac_snaive[i+1]
  a<-abs(a)
  b<-abs(b)
  c<-(a+b)/2
  r_snaive[k]=c
  k=k+1
  i=i+2
  j=j+1
}


Ac_drift<-accuracy(dow_drift,dowjones_test)
Ac_drift

i=1
r_drift=0
k=1
j=1

while(j<=8)
{
  a<-Ac_drift[i]
  b<-Ac_drift[i+1]
  a<-abs(a)
  b<-abs(b)
  c<-(a+b)/2
  r_drift[k]=c
  k=k+1
  i=i+2
  j=j+1
}

autoplot(dowjones_train) +
  autolayer(dow_mean, series="Mean", PI=FALSE) +
  autolayer(dow_naive, series="Naïve", PI=FALSE) +
  autolayer(dow_snaive, series="Seasonal Naïve", PI=FALSE) +
  autolayer(dow_drift, series="Drift", PI=FALSE) +
  xlab("year") + ylab("Adjusted Closing Price monthly") +
  ggtitle("DOW JONES INEDEX") +
  guides(colour=guide_legend(title="Forecast"))



################################################Models on Non Stationary Timeseries#############################

##HOlt(works only on trend)##
#Holt works on only trend
holt_fit<-holt(dowjones_train,h=12)
Ac_holt<-accuracy(holt_fit,dowjones_test)
i=1
r_holt=0
k=1
j=1

while(j<=8)
{
  a<-Ac_holt[i]
  b<-Ac_holt[i+1]
  a<-abs(a)
  b<-abs(b)
  c<-(a+b)/2
  r_holt[k]=c
  k=k+1
  i=i+2
  j=j+1
}

r_holt
autoplot(dowjones_train) +
  autolayer(holt_fit, series="Holt", PI=FALSE)+
  xlab("year") + ylab("Adjusted Closing Price monthly") 


##Holt Winter model(works on trend and Seasonality)##
##Additive##


hwa<- hw(dowjones_train,seasonal="additive")
fit_hwa<-forecast(hwa,h=12)
Ac_hwa<-accuracy(fit_hwa,dowjones_test)

autoplot(dowjones_train) +
  autolayer(fit_hwa, series="Holt Winter Additive", PI=FALSE) +
  xlab("year") + ylab("Adjusted Closing Price monthly") 

i=1
r_hwa=0
k=1
j=1

while(j<=8)
{
  a<-Ac_hwa[i]
  b<-Ac_hwa[i+1]
  a<-abs(a)
  b<-abs(b)
  
  c<-(a+b)/2
  r_hwa[k]=c
  k=k+1
  i=i+2
  j=j+1
}
##Holt Winter model(works on trend and Seasonality)##
##Multiplicative##
hwm<- hw(dowjones_train,seasonal="multiplicative")
fit_hwm<-forecast(hwm,h=12)
Ac_hwm<-accuracy(fit_hwm,dowjones_test)
i=1
r_hwm=0
k=1
j=1

while(j<=8)
{
  a<-Ac_hwm[i]
  b<-Ac_hwm[i+1]
  a<-abs(a)
  b<-abs(b)
  
  c<-(a+b)/2
  r_hwm[k]=c
  k=k+1
  i=i+2
  j=j+1
}

autoplot(dowjones_train) +
  autolayer(fit_hwm, series="Holt Winter Multiplicative", PI=FALSE) +
  xlab("year") + ylab("Adjusted Closing Price monthly") 

#####################################################MAking time Series Stationary ###################################

ndiffs(Dow_jones1)
dowjones_stationary<-diff(Dow_jones1)

plot(dowjones_stationary)
plot(acf(dowjones_stationary))
plot(pacf(dowjones_stationary))



#################################################Arima model:works on stationary model############################################################################################
dowjones_train_stationary<-window(dowjones_stationary, start=c(2000, 1), end=c(2017, 12), freq=12)
dowjones_test_stationary <- window(dowjones_stationary, start=c(2018, 1), end=c(2018, 12), freq=12)

#AR model based on acf and pacf
ar_fit<-Arima(dowjones_train_stationary,order=c(1,0,0))
f<-forecast(ar_fit,h=12)
Ac_ar<-accuracy(f,dowjones_test_stationary)

i=1
r_ar=0
k=1
j=1

while(j<=8)
{
  a<-Ac_ar[i]
  b<-Ac_ar[i+1]
  a<-abs(a)
  b<-abs(b)
  c<-(a+b)/2
  r_ar[k]=c
  k=k+1
  i=i+2
  j=j+1
}



autoplot(dowjones_train_stationary) + autolayer(f, series="Auto Regressive Model", PI=FALSE) 


##Best model was found out to be with p=2 d=1 and q=1
bestmodel <-auto.arima(dowjones_train,seasonal=FALSE)

arima_model <- Arima(dowjones_train, order = c(2,1,1),include.drift = TRUE)
summary(arima_model)
fit1<-forecast(arima_model,h=12)
Ac_arima<-accuracy(fit1,dowjones_test)

i=1
r_arima=0
k=1
j=1

while(j<=8)
{
  a<-Ac_arima[i]
  b<-Ac_arima[i+1]
  a<-abs(a)
  b<-abs(b)
  c<-(a+b)/2
  r_arima[k]=c
  k=k+1
  i=i+2
  j=j+1
}


autoplot(dowjones_train) +
  autolayer(fit1, series="Arima model", PI=FALSE)+
  xlab("year") + ylab("Adjusted Closing Price monthly") 

##################################################PLot the errors#####################################################

err<-c("mean","naive","Seasonal Naive","Drift","Holt","Holt Winter (A)","Holt Winter (M)","AR","Arima")
err1<-c(r_mean[1],r_naive[1],r_snaive[1],r_drift[1],r_holt[1],r_hwa[1],r_hwm[1],r_ar[1],r_arima[1])
err2<-c(r_mean[2],r_naive[2],r_snaive[2],r_drift[2],r_holt[2],r_hwa[2],r_hwm[2],r_ar[2],r_arima[2])
err3<-c(r_mean[3],r_naive[3],r_snaive[3],r_drift[3],r_holt[3],r_hwa[3],r_hwm[3],r_ar[3],r_arima[3])
err4<-c(r_mean[4],r_naive[4],r_snaive[4],r_drift[4],r_holt[4],r_hwa[4],r_hwm[4],r_ar[4],r_arima[4])
err5<-c(r_mean[5],r_naive[5],r_snaive[5],r_drift[5],r_holt[5],r_hwa[5],r_hwm[5],r_ar[5],r_arima[5])
err6<-c(r_mean[6],r_naive[6],r_snaive[6],r_drift[6],r_holt[6],r_hwa[6],r_hwm[6],r_ar[6],r_arima[6])
err7<-c(r_mean[7],r_naive[7],r_snaive[7],r_drift[7],r_holt[7],r_hwa[7],r_hwm[7],r_ar[7],r_arima[7])


barplot(err1,names.arg = c("mean","naive","Seasonal Naive","Drift","Holt","Holt Winter (A)","Holt Winter (M)","AR","Arima"),col = "darkslategray1",main="ME")
barplot(err2,names.arg = c("mean","naive","Seasonal Naive","Drift","Holt","Holt Winter (A)","Holt Winter (M)","AR","Arima"),col = "darkslategray2",main=" RMSE ")
barplot(err3,names.arg = c("mean","naive","Seasonal Naive","Drift","Holt","Holt Winter (A)","Holt Winter (M)","AR","Arima"),col = "light blue",main="MAE")
barplot(err4,names.arg = c("mean","naive","Seasonal Naive","Drift","Holt","Holt Winter (A)","Holt Winter (M)","AR","Arima"),col = "darkslategray3",main="MPE")
barplot(err5,names.arg = c("mean","naive","Seasonal Naive","Drift","Holt","Holt Winter (A)","Holt Winter (M)","AR","Arima"),col = "darkturquoise",main="MAPE")
barplot(err6,names.arg = c("mean","naive","Seasonal Naive","Drift","Holt","Holt Winter (A)","Holt Winter (M)","AR","Arima"),col = "darkslategrey",main="MASE")
barplot(err7,names.arg = c("mean","naive","Seasonal Naive","Drift","Holt","Holt Winter (A)","Holt Winter (M)","AR","Arima"),col = "grey",main="ACF1")

#######################################################Find the Best##################################################
e1_min=(which.min(err1))
e2_min=(which.min(err2))
e3_min=(which.min(err3))
e4_min=(which.min(err4))
e5_min=(which.min(err5))
e6_min=(which.min(err6))
e7_min=(which.min(err7))

best<-c(err[e1_min],err[e2_min],err[e3_min],err[e4_min],err[e5_min],err[e6_min],err[e7_min])
best<-factor(best)
table(best)
#So Arima is best
###############################################Find the worst model###################################################

e1_max=(which.max(err1))
e2_max=(which.max(err2))
e3_max=(which.max(err3))
e4_max=(which.max(err4))
e5_max=(which.max(err5))
e6_max=(which.max(err6))
e7_max=(which.max(err7))

worst<-c(err[e1_max],err[e2_max],err[e3_max],err[e4_max],err[e5_max],err[e6_max],err[e7_max])
worst<-factor(worst)
table(worst)
#So mean is the worst