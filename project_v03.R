#Load library
library(ggplot2)
library(forecast)
library(fpp)
library(fpp2)
library(urca)
############################read and create a time series#####################

Dow_jones <- read.csv("C:/Users/pooja/Documents/Sample/BF/dow_jones.csv")
Dow_jones1 <- ts(Dow_jones$Adj.Close, start=c(2000, 1), freq=12)

##################plot the timeseries graph and acf graph#########################

plot(Dow_jones1)
ggAcf(Dow_jones1)
ggPacf(Dow_jones1)

#we can see the trend but there is no seasonaliy.

############st stationary or non satationary##############################

test_stationary = ur.kpss(Dow_jones1)
summary(test_stationary)

#found the data is non stationary its dependent on time

######Decomposition to understand the trend and seasonality in the data############

fit_stl <- stl(Dow_jones1, s.window=10)
plot(fit_stl)
fit_stl$time.series
plot(Dow_jones1, col="gray",main="Dow Jones Index",ylab="Adujusted Closing Price", xlab="")


monthplot(fit_stl$time.series[,"seasonal"], main="", ylab="Seasonal")
#how to interpret this


################ Simple Forecasting Techniques ###################################
#################Without Adjusting Seasonality and trend############################

dowjones_train<-ts(Dow_jones1, start=c(2000, 1), end=c(2017, 12), freq=12)
dowjones_test <- ts(Dow_jones1, start=c(2018, 1), end=c(2018, 12), freq=12)


dow_mean <-meanf(dowjones_train,h=12)
dow_mean

dow_naive <- naive(dowjones_train, h=12)
dow_naive

dow_snaive <- snaive(dowjones_train, h=12)
dow_snaive

dow_drift<-rwf(dowjones_train,h=12,drift=TRUE)
dow_drift

Ac_mean<-accuracy(dow_mean, dowjones_test)
Acmean<-Ac_mean[4]*Ac_mean[4]

Ac_naive<-accuracy(dow_naive,dowjones_test)
Acnaive<-Ac_naive[4]*Ac_naive[4]

Ac_snavie<-accuracy(dow_snaive,dowjones_test)
Acsnaive<-Ac_snavie[4]*Ac_snavie[4]

Ac_drift<-accuracy(dow_drift,dowjones_test)
Acdrift<-Ac_drift[4]*Ac_drift[4]

autoplot(dowjones_train) +
  autolayer(dow_mean, series="Mean", PI=FALSE) +
  autolayer(dow_naive, series="Naïve", PI=FALSE) +
  autolayer(dow_snaive, series="Seasonal Naïve", PI=FALSE) +
  autolayer(dow_drift, series="Drift", PI=FALSE) +
  xlab("Monthly") + ylab("Adjusted Closing Price") +
  ggtitle("DOW JONES INEDEX") +
  guides(colour=guide_legend(title="Forecast"))

err<-c(Acmean,Acnaive,Acsnaive,Acdrift)
barplot(err,names.arg = c("mean","naive","Seasonal Naive","Drift"),col = "green")

##############Models on Non Stationary Timeseries#############################

##HOlt(works only on trend)##
#Holt works on only trend
#Not sure if the seasonality is there in the data,if seasonality is there then we will use  holt winter model

holt_fit<-holt(dowjones_train,h=12)
fit_holt<-forecast(holt_fit,h=12)
Ac_holt<-accuracy(fit_holt,dowjones_test)
Holt_mse<-Ac_holt[4]*Ac_holt[4]
autoplot(dowjones_train) +
  autolayer(fit_holt, series="Holt", PI=FALSE) 

##Holt Winter model(works on trend and Seasonality)##
##Additive##
hwa<- hw(dowjones_train,seasonal="additive")
fit_hwa<-forecast(hwa,h=12)
Ac_hwa<-accuracy(fit_hwa,dowjones_test)
Hwa_mse<-Ac_hwa[4]*Ac_hwa[4]
autoplot(dowjones_train) +
  autolayer(fit_hwa, series="Holt Winter Additive", PI=FALSE)

##Holt Winter model(works on trend and Seasonality)##
##Multiplicative##
hwm<- hw(dowjones_train,seasonal="multiplicative")
fit_hwm<-forecast(hwm,h=12)
Ac_hwm<-accuracy(fit_hwm,dowjones_test)
Hwm_mse<-Ac_hwm[4]*Ac_hwm[4]

autoplot(dowjones_train) +
  autolayer(fit_hwm, series="Holt Winter Multiplicative", PI=FALSE) 



################Making time Series Stationary ###################################


ndiffs(Dow_jones1)
dowjones_stationary<-diff(Dow_jones1)

plot(dowjones_stationary)
plot(acf(dowjones_stationary))
plot(pacf(dowjones_stationary))
dowjones_train_stationary<-ts(dowjones_stationary, start=c(2000, 1), end=c(2017, 12), freq=12)
dowjones_test_stationary <- ts(dowjones_stationary, start=c(2018, 1), end=c(2018, 12), freq=12)



##ARMA##to be used
#find p
#check accuracy

arma_fit<- Arima(dowjones_train_stationary, order=c(1,0,0))
arma_fit

#

##################Arima model:works on stationary model#############################

##Best model was found out to be with p=2 d=1 and q=1
bestmodel <-auto.arima(dowjones_train_stationary,seasonal=FALSE)

arima_model <- Arima(dowjones_train_stationary, order = c(2,1,1),include.drift = TRUE)
summary(arima_model)
fit1<-forecast(arima_model,h=12)
accuracy(fit1,dowjones_test_stationary)

autoplot(dowjones_train_stationary) +
  autolayer(fit1, series="Arima model", PI=FALSE) 



##################### Shiny App ####################


library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
Dow_jones <- read.csv("C:/Users/pooja/Documents/Sample/BF/dow_jones.csv")
Dow_jones1 <- ts(Dow_jones$Adj.Close, start=c(2000, 1), freq=12)


frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  
  
)
frow1.a<-fluidRow(
  valueBoxOutput("value3")
  ,valueBoxOutput("value4")
)
frow2 <- fluidRow( 
  
  box(
    title = "Bins",status = "primary", solidHeader = TRUE,collapsible = TRUE,
    sliderInput("Bin", "Slider input:", 1, 50, 20),height="360px"
    
  )
  ,box(
    title = "Histogram"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("Histogram", height = "300px")
  )
)

frow3 <- fluidRow( 
  
  box(
    title = "BOXplot"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("BoxPlot", height = "300px")
    ,width="600px"
  )
)
frow4 <- fluidRow( 
  
  box(
    title = "Time Series Plot"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("Timeseries", height = "300px")
    ,width = "400px"
  )
  
)

frow5<-fluidRow(
  box(
    title="ACF"
    ,status="primary"
    ,solidHeader=TRUE
    ,collapsible=TRUE
    ,plotOutput("ACF",height="300px")
  ),
  box(
    title="PACF"
    ,status="primary"
    ,solidHeader=TRUE
    ,collapsible=TRUE
    ,plotOutput("PACF",height="300px")
  )
)

frow6<-fluidRow( 
  box(title="Time Series Decomposition"
      ,status="primary"
      ,solidHeader=TRUE
      ,collapsible=TRUE
      ,plotOutput("TSD",height="300px")
      ,width='600px'
  ))
frow7<-fluidRow( 
  box(title="Basic Forecasting techniques"
      ,status="primary"
      ,solidHeader=TRUE
      ,collapsible=TRUE
      ,plotOutput("BFT",height="300px")
      ,width='600px'
  ))

frow8<-fluidRow( 
  box(title="MEAN"
      ,status="primary"
      ,solidHeader=TRUE
      ,collapsible=TRUE
      ,plotOutput("mean",height="300px")
      
  ),
  box(title="Drift"
      ,status="primary"
      ,solidHeader=TRUE
      ,collapsible=TRUE
      ,plotOutput("drift",height="300px")
      
  ))

frow9<-fluidRow(
  box(title="Naive"
      ,status="primary"
      ,solidHeader=TRUE
      ,collapsible=TRUE
      ,plotOutput("naive",height="300px")
      
  ),
  
  box(title="Seasonal Naive"
      ,status="primary"
      ,solidHeader=TRUE
      ,collapsible=TRUE
      ,plotOutput("snaive",height="300px")
      
  ))
frow10<-fluidRow(
  box(title="holt"
      ,status="primary"
      ,solidHeader=TRUE
      ,collapsible=TRUE
      ,plotOutput("holt",height="300px")
      ,width="600px"
      
  ))
frow11<-fluidRow(
  box(title="holt Winter Multiplicative"
      ,status="primary"
      ,solidHeader=TRUE
      ,collapsible=TRUE
      ,plotOutput("hwm",height="300px")
      
      
  ),
  box(title="holt Winter Additive"
      ,status="primary"
      ,solidHeader=TRUE
      ,collapsible=TRUE
      ,plotOutput("hwa",height="300px")
      
  ))
frow12<-fluidRow(
  box(title="Arima(2,1,1)"
      ,status="primary"
      ,solidHeader=TRUE
      ,collapsible=TRUE
      ,plotOutput("Arima",height="300px")
      
      
  ),
  box(title="Auto Regressive Model"
      ,status="primary"
      ,solidHeader=TRUE
      ,collapsible=TRUE
      ,plotOutput("ar",height="300px")
      
  ))


# combine the two fluid rows to make the body

ui <- dashboardPage(
  #Dashboard header carrying the title of the dashboard
  header <- dashboardHeader(title = "DOW JONES INDEX"),  
  #Sidebar content of the dashboard
  sidebar <- dashboardSidebar(
    sidebarMenu(menuItem("Exploratory Analysis", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Time Series", tabName = "dashboard2", 
                         icon = icon("dashboard")),menuItem("Basic Forecasting Tools", 
                                                            tabName = "dashboard3", icon = icon("dashboard")),
                menuItem("Advanced Forecasting Method", tabName = "dashboard4", icon = icon("dashboard")))),
  body <- dashboardBody(tabItems(tabItem(tabName= "dashboard",frow1,frow1.a,frow2,frow3),
                                 tabItem(tabName = "dashboard2",frow4,frow5,frow6),
                                 tabItem(tabName = "dashboard3",frow7,frow8,frow9),
                                 tabItem(tabName = "dashboard4",frow10,frow11,frow12))))




# create the server functions for the dashboard  
server <- function(input, output) { 
  #some data manipulation to derive the values of KPI boxes
  mean_Adj <- mean(Dow_jones1)
  median_Adj <- median(Dow_jones1)
  mode_Adj <- mode(Dow_jones1)
  
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(mean_Adj, format="d", big.mark=',')
      ,paste('Mean')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(median_Adj, format="d", big.mark=',')
      ,'Median'
      ,icon = icon("",lib='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(max(Dow_jones1), format="d", big.mark=',')
      ,paste('Maximum')
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
    
  })
  output$value4 <- renderValueBox({
    valueBox(
      formatC(min(Dow_jones1), format="d", big.mark=',')
      ,paste('Minimum')
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "blue")  
  })
  
  fit_stl <- stl(Dow_jones1, s.window=10)
  
  
  output$TSD <- renderPlot({autoplot(fit_stl)})
  output$Timeseries<-renderPlot({autoplot(Dow_jones1)})
  output$ACF<-renderPlot({ggAcf(Dow_jones1)})
  output$PACF<-renderPlot({ggPacf(Dow_jones1)})
  #creating the plotOutput content
  
  output$Histogram <- renderPlot({
    x<-Dow_jones1
    bins <- seq(min(x), max(x), length.out = input$Bin + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Adj Close in $",
         main = "Histogram of Adj Close")
  })
  output$BoxPlot<- renderPlot({
    boxplot(Dow_jones1,horizontal=TRUE)
  })
  
  
  dowjones_train<-ts(Dow_jones1, start=c(2000, 1), end=c(2017, 12), freq=12)
  dowjones_test <- ts(Dow_jones1, start=c(2018, 1), end=c(2018, 12), freq=12)
  
  
  dow_mean <-meanf(dowjones_train,h=12)
  dow_mean
  output$mean<-renderPlot({autoplot(dow_mean)})
  
  
  
  dow_naive <- naive(dowjones_train, h=12)
  dow_naive
  output$naive<-renderPlot({autoplot(dow_naive)})
  
  dow_snaive <- snaive(dowjones_train, h=12)
  output$snaive<-renderPlot({autoplot(dow_snaive)})
  
  dow_drift<-rwf(dowjones_train,h=12,drift=TRUE)
  output$drift<-renderPlot({autoplot(dow_drift)})
  
  output$BFT<-renderPlot({autoplot(dowjones_train) +
      autolayer(dow_mean, series="Mean", PI=FALSE) +
      autolayer(dow_naive, series="Naive", PI=FALSE) +
      autolayer(dow_snaive, series="Seasonal Naive", PI=FALSE) +
      autolayer(dow_drift, series="Drift", PI=FALSE) +
      xlab("Monthly") + ylab("Adjusted Closing Price") +
      ggtitle("DOW JONES INEDEX") +
      guides(colour=guide_legend(title="Forecast"))})
  
  holt_fit<-holt(dowjones_train,h=12)
  fit_holt<-forecast(holt_fit,h=12)
  output$holt<-renderPlot({autoplot(dowjones_train) +
      autolayer(fit_holt, series="Holt", PI=FALSE) })
  
  hwa<- hw(dowjones_train,seasonal="additive")
  fit_hwa<-forecast(hwa,h=12)
  
  output$hwa<-renderPlot({
    autoplot(dowjones_train) +
      autolayer(fit_hwa, series="Holt Winter Additive", PI=FALSE) })
  
  hwm<- hw(dowjones_train,seasonal="multiplicative")
  fit_hwm<-forecast(hwm,h=12)
  
  output$hwm<-renderPlot({
    autoplot(dowjones_train) +
      autolayer(fit_hwm, series="Holt Winter Multiplicative", PI=FALSE) })
  
  ndiffs(Dow_jones1)
  dowjones_stationary<-diff(Dow_jones1)
  
  plot(dowjones_stationary)
  plot(acf(dowjones_stationary))
  plot(pacf(dowjones_stationary))
  
  dowjones_train_stationary<-ts(dowjones_stationary, start=c(2000, 1), end=c(2017, 12), freq=12)
  dowjones_test_stationary <- ts(dowjones_stationary, start=c(2018, 1), end=c(2018, 12), freq=12)
  
  fitteda<-Arima(dowjones_train_stationary,order=c(1,0,0))
  val<-forecast(fitteda,h=12)
  output$ar<-renderPlot({autoplot(dowjones_train_stationary) +
      autolayer(val, series="AR model", PI=FALSE) 
  })
  
  arima_model <- Arima(dowjones_train_stationary, order = c(2,1,1),include.drift = TRUE)
  fit1<-forecast(arima_model,h=12)
  
  output$Arima<-renderPlot({ autoplot(dowjones_train_stationary) +
      autolayer(fit1, series="Arima model", PI=FALSE) })
  
  
  
}
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')

shinyApp(ui, server)