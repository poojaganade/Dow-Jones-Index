## Dow Jones Index Definition 
#### What is Dow Jones Index?

- The Dow Jones Industrial Average (DJIA) is an index that tracks 30 large, publicly-owned companies trading on the New York Stock Exchange   (NYSE) and the NASDAQ. 

#### Understanding the Dow Jones Industrial Average
- The Dow Jones Industrial Average was designed to serve as a proxy for the broader U.S. economy.
- As the economy changes over time, so does the composition of the index. The Dow typically makes changes when a company becomes less 
  representative of the economy. 
  
## Forecasting Dow Jones Index

#### Dataset
- Downloaded data from Yahoo finance for dow jones index from 2000 to 2018.
- The data-set contains five Dow Jones values for each day - open, high, low, close, and adjusted close.
- We used adjusted close price to the raw close price, because it accounts for corporate actions such as dividents, stock splits etc.

#### Forecasting applied
- We used 2000 to 2017 as training data set and forecasted the Dow Jones index in 2018.
- We applied multiple forecasting models - ranging from simple statistical models such as naive method, drift method etc. to more complex
  forecasting models such as ARIMA model, Holt model etc.
- Auto regressive model turned out to be the best forecasting model since it generated least error values in 4 out of 7 error types.
- For visualization, we used Shinny app, which is an interactive approach that helps us explain data analysis in form of a story using R.

