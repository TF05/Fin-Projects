#This model is to create a portfolio and its 

#Load the required packages
library(PerformanceAnalytics)
library(quantmod)

#First we need to create a list of stocks to be in our portfolio
tickers<-c("TSLA", "MSFT", "AAPL", "NVDA")
#We need to assign weights to these stocks
w<-c(0.25,0.25,0.25,0.25)
#Nasdaq index ticker, but can be any popular index
index_ticker<-c("^IXIC")
#T-bill for Risk Free Rate, We are using three month treasury bill, adjusted monthly
t_bill<-"TB3MS"


#We now need to get daily stock prices from Yahoo Finance
PortfolioPrices<-NULL
for(i in tickers) {
  PortfolioPrices<- cbind(PortfolioPrices, 
                          getSymbols(i,periodicity = "monthly", from = Sys.Date()-1825, to = Sys.Date(),auto.assign = FALSE)[,4])
}

#We need our index data and t-bill data as well
index_ticker<-getSymbols(index_ticker,periodicity = "monthly", from = Sys.Date()-1825, to = Sys.Date(),auto.assign = FALSE)
#The tbill data needs to be filtered to match the portfolio's 5yr historic data, this will be done later
t_bill<-getSymbols(t_bill, src = "FRED", auto.assign = FALSE)


#Returns of each stock
Returns_of_Stock<-Return.calculate(PortfolioPrices)
Returns_of_Stock<-na.omit(Returns_of_Stock)
#Returns of index 
Returns_of_Index<-Return.calculate(index_ticker[,4])
Returns_of_Index<-na.omit(Returns_of_Index)
#Filtering the T-Bill for the Required Dates
t_bill<-merge.xts(Returns_of_Index, t_bill/100, join = "left")
t_bill<-na.locf(t_bill)
t_bill<-t_bill[,-1]


#Portfolio-Returns
Portfolio_Returns<-Return.portfolio(Returns_of_Stock, weights = w, geometric = FALSE, verbose = TRUE)

#Beta of Portfolio, Method 1, Regress Stock Returns on Index Returns
Beta_1<-lm(Portfolio_Returns$returns~Returns_of_Index)
summary(Beta_1)
#Beta of Portfolio, Method 2, Covariance Method
Beta_2<-cov(Portfolio_Returns$returns,Returns_of_Index)/var(Returns_of_Index)
print(Beta_2)
#Beta of Portfolio, Method 3, Formula from Performance Analytics
Beta_3<-BetaCoVariance(Portfolio_Returns$returns, Returns_of_Index)
print(Beta_3)

#Proceed if all three values are the same. 


#We also want to measure the Sharpe Ratio
Sharpe_Ratio<-((mean(Portfolio_Returns$returns-t_bill))/sd(Portfolio_Returns$returns))
Annualized_S_R<-Sharpe_Ratio*sqrt(12)
X<-SharpeRatio.annualized(Portfolio_Returns$returns, t_bill, geometric = FALSE)
#We have also demonstrated the different methods of getting the sharpe ratio
print(Annualized_S_R)
#We will use the function to see if the results are the same 
Annual_SR_Function<-SharpeRatio.annualized(Portfolio_Returns$returns, t_bill, geometric = FALSE)
print(Annual_SR_Function)
#We have also demonstrated the different methods of getting the Sharpe ratio

#Jensens Alpha
CAPM.jensenAlpha(Portfolio_Returns$returns["2021-01-01/"], Returns_of_Index["2021-01-01/"], t_bill["2021-01-01/"])

#Jensens Alpha, This calculate the excess return or loss incomparison to the benchmark index. 
Jen_Alpha<-CAPM.jensenAlpha(Portfolio_Returns$returns["2021-01-01/"], Returns_of_Index["2021-01-01/"], t_bill["2021-01-01/"])
print(Jen_Alpha)
