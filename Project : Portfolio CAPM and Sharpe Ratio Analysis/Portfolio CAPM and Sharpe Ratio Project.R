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


