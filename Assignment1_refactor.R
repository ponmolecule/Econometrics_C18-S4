
#################################################################################################################
#1. Download data for 5 stocks starting for period starting on 1-1-2009 till date.We use an R method to fetch the 
#data directly.
#################################################################################################################

#################################################################################################################
#Download and calculate daily and log returns of the 5 stocks over the period under study
#################################################################################################################

library(XML) 
library(quantmod)
library(PerformanceAnalytics)
library(RCurl)


Symbols<- c("MMM", "GPC", "JNJ", "LOW", "PG")
chooseStocks<-function(Symbols){
for (i in 1:1)
 { 
    ticker<-combn(Symbols, length(Symbols), FUN=NULL, simplify=FALSE)
    Stocks = lapply(ticker[[1]], function(sym) {
    getSymbols(sym, from="2009-01-01", auto.assign=FALSE)[,6]
     })
    returns = lapply(ticker[[1]], function(sym) {
    dailyReturn(na.omit(getSymbols(sym, from="2009-01-01", auto.assign=FALSE)[,6]))
     })
    log_returns= lapply(ticker[[1]], function(sym) {
    dailyReturn(na.omit(getSymbols(sym, from="2009-01-01", auto.assign=FALSE)[,6]), type='log')
     })

    portfolio<-cbind(do.call(cbind, Stocks), do.call(cbind, returns), do.call(cbind, log_returns))
    names=character(0)
    return_names=character(0)
    log_return_names=character(0)
    for (i in 1:5)
    {
     names<-c(names, paste0(ticker[[1]][i],"_AdjClose")) 
     return_names<-c(return_names, paste0(ticker[[1]][i],"_daily_return"))
     log_return_names<-c(log_return_names, paste0("LDR_",ticker[[1]][i]))
    }  
    names(portfolio)<-c(names, return_names, log_return_names)
  }
 return(portfolio)
}
portfolio<-chooseStocks(Symbols)
DailyData<-data.frame(Date=index(portfolio), coredata(portfolio))
head(DailyData,10)



DailyData<-chooseStocks(Symbols)

DailyData<-data.frame(Date=index(DailyData), coredata(DailyData))

head(DailyData,10)