# WQU Econometrics
# Group Project Assignment 1
#
# This part does the Price Graph and the  Log Return Time Plots


EnsurePackage<-function(x){
 x<-as.character(x)
 if (!require(x, character.only=TRUE))
 {
  install.packages(pkgs=x, repos="http://cran.r-project.org")
   require(x, character.only=TRUE)
 }
}

EnsurePackage("XML")
EnsurePackage("RCurl")
EnsurePackage("quantmod")
EnsurePackage("PerformanceAnalytics") 
EnsurePackage("tidyverse")
EnsurePackage("ggplot2")
EnsurePackage("stats")
EnsurePackage("tseries")
EnsurePackage("forecast")
EnsurePackage("kdensity")
EnsurePackage("corrplot")
EnsurePackage("readxl")



################################################################################################################################# 
# 1. Download data for 5 stocks starting for period starting on 1-1-2009 till date.We use an R method to fetch the data directly.
#
# 2. Calculate daily and log returns of the 5 stocks over the period under study
#################################################################################################################################

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
DailyData<-chooseStocks(Symbols)
DailyData<-data.frame(Date=index(DailyData), coredata(DailyData))
head(DailyData,10)

# View time plot of closing prices
#ggplot(data = DailyData, geom_line(mapping = aes(x = Date,y = MMM_AdjClose, color="darkblue"),
#         labs(x = "Year", y = "Daily Closing Price") 
         
ggplot(data = DailyData, aes(x = Date, y = MMM_AdjClose))+
  ggtitle("Daily Adjusted Close Prices  1-1-2009 to 8-31-2018") + 
  labs(x = "Year", y = "Stock Adjusted Close Price in Dollars") +
  geom_line(mapping = aes(x = Date,y = MMM_AdjClose), color = "darkblue") +
  geom_line(mapping = aes(x = Date,y = GPC_AdjClose), color="red") +
  geom_line(mapping = aes(x = Date,y = JNJ_AdjClose), color="darkgreen") +
  geom_line(mapping = aes(x = Date,y = LOW_AdjClose), color="black") +
  geom_line(mapping = aes(x = Date,y = PG_AdjClose), color="orange") 
# 
ggsave("D:/WQU/MSCFE/Econometrics_v2/Group Work/Project1 Stock Prices.png")
# 
#   Need to calculate summary statistics for graphd here
# 
# # Generate plots of the log returns of all 5  assets
# 

############################################################################################
# Generate scatter plots of the log returns of all 5 stocks over the period under study
############################################################################################

ggplot(data = DailyData) + 
    geom_point(mapping = aes(x = Date,y = LDR_MMM), color="darkblue") +
    labs(x = "Year", y = "Log Daily Returns", title ="MMM - Log Daily Returns")
#
ggsave("D:/WQU/MSCFE/Econometrics_v2/Group Work/MMM_log_returns.png")

# 
ggplot(data = DailyData) + 
  geom_point(mapping = aes(x = Date,y = LDR_GPC), color="red") +
  labs(x = "Year", y = "GPC - Log Daily Returns", title = "GPC - Log Daily Returns")
# 
ggsave("D:/WQU/MSCFE/Econometrics_v2/Group Work/GPC_log_returns.png")
# 
ggplot(data = DailyData) + 
  geom_point(mapping = aes(x = Date,y = LDR_JNJ), color="darkgreen") +
  labs(x = "Year", y = "JNJ - Log Daily Returns", title= "JNJ - Log Daily Returns")

ggsave("D:/WQU/MSCFE/Econometrics_v2/Group Work/JNJ_log_returns.png")
# 
ggplot(data = DailyData) + 
  geom_point(mapping = aes(x = Date,y = LDR_LOW), color="black") +
  labs(x = "Year", y = "LOW - Log Daily Returns", title = "LOW - Log Daily Returns")
# 
ggsave("D:/WQU/MSCFE/Econometrics_v2/Group Work/LOW_log_returns.png")
# 
ggplot(data = DailyData) + 
  geom_point(mapping = aes(x = Date,y = LDR_PG), color="orange") +
  labs(x = "Year", y = "PG - Log Daily Returns", title="PG - Log Daily Returns")

ggsave("D:/WQU/MSCFE/Econometrics_v2/Group Work/PG_log_returns.png")


#############################################################################################################
# Generate histogram and kernel density plots of the log returns of all 5 stocks over the period under study
#############################################################################################################


##################### MMM #############################

# Generate MMM Histogram with 64 bins
hist(DailyData$LDR_MMM, breaks=64, freq=F, main = "MMM Kdensity of Log Returns", xlab="Daily Log Returns",
     xlim=c(-.06,.06), ylim=c(0,65))

# fit a kernel density estimate
kde_MMM = kdensity(DailyData$LDR_MMM, start = "normal")

# plot the KDE
lines(kde_MMM, col ="blue")

# plot the closest Normal distribution
lines(kde_MMM, plot_start = T, col="red")

legend("topright", c("Kernel Estimate", "Normal"), 
       lty = c(1,1), lwd = c(1,1), col = c("blue", "red"))

################# GPC #######################

# Generate GPC Histogram with 64 bins
hist(DailyData$LDR_GPC, breaks=64, freq=F, main = "GPC Kdensity of Log Returns", xlab="Daily Log Returns",
     xlim=c(-.06,.06), ylim=c(0,65))

# fit a kernel density estimate
kde_GPC = kdensity(DailyData$LDR_GPC, start = "normal")

# plot the KDE
lines(kde_GPC, col ="blue")
# plot the closest Normal distribution
lines(kde_GPC, plot_start = T, col="red")

legend("topright", c("Kernel Estimate", "Normal"), 
       lty = c(1,1), lwd = c(1,1), col = c("blue", "red"))

################ JNJ ########################
# Generate JNJ Histogram with 64 bins
hist(DailyData$LDR_JNJ, breaks=64, freq=F, main = "JNJ Kdensity of Log Returns", xlab="Daily Log Returns",
          xlim=c(-.06,.06), ylim=c(0,65))

# fit a kernel density estimate
kde_JNJ = kdensity(DailyData$LDR_JNJ, start = "normal")

# plot the KDE
lines(kde_JNJ, col ="blue")
# plot the closest Normal distribution
lines(kde_JNJ, plot_start = T, col="red")

legend("topright", c("Kernel Estimate", "Normal"), 
       lty = c(1,1), lwd = c(1,1), col = c("blue", "red"))

################### LOW #####################
# Generate LOW Histogram with 64 bins
hist(DailyData$LDR_LOW, breaks=64, freq=F, main = "LOW Kdensity of Log Returns", xlab="Daily Log Returns",
     xlim=c(-.06,.06), ylim=c(0,65))

# fit a kernel density estimate
kde_LOW = kdensity(DailyData$LDR_LOW, start = "normal")

# plot the KDE
lines(kde_LOW, col ="blue")
# plot the closest Normal distribution
lines(kde_LOW, plot_start = T, col="red")

legend("topright", c("Kernel Estimate", "Normal"), 
       lty = c(1,1), lwd = c(1,1), col = c("blue", "red"))

################### PG #####################
# Generate PG Histogram with 64 bins
hist(DailyData$LDR_PG, breaks=64, freq=F, main = "PG Kdensity of Log Returns", xlab="Daily Log Returns",
     xlim=c(-.06,.06), ylim=c(0,65))

# fit a kernel density estimate
kde_PG = kdensity(DailyData$LDR_PG, start = "normal")

# plot the KDE
lines(kde_PG, col ="blue")
# plot the closest Normal distribution
lines(kde_PG, plot_start = T, col="red")

legend("topright", c("Kernel Estimate", "Normal"), 
       lty = c(1,1), lwd = c(1,1), col = c("blue", "red"))


############################################################################################################################
# Generate Empirical Cumulative density function and QQ-plot of the log returns of all 5 stocks over the period under study
#############################################################################################################################

# Fit Empirical CDF to data
ecdf_MMM = ecdf(DailyData$LDR_MMM)
ecdf_GPC = ecdf(DailyData$LDR_GPC)
ecdf_JNJ = ecdf(DailyData$LDR_JNJ)
ecdf_LOW = ecdf(DailyData$LDR_LOW)
ecdf_PG =  ecdf(DailyData$LDR_PG)

# compute Statistics of empirical distributions
mean_MMM = mean(DailyData$LDR_MMM)
mean_GPC = mean(DailyData$LDR_GPC)
mean_JNJ = mean(DailyData$LDR_JNJ)
mean_LOW = mean(DailyData$LDR_LOW)
mean_PG = mean(DailyData$LDR_PG)

sd_MMM = sd(DailyData$LDR_MMM)
sd_GPC = sd(DailyData$LDR_GPC)
sd_JNJ = sd(DailyData$LDR_JNJ)
sd_LOW = sd(DailyData$LDR_LOW)
sd_PG = sd(DailyData$LDR_PG)

min_MMM = min(DailyData$LDR_MMM)
min_GPC = min(DailyData$LDR_GPC)
min_JNJ = min(DailyData$LDR_JNJ)
min_LOW = min(DailyData$LDR_LOW)
min_PG = min(DailyData$LDR_PG)

max_MMM = max(DailyData$LDR_MMM)
max_GPC = max(DailyData$LDR_GPC)
max_JNJ = max(DailyData$LDR_JNJ)
max_LOW = max(DailyData$LDR_LOW)
max_PG = max(DailyData$LDR_PG)

# Plot the step function MMM
plot(ecdf_MMM, verticals = T, do.p = F, main = "MMM EDF and Normal CDF")

# generate the closest equivalent CDf for the normal distributons
# with mean and stdevs of the empirical distributions

q_vector_MMM = seq(from = min_MMM, to = max_MMM,  by = .005)
norm_cdf_MMM = pnorm(q_vector_MMM, mean = mean_MMM, sd = sd_MMM)

# add the Normal CDF plot
lines(q_vector_MMM, norm_cdf_MMM, col = "red")

legend("bottomright", c("Empirical Distribution", "Normal Distribution" ),
       lty = c(1,1), lwd = c(1,1), col = c("black", "red"))

# Plot the step function GPC
plot(ecdf_GPC, verticals = T, do.p = F, main = "GPC EDF and Normal CDF")

# generate the closest equivalent CDf for the normal distributons
# with mean and stdevs of the empirical distributions

q_vector_GPC = seq(from = min_GPC, to = max_GPC,  by = .005)
norm_cdf_GPC = pnorm(q_vector_GPC, mean = mean_GPC, sd = sd_GPC)

# add the Normal CDF plot
lines(q_vector_GPC, norm_cdf_GPC, col = "red")

legend("bottomright", c("Empirical Distribution", "Normal Distribution" ),
       lty = c(1,1), lwd = c(1,1), col = c("black", "red"))

# Plot the step function JNJ
plot(ecdf_JNJ, verticals = T, do.p = F, main = "JNJ EDF and Normal CDF")

# generate the closest equivalent CDf for the normal distributons
# with mean and stdevs of the empirical distributions

q_vector_JNJ = seq(from = min_JNJ, to = max_JNJ,  by = .005)
norm_cdf_JNJ = pnorm(q_vector_JNJ, mean = mean_JNJ, sd = sd_JNJ)

# add the Normal CDF plot
lines(q_vector_JNJ, norm_cdf_JNJ, col = "red")

legend("bottomright", c("Empirical Distribution", "Normal Distribution" ),
       lty = c(1,1), lwd = c(1,1), col = c("black", "red"))

# Plot the step function LOW
plot(ecdf_LOW, verticals = T, do.p = F, main = "LOW EDF and Normal CDF")

# generate the closest equivalent CDf for the normal distributons
# with mean and stdevs of the empirical distributions

q_vector_LOW = seq(from = min_LOW, to = max_LOW,  by = .005)
norm_cdf_LOW = pnorm(q_vector_LOW, mean = mean_LOW, sd = sd_LOW)

# add the Normal CDF plot
lines(q_vector_LOW, norm_cdf_LOW, col = "red")

legend("bottomright", c("Empirical Distribution", "Normal Distribution" ),
       lty = c(1,1), lwd = c(1,1), col = c("black", "red"))

# Plot the step function PG
plot(ecdf_PG, verticals = T, do.p = F, main = "PG EDF and Normal CDF")

# generate the closest equivalent CDf for the normal distributons
# with mean and stdevs of the empirical distributions

q_vector_PG = seq(from = min_PG, to = max_PG,  by = .005)
norm_cdf_PG = pnorm(q_vector_PG, mean = mean_PG, sd = sd_PG)

# add the Normal CDF plot
lines(q_vector_PG, norm_cdf_PG, col = "red")

legend("bottomright", c("Empirical Distribution", "Normal Distribution" ),
       lty = c(1,1), lwd = c(1,1), col = c("black", "red"))

#########  QQ PLOTS ####################
#
# Standardise REturns for to havve zero mean and unit variance

MMM_std = (DailyData$LDR_MMM - mean_MMM)/sd_MMM
GPC_std = (DailyData$LDR_GPC - mean_GPC)/sd_GPC
JNJ_std = (DailyData$LDR_JNJ - mean_JNJ)/sd_JNJ
LOW_std = (DailyData$LDR_LOW - mean_LOW)/sd_LOW
PG_std = (DailyData$LDR_PG - mean_PG)/sd_PG

# Produce QQ Plot MMM
qqnorm(MMM_std, main = "MMM Normal Q-Q Plot",
       plot.it = T, datax=T)
# Add a line from 25th to 75th quantiles
qqline(MMM_std, datax = F, 
       distribution = qnorm, probs = c( 0.25, 0.75), qtype = 7)


# Produce QQ Plot GPC
qqnorm(GPC_std, main = "GPC Normal Q-Q Plot",
       plot.it = T, datax=T)
# Add a line from 25th to 75th quantiles
qqline(GPC_std, datax = F, 
       distribution = qnorm, probs = c( 0.25, 0.75), qtype = 7)

# Produce QQ Plot JNJ
qqnorm(JNJ_std, main = "JNJ Normal Q-Q Plot",
       plot.it = T, datax=T)
# Add a line from 25th to 75th quantiles
qqline(JNJ_std, datax = F, 
       distribution = qnorm, probs = c( 0.25, 0.75), qtype = 7)

# Produce QQ Plot LOW
qqnorm(LOW_std, main = "LOW Normal Q-Q Plot",
       plot.it = T, datax=T)
# Add a line from 25th to 75th quantiles
qqline(LOW_std, datax = F, 
       distribution = qnorm, probs = c( 0.25, 0.75), qtype = 7)

# Produce QQ Plot PG
qqnorm(PG_std, main = "PG Normal Q-Q Plot",
       plot.it = T, datax=T)
# Add a line from 25th to 75th quantiles
qqline(PG_std, datax = F, 
       distribution = qnorm, probs = c( 0.25, 0.75), qtype = 7)


# # create scatterplots of log daily returns
# Set 1 MMM
plot(DailyData$LDR_MMM, DailyData$LDR_GPC,xlab="MMM", ylab="GPC") 
plot(DailyData$LDR_MMM, DailyData$LDR_JNJ,xlab="MMM", ylab="JNJ") 
plot(DailyData$LDR_MMM, DailyData$LDR_LOW,xlab="MMM", ylab="LOW") 
plot(DailyData$LDR_MMM, DailyData$LDR_PG,xlab="MMM", ylab="PG") 

# Set 2
plot(DailyData$LDR_GPC, DailyData$LDR_JNJ,xlab="GPC", ylab="JNJ") 
plot(DailyData$LDR_GPC, DailyData$LDR_LOW,xlab="GPC", ylab="LOW") 
plot(DailyData$LDR_GPC, DailyData$LDR_PG,xlab="GPC", ylab="PG") 

# set 3
plot(DailyData$LDR_JNJ, DailyData$LDR_LOW,xlab="JNJ", ylab="LOW") 
plot(DailyData$LDR_JNJ, DailyData$LDR_PG,xlab="JNJ", ylab="PG") 
# Set 4
plot(DailyData$LDR_LOW, DailyData$LDR_PG, xlab="LOW", ylab="PG") 

##############################################
# Correlation Matrix

# Creata a data set with just data to be correlated
# i.e. Log Returns
CorData = cbind(DailyData$LDR_MMM, DailyData$LDR_GPC, 
                DailyData$LDR_JNJ, DailyData$LDR_LOW, DailyData$LDR_PG)

pairs(CorData, labels = c("MMM", "GPC", "JNJ", "LOW","PG"))
#Cordata = rbind(c("MMM", "GPC", "JNJ" , "LOW", "PG"), CorData)
# Calculate Correlation Matrix
cor(CorData)

# Create data for Correlation Plot
for_cor_plot = cor(CorData)

# Plot it
corrplot(for_cor_plot)

#################################
# Time series Analysis
#################################

LDR_MMM_TS = ts(DailyData$LDR_MMM)
LDR_GPC_TS = ts(DailyData$LDR_GPC)
LDR_JNJ_TS = ts(DailyData$LDR_JNJ)
LDR_LOW_TS = ts(DailyData$LDR_LOW)
LDR_PG_TS = ts(DailyData$LDR_PG)
#
#head(LDR_MMM_TS) # For Testing
# 
# 
# 
# # Construct the ACF 
#
acf(LDR_MMM_TS)
acf(LDR_GPC_TS)
acf(LDR_JNJ_TS)
acf(LDR_LOW_TS)
acf(LDR_PG_TS)
# # acf(CPI_percent_change_truncated)
# 
# # Now Construct the partial autocorrelation of the two objects
# 
#
pacf(LDR_MMM_TS)
pacf(LDR_GPC_TS)
pacf(LDR_JNJ_TS)
pacf(LDR_LOW_TS)
pacf(LDR_PG_TS)
# 
# 
# conduct formal staitonary tests using
# Augmented Dickey Fuller (adf) test
# test for a null of non stationary v stationsry
# 
adf.test(LDR_MMM_TS)
adf.test(LDR_GPC_TS)
adf.test(LDR_JNJ_TS)
adf.test(LDR_LOW_TS)
adf.test(LDR_PG_TS)

# 
# Conduct KPSS tests
# This is null stationary v no stationary

kpss.test(LDR_MMM_TS)
kpss.test(LDR_GPC_TS)
kpss.test(LDR_JNJ_TS)
kpss.test(LDR_LOW_TS)
kpss.test(LDR_PG_TS)

# 
# # Fit a sample AR model with 12 Lags, no differencing and no moving average
# # terms - i,e, an ARIMA(12,0,0) MODEL
# 
AR_model_MMM = arima(window(LDR_MMM_TS),
                 order=c(1,2,2), method = "ML")
summary(AR_model_MMM)

AR_model_GPC = arima(window(LDR_GPC_TS),
                     order=c(1,2,1), method = "ML")
summary(AR_model_GPC)

AR_model_JNJ = arima(window(LDR_JNJ_TS),
                     order=c(1,2,4), method = "ML")
summary(AR_model_JNJ)

AR_model_LOW = arima(window(LDR_LOW_TS),
                     order=c(1,2,4), method = "ML")
summary(AR_model_LOW)

AR_model_PG = arima(window(LDR_PG_TS),
                     order=c(1,2,2), method = "ML")
summary(AR_model_PG)

# ets(LDR_MMM_TS, model = "ZZZ")
# # Conduct a Box TEst
# 
Box.test(AR_model_MMM$residuals, lag = 1)
Box.test(AR_model_GPC$residuals, lag = 1)
Box.test(AR_model_JNJ$residuals, lag = 1)
Box.test(AR_model_LOW$residuals, lag = 1)
Box.test(AR_model_PG$residuals, lag = 1)
# 
# 
# AR_model = ar(window(US_GDP_Data_TS, start=1960, end= 2017),
#               order = 12, method = "mle")
AR_forecast_MMM = predict(AR_model_MMM,n.ahead = 72, se.fit = TRUE)
AR_forecast_GPC = predict(AR_model_GPC,n.ahead = 72, se.fit = TRUE)
AR_forecast_JNJ = predict(AR_model_JNJ,n.ahead = 72, se.fit = TRUE)
AR_forecast_LOW = predict(AR_model_LOW,n.ahead = 72, se.fit = TRUE)
AR_forecast_PG = predict(AR_model_PG,n.ahead = 72, se.fit = TRUE)
#head(AR_forecast_MMM)
# # Plot predicted values and SE of predicted values
# 
plot(window(LDR_MMM_TS, start=2000))
lines(AR_forecast_MMM$pred, col="blue")
lines(AR_forecast_MMM$pred + 2*AR_forecast_MMM$se, col="cornflowerblue", lty='dashed')
lines(AR_forecast_MMM$pred - 2*AR_forecast_MMM$se, col="cornflowerblue", lty='dashed')
# 
plot(window(LDR_GPC_TS, start=2000))
lines(AR_forecast_GPC$pred, col="blue")
lines(AR_forecast_GPC$pred + 2*AR_forecast_GPC$se, col="cornflowerblue", lty='dashed')
lines(AR_forecast_GPC$pred - 2*AR_forecast_GPC$se, col="cornflowerblue", lty='dashed')
# 
plot(window(LDR_JNJ_TS, start=2000))
lines(AR_forecast_JNJ$pred, col="blue")
lines(AR_forecast_JNJ$pred + 2*AR_forecast_JNJ$se, col="cornflowerblue", lty='dashed')
lines(AR_forecast_JNJ$pred - 2*AR_forecast_JNJ$se, col="cornflowerblue", lty='dashed')
# 
plot(window(LDR_LOW_TS, start=2000))
lines(AR_forecast_LOW$pred, col="blue")
lines(AR_forecast_LOW$pred + 2*AR_forecast_LOW$se, col="cornflowerblue", lty='dashed')
lines(AR_forecast_LOW$pred - 2*AR_forecast_LOW$se, col="cornflowerblue", lty='dashed')
# 
plot(window(LDR_PG_TS, start=2000))
lines(AR_forecast_PG$pred, col="blue")
lines(AR_forecast_PG$pred + 2*AR_forecast_PG$se, col="cornflowerblue", lty='dashed')
lines(AR_forecast_PG$pred - 2*AR_forecast_PG$se, col="cornflowerblue", lty='dashed')
