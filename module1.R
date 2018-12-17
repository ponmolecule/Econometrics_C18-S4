# Basic Time Plots:
# Load necessary packages
library(tidyverse)
library(ggplot2)
# view time plots of the closing price of Microsoft:
ggplot(data = FinData) + geom_line(mapping = aes(x = Date,y = MSFT),color = "darkblue") + labs(x = "year", y = "Microsoft - Daily Closing Price")
ggsave("D:/WQU/MSCFE/Econometrics_v2/Microsoft_closing_price.png")
# generate plots for each of the log return graphs
of the four assets
ggplot(data = FinData)+ geom_line(mapping = aes(x = Date,y = APPL_lr), color = "darkred") + labs(x = "year",y = "Apple - Log Daily Return")
ggsave("C:/R_illustration/Apple_log_returns.png")
ggplot(data = FinData) + geom_line(mapping = aes(x = Date,y = INTC_lr), color ="darkgreen") + labs(x = "year",y = "Intel - Log Daily Return")
ggsave("C:/R_illustration/Intel_log_returns.png")

ggplot(data = FinData)
+ geom_line(mapping = aes(x = Date, y = IBM_lr),
color = “darkcyan”)
+labs(x = “year”,y = “IBM - Log Daily Return”)
ggsave(“C:/R_illustration/IBM_log_returns.png”)