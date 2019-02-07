library(VineCopula)
library(XML) 
library(quantmod)
library(PerformanceAnalytics)
library(RCurl)
library(copula)


all_combo<-list()
Symbols<- c("MMM", "GPC", "JNJ", "LOW", "PG")
for (i in 1:10)
 { 
    ticker<-combn(Symbols, 2, FUN=NULL, simplify=FALSE)
    log_returns= lapply(ticker[[i]], function(sym) {
    dailyReturn(na.omit(getSymbols(sym, from="2009-01-01", to="2018-12-31",auto.assign=FALSE)[,6]), type='log')
     })
    head(cbind(log_returns[[1]], log_returns[[2]]),2)

    portfolio<-cbind(do.call(cbind, log_returns))
    log_return_names=character(0)
    log_return_names<-c(log_return_names, paste0("LDR_",ticker[[i]][1]),
                                          paste0("LDR_",ticker[[i]][2]) 
                        )
    names(portfolio)<-c(log_return_names) 
    all_combo[[i]]<-portfolio


v <- pobs(as.matrix(cbind(all_combo[[i]][,1],all_combo[[i]][,2])))[,2]
u <- pobs(as.matrix(cbind(all_combo[[i]][,1],all_combo[[i]][,2])))[,1]
plot(u,v, xlab=ticker[[i]][1], main=c("Scatter plot of ", ticker[[i]][1], "by ", ticker[[i]][2]), ylab=ticker[[i]][2], col="blue")
abline(lm(u~v),col='red',lwd=2)
cor(u,v,method='spearman')

xhist <- hist(coredata(all_combo[[i]][,1]), breaks=30, plot=FALSE)
yhist <- hist(coredata(all_combo[[i]][,2]), breaks=30, plot=FALSE) 
top <- max(c(xhist$counts, yhist$counts)) 
xrange <- c(-0.1,0.1)
yrange <- c(-0.1,0.1)
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE) 
par(mar=c(3,3,1,1)) 
plot(coredata(all_combo[[i]][,1]), coredata(all_combo[[i]][,2]), main="Joint/marginal distribution", xlim=xrange, ylim=yrange, xlab=ticker[[1]][1], ylab=ticker[[1]][2]) 
par(mar=c(0,3,1,1)) 
barplot(xhist$counts,main=ticker[[i]][1], axes=FALSE, space=0) 
par(mar=c(3,0,1,1)) 
barplot(yhist$counts, main=ticker[[i]][2], axes=FALSE, space=0, horiz=TRUE) 


selectedCopula <- BiCopSelect(u,v,familyset=NA)
CopulaName=c("independence copula",
"Gaussian copula",
"Student t copula (t-copula)",
"Clayton copula", 
"Gumbel copula", 
"Frank copula", 
"Joe copula", 
"BB1 copula", 
"BB6 copula", 
"BB7 copula", 
"BB8 copula", 
"rotated Clayton copula (180 degrees; ``survival Clayton'')", 
"rotated Gumbel copula (180 degrees; ``survival Gumbel'')", 
"rotated Joe copula (180 degrees; ``survival Joe'')", 
"rotated BB1 copula (180 degrees; ``survival BB1'')", 
"rotated BB6 copula (180 degrees; ``survival BB6'')", 
"rotated BB7 copula (180 degrees; ``survival BB7'')", 
"rotated BB8 copula (180 degrees; ``survival BB8'')", 
"rotated Clayton copula (90 degrees)", 
"rotated Gumbel copula (90 degrees)", 
"rotated Joe copula (90 degrees)", 
"rotated BB1 copula (90 degrees)", 
"rotated BB6 copula (90 degrees)", 
"rotated BB7 copula (90 degrees)", 
"rotated BB8 copula (90 degrees)", 
"rotated Clayton copula (270 degrees)", 
"rotated Gumbel copula (270 degrees)", 
"rotated Joe copula (270 degrees)", 
"rotated BB1 copula (270 degrees)", 
"rotated BB6 copula (270 degrees)", 
"rotated BB7 copula (270 degrees)", 
"rotated BB8 copula (270 degrees)", 
"Tawn type 1 copula", 
"rotated Tawn type 1 copula (180 degrees)", 
"rotated Tawn type 1 copula (90 degrees)", 
"rotated Tawn type 1 copula (270 degrees)", 
"Tawn type 2 copula", 
"rotated Tawn type 2 copula (180 degrees)", 
"rotated Tawn type 2 copula (90 degrees)", 
"rotated Tawn type 2 copula (270 degrees)"
)


print(paste0("Most appropriate Copula for ", ticker[[i]][1]," and ", ticker[[i]][2]," is ", CopulaName[1+selectedCopula$family]))
selectedCopula$par
selectedCopula$par2

t.cop<-tCopula(dim=2)
set.seed(500)
m<-cbind(u,v)
fit<-fitCopula(t.cop, m, method='ml')
coeff<-coef(fit)
print (paste('Parameter for Copula of', ticker[[i]][1],"and", ticker[[i]][2], "=", "Rho:", round(as.numeric(coeff[1]),3), ", DF:", round(as.numeric(coeff[2]),3), sep=" "))

rho <- coef(fit)[1]
df <- coef(fit)[2]
persp(tCopula(dim=2,rho,df=df),main=c("Copula density for ", ticker[[i]][1], " and ", ticker[[i]][2]), xlab=ticker[[i]][1], ylab=ticker[[i]][2], dCopula)

}
