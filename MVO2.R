library(quadprog)
library(xts)
library(zoo)
## config
setwd("C:/src/R")
DATA_HOME<-'C:/opt/data/daily/us/SUB'
## api
source('./MVO2.functions.R')
## input params
syms<-c('jnk', 'gld', 'eem', 'spy', 'oil', 'bond')
lookback.years<-'all'
short<-'yes'
risk.premium.up<-0.1
risk.increment<-.01
## load data
close.levels<-load.close.levels(syms)
## optimize
eff<-mvo(syms, close.levels, 
         short=short, 
         risk.premium.up=risk.premium.up,
         risk.increment=risk.increment,
         lookback.years=lookback.years
)
## number of assets
num.assets<-length(syms)
## weights for all  risk levels
d<-eff[,1:num.assets]
## max sharpe portfolio
bullet<-head(eff[abs(eff$sharpe-max(eff$sharpe))<.0000000001,], 1)

## plot weights, efficient frontier and prices
if (is.numeric(lookback.years)){
  plot(tail(close.levels, 252*lookback.years))
}else{
  plot(close.levels)
}
par(mfrow=c(2,2))
## weights barplot
d1<-as.data.frame(lapply(d, function(x)ifelse(x<0,x,0)))
d2<-as.data.frame(lapply(d, function(x)ifelse(x>0,x,0)))
barplot(t(d1), ylab='weights', legend=colnames(d),  col=rainbow(num.assets), ylim=c(min(d), max(d)))
barplot(t(d2), add=TRUE, ylab='weights', legend=colnames(d),  col=rainbow(num.assets), ylim=c(min(d), max(d)))
abline(v=as.numeric(row.names(bullet)),col="blue")
## eff frontier chart
plot(eff$stdDev, eff$expRet, type='l')
points(bullet$stdDev, bullet$expRet)

## VaR
## max sharpe weights
weights<-bullet[1:num.assets]
## today's prices
prices<-tail(close.levels, 1)
## optimal portfolio current pnl
pl<-sum(as.vector(prices) * weights)
scenarios.rel.returns<-diff(close.levels)/close.levels
scenarios.prices<-zoo(prices, index(close.levels)) * (1+scenarios.rel.returns)
scenarios.portfolio<-zoo(weights, index(scenarios.prices)) * scenarios.prices
scenarios.pl<-rowSums(scenarios.portfolio)
var<-quantile(scenarios.pl, probs=c(.05))
## plot VaR
hist(scenarios.pl, breaks=100, xlab='Scenarios P&L', main='VaR')
abline(v=var,col="red")
points(pl, 0, pch=19, col='blue')
legend('topright', c('Current P&L', '5% VaR'), fill=c("blue", "red"))

## backtest
pl<-mvo.backtest(syms, close.levels, 
                 short=short, 
                 risk.premium.up=risk.premium.up,
                 risk.increment=risk.increment,
                 lookback.years=lookback.years)
## load benchmark
spy.levels<-load.close.levels(c('spy', 'spy'))
pl.bench<-merge(zoo(pl), spy.levels[,1], all=FALSE)
colnames(pl.bench)<-c('pl', 'spy')
## rescale benchmark
pl.bench[,'spy']<-pl.bench[,'spy']/coredata(pl.bench[1,'spy'])
pl.bench[,'pl']<-pl.bench[,'pl']/coredata(pl.bench[1,'pl'])
plot(pl.bench[,'spy'], type='l', col='red', ylim=c(min(pl.bench), max(pl.bench)))
lines(pl.bench[,'pl'], col='blue')
abline(h=1)
