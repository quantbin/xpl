##################################################
## load time series close levels from files
load.close.levels<-function(syms){
  subfolders<-c(
    'nyse stocks/1', 'nyse stocks/2',
    'nasdaq stocks/1', 'nasdaq stocks/2',
    'nyse etfs',
    'nasdaq etfs',
    'nysemkt stocks',
    'nysemkt etfs'
  )
  close.levels<-NULL
  for(sym in syms){
    for(sf in subfolders){
      found<-FALSE
      p<-sub('SUB', sf, DATA_HOME)
      p<-paste(p, 'SYM.us.txt',sep='/')
      p<-sub('SYM', sym, p)
      if (file.exists(p)){
        found<-TRUE
        break
      }
    }
    if (!found) stop(paste('sym not found: ', sym))
    d<-read.zoo(p, sep=',', header=TRUE, format='%Y%m%d')
    if (is.null(close.levels)){
      close.levels<-d[,'Close']
    }else{
      close.levels<-merge(close.levels, d[,'Close'], all=FALSE)
    }
  }
  colnames(close.levels)<-syms
  return(close.levels)
}
##################################################
## calculate efficient frontier
mvo<-function(syms, close.levels,
              short='no', lookback.years='all',
              min.allocation='no', max.allocation='no',
              risk.premium.up=.5, risk.increment=.005){
  ## calculate returns
  returns<-NULL
  for(sym in syms){
    sym.returns<-close.levels[,sym]
    sym.returns<-diff(sym.returns)/sym.returns
    if (is.null(returns)){
      returns<-sym.returns
    }else{
      returns<-merge(returns, sym.returns, all=FALSE)
    }
  }
  ## how much history to take?
  if (is.numeric(lookback.years)){
    returns<-tail(returns, 252*lookback.years)
  }
  colnames(returns)<-syms
  n<-length(syms)
  ## prepare constraints
  ##   sum of weights=1
  Amat<-matrix(1, nrow=n)
  bvec<-1
  meq<-1
  ##   are shorts allowed?
  if ('no'==short){
    Amat<-cbind(Amat, diag(n)) #????
    bvec<-c(bvec, rep(0, n))
  }
  ##   concentration limits
  if ('no'!=max.allocation){
    if (max.allocation<0 | 1<max.allocation){
      stop('max.allocation must be between 0 and 1')
    }
    if (n*max.allocation<1){
      stop('num.assets * max.allocation is less than 1')
    }
    Amat<-cbind(Amat, -diag(n))
    bvec<-c(bvec, rep(-max.allocation, n))
  }
  if ('no'!=min.allocation){
    Amat<-cbind(Amat, diag(n))
    bvec<-c(bvec, rep(min.allocation, n))
  }
  ## calculate efficient frontier
  loops<-risk.premium.up/risk.increment
  loop<-1
  eff<-matrix(nrow=loops+1, ncol=n+3)
  colnames(eff)<-c(colnames(returns), 'stdDev', 'expRet', 'sharpe')
  Dmat<-cov(returns)
  for(risk.level in seq(from=0, to=risk.premium.up, by=risk.increment)){
    means<-colMeans(returns)
    dvec<-means*risk.level
    sol<-solve.QP(Dmat, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    eff[loop, 'stdDev']<-sqrt(252*sum(sol$solution*colSums(Dmat*sol$solution)))
    eff[loop, 'expRet']<-252*as.numeric(sol$solution%*%means)
    eff[loop, 'sharpe']<-eff[loop, 'expRet']/eff[loop, 'stdDev']
    eff[loop, 1:n]<-sol$solution
    loop<-loop+1
  }
  return(as.data.frame(eff))
}
##################################################
## backtest mvo
mvo.backtest<-function(syms, close.levels, 
                       short=short, 
                       risk.premium.up=risk.premium.up,
                       risk.increment=risk.increment,
                       lookback.years=lookback.years){
  weights<-NULL
  pl.dates<-matrix(nrow=52, ncol=1)
  class(pl.dates)<-'Date'
  pl.values<-matrix(nrow=52, ncol=1)
  loop<-1
  for(weeks.back in seq(from=52, to=0, by=-1)){
    if (0!=weeks.back){
      close.levels.weeks.back<-head(close.levels, -5*weeks.back)
    }else{
      close.levels.weeks.back<-close.levels
    }
    if (!is.null(weights)){
      ## calculate PL
      prices<-tail(close.levels.weeks.back, 1)
      pl.dates[loop]<-as.Date(index(prices), format="%Y-%m-%d")
      pl.values[loop]<-sum(as.vector(prices) * weights)
      loop<-loop+1
    }
    eff<-mvo(syms, close.levels.weeks.back, 
             short=short, 
             risk.premium.up=risk.premium.up,
             risk.increment=risk.increment,
             lookback.years=lookback.years
    )
    #bullet<-eff[eff$sharpe==max(eff$sharpe),]
    bullet<-head(eff[abs(eff$sharpe-max(eff$sharpe))<.0000000001,], 1)
    weights<-bullet[1:num.assets]
  }
  pl<-zoo(x=pl.values, order.by=pl.dates)
  return (pl)
}
