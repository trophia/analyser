library(MASS) #For fitdistr
library(SuppDists) #For inverse Gaussian distibution : dinvGauss and qinvGauss
library(actuar) #For Log-logistic distributuion : dllogis and qllogis
library(survival) #For survreg
#library(VGAM) #For vglm

Distributioner <- Worker$proto(
  label = "Distributioner",
  data = NULL,
  formula = NULL
)

Distributioner$new <- function(.,data,formula=catch~fyear+month+area+vessel){
  inst = .$proto()
  inst$data = data
  inst$formula = formula
  inst
}

Distributioner$calc <- function(.,dists = c('gamma','inverse.gaussian','lognormal','log.logistic','weibull')){
  data = subset(.$data,catch>0 & is.finite(catch))
  #Scale by sd so that distributions fit better
  .$var = data$catch
  .$var = .$var/sd(.$var)
  #Remove very small and very large value because they can cause
  #the fitting of distributions to fail
  .$var = .$var[log(.$var)>-6 & log(.$var)<6]
  
  errorCapture = function(error){
    substr(capture.output(print(error)),1,100)
  }

  .$dists = list()
  for(dist in dists){
    
    fit = tryCatch(
      fitdistr(.$var,
        densfun = switch(dist,
        	gaussian = "normal",
        	inverse.gaussian = dinvGauss,
        	log.logistic = dllogis,
        	dist
        ),
        start = switch(dist,
        	inverse.gaussian = list(nu = 1, lambda = 1),
        	log.logistic = list(shape = 1, rate = 1),
        	NULL
        )
      ),
      error=errorCapture
    )
    
    survregFormula = update(.$formula,Surv(.)~.)
    model = tryCatch(
      switch(dist,
    	  gaussian = glm(.$formula,data=data,family=gaussian(link='log')),
    	  gamma = glm(.$formula,data=data,family=Gamma(link='log')),
    	  inverse.gaussian = glm(.$formula,data=data,family=inverse.gaussian(link='log')),
    	  lognormal = survreg(survregFormula,data=data,dist='lognormal'),
    	  weibull = survreg(survregFormula,data=data,dist='weibull'),
    	  log.logistic = survreg(survregFormula,data=data,dist='loglogistic')
    	),
      error=errorCapture
    )
    
    type = class(model)[1]
    
    logLike = tryCatch(
      switch(type,
      	'glm' = logLik(model),
      	'survreg' = model$loglik[2],
      ),
      error=errorCapture
    )
  
    aic = tryCatch(
      switch(type,
       'glm' = AIC(model),
       'survreg' = extractAIC(model)[2],
      ),
      error=errorCapture
    )
    
    .$dists[[dist]] = list(fit=fit,model=model,logLike=logLike,aic=aic)
  }
}

Distributioner$meanVariancePlot <- function(.){
  do = function(vars,row,col){
    data = ddply(.$data,vars,function(sub)with(sub,data.frame(mean=mean(catch,na.rm=T),var=var(catch,na.rm=T),n=nrow(sub))))
    data = subset(data,n>=10 & var>0)
    coeffs = summary(lm(log(var)~log(mean),data))$coef
    p = ggplot(data,aes(x=log(mean),y=log(var)))+geom_point(aes(size=n),shape=1,alpha=0.3)+scale_area()+
      geom_abline(intercept=coeffs[1,1],slope=coeffs[2,1])+
      geom_text(aes(x=x,y=y,label=label),data=data.frame(x=min(log(data$mean)),y=max(log(data$var)),label=paste(round(coeffs[2,1],2),' (',round(coeffs[2,2],2),')',sep='')),size=3.5,hjust=0)+
      opts(axis.title.x=theme_text(size=10),axis.title.y=theme_text(size=10,angle=90),title=paste(vars,collapse='*'),plot.title=theme_text(size=10))+
      labs(size='')
    print(p,vp=subplot(row,col))
  }
  dev.new(width=16/2.54,height=24/2.54)
  vplayout(3,1)
  do(c('fyear','month'),1,1)
  do(c('fyear','month','area'),2,1)
  do(c('fyear','month','area','vessel'),3,1)
  Figure(
    "Distributioner.meanVariancePlot",
    "Mean-variance plots at for alternative strata definitions. Area of circles is proporiotal to the number of records in each stratum. Lines indicate the unweighted fit to the points and
    values in upper-left are the estimated slope and standard error. Only strata where n>=10 are shown."
  ) 
  dev.off()
}

Distributioner$diagPlot <- function(.){

  dev.new(width=16/2.54,height=18/2.54)
  par(mfrow=c(length(names(.$dists)),3),mar=c(0.5,5,0,0),oma=c(5,0,1,3))
  for(dist in names(.$dists)){
    
    last = dist==names(.$dists)[length(.$dists)]

    fit = .$dists[[dist]]$fit
    if(typeof(fit)!='character'){
      if(0){
        #Fitted distributions
        xlim = c(0,quantile(.$var,p=0.99))
        h = hist(.$var,breaks=100,plot=F)
        xhist = c(min(h$breaks),h$breaks)
        yhist = c(0,h$density,0)
        xfit = seq(min(.$var),max(.$var),length=1000)
        yfit = switch(dist,
        	gaussian = dnorm(xfit,mean=fit$estimate[1],sd=fit$estimate[2]),
        	gamma = dgamma(xfit,shape=fit$estimate[1],rate=fit$estimate[2]),
        	lognormal = dlnorm(xfit,meanlog=fit$estimate[1],sdlog=fit$estimate[2]),
        	weibull = dweibull(xfit,shape=fit$estimate[1],scale=fit$estimate[2]),
        	log.logistic = dllogis(xfit,shape=fit$estimate[1],rate=fit$estimate[2]),
        	inverse.gaussian = dinvGauss(xfit,nu=fit$estimate[1],lambda=fit$estimate[2]),
        	rep(NA,length(xfit))
        )
        plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)),ylab='Density',las=1,xlim=xlim,xaxt=if(last)'s'else'n')
        lines(xfit,yfit,col='blue',lty=2)
        legend('topleft',paste('NLL:',round(-fit$loglik)),bty='n',cex=1.1,xjust=1)
        if(last) mtext('Catch (scaled)',side=1,line=3,cex=0.7)
      } else {
      	#QQplot (thoretical verus emprical quantiles)
      	qs = seq(0.01,0.99,0.01)
      	the = switch(dist,
      	  gaussian = qnorm(qs,mean=fit$estimate[1],sd=fit$estimate[2]),
      	  gamma = qgamma(qs,shape=fit$estimate[1],rate=fit$estimate[2]),
      	  lognormal = qlnorm(qs,meanlog=fit$estimate[1],sdlog=fit$estimate[2]),
      	  weibull = qweibull(qs,shape=fit$estimate[1],scale=fit$estimate[2]),
      	  log.logistic = qllogis(qs,shape=fit$estimate[1],rate=fit$estimate[2]),
      	  inverse.gaussian = qinvGauss(qs,nu=fit$estimate[1],lambda=fit$estimate[2]),
      	  rep(qs,length(xfit))
      	)
      	emp = quantile(.$var,qs)
      	plot(emp~the,cex=0.5,ylab='Observed',las=1,xaxt=if(last)'s'else'n',log='xy')
      	abline(0,1,col='blue',lty=2)
      	legend('topleft',paste('NLL:',round(-fit$loglik)),bty='n',cex=1.1,xjust=1)
        if(last) mtext('Expected',side=1,line=3,cex=0.7)
      }
    } else {
      plot.new()
    }

    model = .$dists[[dist]]$model
    logLike = .$dists[[dist]]$logLike
    aic = .$dists[[dist]]$aic
    if(typeof(model)!='character'){
      type = class(model)[1]
      rs = switch(type,
      	'glm' = rstandard(model),
      	'survreg' = residuals(model,type='deviance')
      )
      rs = rs-mean(rs,na.rm=T)
      h = hist(rs,breaks=100,plot=F)
      xhist = c(min(h$breaks),h$breaks)
      yhist = c(0,h$density,0)
      xfit = seq(min(rs),max(rs),length=1000)
      yfit =  dnorm(xfit)
      plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)*1.3),ylab='Denisty',las=1,xlim=c(-4,4),xaxt=if(last)'s'else'n')
      lines(xfit,yfit,col='blue',lty=2)
      legend('topleft',c(paste('NLL:',round(-logLike)),paste('AIC:',round(aic))),bty='n',cex=1.1,xjust=1)
      if(last) mtext('Stand. resid.',side=1,line=3,cex=0.7)

      qqnorm(rs,cex=0.5,main='',xlim=c(-4,4),ylab='Observed',xaxt=if(last)'s'else'n')
      abline(v=qnorm(c(0.001,0.01,0.1,0.9,0.99,0.999)),lty=3,col='grey')
      abline(0,1,col='blue',lty=2)
      if(last) mtext('Expected',side=1,line=3,cex=0.7)
    }
    else {
      plot.new()
      plot.new()
    }
    mtext(dist,side=4,srt=90,line=1.5)
  }
  Figure(
    "Distributioner.diagPlot",
    paste(
    "Diagnostics for alternative distributional assumptions for catch. 
     Left: quantile-quantile plot of observed catches (centred (by mean) and scaled (by standard deviation) in log space) versus maximum likelihood fit of distribution (missing panel indicates the fit failed to converge);
     Middle: standardised residuals from a generalised linear model fitted using the formula",deparse(.$formula),"and the distribution (missing panel indicates the model failed to converge);
     Right: quantile-quantile plot of model standardised residuals against standard normal (vertical lines represent 0.1%, 1% and 10% percentiles).
     NLL = negative log-likelihood; AIC = Akaike information criterion
    "
    )
  ) 
  dev.off()
}

Distributioner$best <- function(.){
  best = NULL
  max = -Inf
  for(dist in names(.$dists)){
    ll = .$dists[[dist]]$logLike
    if(is.numeric(ll) & ll>max){
      best = dist
      max = ll
    }
  }
  list(
      distribution = best,
      model = .$dists[[best]]$model,
      criterion = .$dists[[best]]$logLike
  )
}

Distributioner$initModel <- function(.,dist,data){
  #Create an initial(null) model that can be used in forward stepwise selection
  if(dist %in% c('gamma','inverse.gaussian')){
    init = glm(catch~1,data=data,family=switch(dist,
      gamma = Gamma(link='log'),
      inverse.gaussian = inverse.gaussian(link='log')
    ))
  }
  if(dist %in% c('lognormal','log.logistic','weibull')){
    init = survreg(Surv(catch)~1,data=data,dist=dist)
  }
  return(init)
}

Distributioner$convertModel <- function(.,model,dist,data){
  #Adjust response term in model formula as required
  formula = as.character(formula(model))
  response = formula[2]
  if(substr(response,1,5)=='Surv(') response = substr(response,6,nchar(response)-1)
  #Create model
  if(dist %in% c('gamma','inverse.gaussian','log.trans.normal')){
    if(dist=='log.trans.normal') response = paste('log(',response,')',sep='')
    formula = as.formula(paste(response,'~',formula[3]))
    new = glm(formula,data=data,family=switch(dist,
      gamma = Gamma(link='log'),
      inverse.gaussian = inverse.gaussian(link='log'),
      log.trans.normal = gaussian(link='identity')
     ))
  }
  if(dist %in% c('lognormal','log.logistic','weibull')){
    formula = as.formula(paste('Surv(',response,')~',formula[3]))
    new = survreg(formula,data=data,dist=dist)
  }
  return(new)
}
