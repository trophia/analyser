
library(MASS) #For fitdistr
library(SuppDists) #For inverse Gaussian distibution : dinvGauss and qinvGauss
library(actuar) #For Log-logistic distributuion : dllogis and qllogis
library(survival) #For survreg
library(VGAM) #For vglm

Distributioner <- Worker$proto(
  label = "Distributioner",
  data = NULL
)

Distributioner$new <- function(.,data){
  inst = .$proto()
  inst$data = data
  inst
}

Distributioner$calc <- function(.){
  data = subset(.$data,catch>0 & is.finite(catch))
  .$var = data$catch/mean(data$catch)

  modelFunc = 'd'
  dists = list()
  for(dist in c('lognormal','log.logistic','gamma','weibull','inverse.gaussian')){
    dists[[dist]]$fit = tryCatch(fitdistr(var,
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
    ),error=function(error){print(error);return(NULL)})
    dists[[dist]]$model = tryCatch(switch(modelFunc,
	a = glm(catch~fyear+month+area+vessel,data=data,family=switch(dist,
	  gaussian = gaussian(link='log'),
	  gamma = Gamma(link='log'),
	  inverse.gaussian = inverse.gaussian(link='log'),
	)),
	b = survreg(Surv(catch)~fyear+month+area+vessel,data=data,dist=switch(dist,
	  log.logistic = 'loglogistic',
	  dist
	)),
	c = vglm(catch~fyear+month+area+vessel,data=data,family=switch(dist,
	  gaussian = gaussianff,
	  gamma = gamma2,
	  inverse.gaussian = inv.gaussianff,
	  lognormal = lognormal,
	  weibull = weibull
	)),
	d = switch(dist,
	  gaussian = glm(catch~fyear+month+area+vessel,data=data,family=gaussian(link='log')),
	  gamma = glm(catch~fyear+month+area+vessel,data=data,family=Gamma(link='log')),
	  inverse.gaussian = glm(catch~fyear+month+area+vessel,data=data,family=inverse.gaussian(link='log')),
	  lognormal = survreg(Surv(catch)~fyear+month+area+vessel,data=data,dist='lognormal'),
	  weibull = survreg(Surv(catch)~fyear+month+area+vessel,data=data,dist='weibull'),
	  log.logistic = survreg(Surv(catch)~fyear+month+area+vessel,data=data,dist='loglogistic')
	)
      ),error=function(error){print(error);return(NULL)})
      dists[[dist]]$criterion = tryCatch(switch(class(dists[[dist]]$model)[1],
	'glm' = logLik(model),
	'survreg' = summary(model)$loglik[1],
	'vglm' = logLik(model)
      ),error=function(error){print(error);return(NULL)})
    }
    .$dists = dists
}

Distributioner$meanVariancePlot <- function(.){
  do = function(vars,row,col){
    data = ddply(.$data,vars,function(sub)with(sub,data.frame(mean=mean(catch,na.rm=T),var=var(catch,na.rm=T),n=nrow(sub))))
    data = subset(data,n>3)
    coeffs = summary(lm(log(var)~log(mean),data))$coef
    p = ggplot(data,aes(x=log(mean),y=log(var)))+geom_point(aes(size=n),shape=1,alpha=0.3)+scale_area()+
      geom_abline(intercept=coeffs[1,1],slope=coeffs[2,1])+
      geom_text(aes(x=x,y=y,label=label),data=data.frame(x=min(log(data$mean)),y=max(log(data$var)),label=paste(round(coeffs[2,1],2),' (',round(coeffs[2,2],2),')',sep='')),size=3.5,hjust=0)+
      opts(axis.title.x=theme_text(size=10),axis.title.y=theme_text(size=10,angle=90),title=paste(vars,collapse='*'),plot.title=theme_text(size=10))+
      labs(size='')
    print(p,vp=subplot(row,col))
  }
  dev.new(width=16/2.54,height=16/2.54)
  vplayout(3,1)
  do(c('fyear','month'),1,1)
  do(c('fyear','month','area'),2,1)
  do(c('fyear','month','area','vessel'),3,1)
  Figure(
    "Distributioner.meanVariancePlot",
    "Mean-variance plots at for alternative strata definitions. Area of circles is proporiotal to the number of records in each stratum. Lines indicate the unweighted fit to the points and
    values in upper-left are the estimated slope and standard error."
  ) 
  dev.off()
}

Distributioner$diagPlot <- function(.){

  dev.new(width=16/2.54,height=16/2.54)
  par(mfrow=c(length(names(.$dists)),3),mar=c(0.5,5,0,0),oma=c(5,0,1,3))
  for(dist in names(.$dists)){
    
    last = dist==names(.$dists)[length(.$dists)]

    fit = .$dists[[dist]]$fit
    if(!is.null(fit)){
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
	rep(NA,length(xfit))
      )
      plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)),ylab='Density',las=1,xlim=xlim,xaxt=if(last)'s'else'n')
      lines(xfit,yfit,col='blue',lty=2)
      legend('topright',paste('LL:',round(fit$loglik)),bty='n',cex=1.2,xjust=1)
      if(last) mtext('Catch (scaled)',side=1,line=3,cex=0.7)

      if(0){
	#QQplot (thoretical verus emprical quantiles)
	qs = seq(0.01,0.99,0.01)
	the = switch(dist,
	  gaussian = qnorm(qs,mean=fit$estimate[1],sd=fit$estimate[2]),
	  gamma = qgamma(qs,shape=fit$estimate[1],rate=fit$estimate[2]),
	  lognormal = qlnorm(qs,meanlog=fit$estimate[1],sdlog=fit$estimate[2]),
	  weibull = qweibull(qs,shape=fit$estimate[1],scale=fit$estimate[2]),
	  log.logistic = qllogis(qs,shape=fit$estimate[1],rate=fit$estimate[2]),
	  rep(qs,length(xfit))
	)
	emp = quantile(.$var,qs)
	plot(emp~the,cex=0.5,ylab='Observed',las=1,xaxt=if(last)'s'else'n')
	abline(0,1,col='blue',lty=2)
	if(last) mtext('Expected',side=1,line=3,cex=0.7)
      }
    } else {
      plot.new()
      if(0) plot.new()
    }

    model = .$dists[[dist]]$model
    if(!is.null(model)){
      type = class(model)[1]
      rs = switch(type,
	'glm' = rstandard(model),
	'survreg' = residuals(model,type='deviance'),
	'vglm' = residuals(model,type='deviance')[,1]
      )
      logLike = switch(type,
	'glm' = logLik(model),
	'survreg' = summary(model)$loglik[1],
	'vglm' = logLik(model)
      )
      #aic = extractAIC(model)[2]

      h = hist(rs,breaks=100,plot=F)
      xhist = c(min(h$breaks),h$breaks)
      yhist = c(0,h$density,0)
      xfit = seq(min(rs),max(rs),length=1000)
      yfit =  dnorm(xfit)
      plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)*1.2),ylab='Denisty',las=1,xlim=c(-4,4),xaxt=if(last)'s'else'n')
      lines(xfit,yfit,col='blue',lty=2)
      legend('topright',c(paste('LL:',round(logLike))),bty='n',cex=1.2,xjust=1) #,paste('AIC:',round(aic))
      if(last) mtext('Stand. resid.',side=1,line=3,cex=0.7)

      qqnorm(rs,cex=0.5,main='',xlim=c(-4,4),ylab='Observed',xaxt=if(last)'s'else'n')
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
    "Diagnostics for alternative distributional assumptions for catch. 
     Left: maximum likelihood fit (dotted) to observed catches (solid, scaled by their mean);
     Middle: standardised residuals from a model catch~fyear+month+area+vessel;
     Right: quantile-quantile plot of standardised residuals of model.
     LL = log-likelihood of fit
    "
  ) 
  dev.off()
}

Distributioner$best <- function(.){
  best = NULL
  max = -Inf
  for(dist in names(.$dists)){
    if(.$dists[[dist]]$criterion>max){
      best = dist
      max = .$dists[[dist]]$criterion
    }
  }
  best
}

