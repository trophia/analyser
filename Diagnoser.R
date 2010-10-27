library(proto)

Diagnoser <- Worker$proto(
  model = NULL
)

Diagnoser$lognormal <- function(.){
  obs = log(.$model$data$catch)#!todo assumes log(catch)variable
  fits = fitted(.$model)
  resids = rstandard(.$model)

  #Standardised residuals
  dev.new(width=11,height=8)
  par(mfcol=c(2,2),mar=c(4,4,1,1),oma=rep(1,4))
  bars = hist(resids,probability=T,breaks=50,main="",xlab="Standardised residual")
  lines(bars$mids,dnorm(bars$mids),col='blue',lty=2)
  legend("topleft",paste("SDSR:",round(sd(resids,na.rm=T),3)),bty='n')
  legend("topleft",paste("MASR:",round(median(abs(resids),na.rm=T),3)),bty='n',adj=c(0,2))
  qqnorm(resids,main="")
  abline(a=0,b=1,col='blue',lty=2)
  plot(fits,resids,ylab='Standardised residual',xlab='Fitted value')
  abline(h=0,col='blue',lty=2)
  plot(fits,obs,ylab='Observed value',xlab='Fitted value')
  abline(a=0,b=1,col='blue',lty=2)
  .$figure("Diagnoser.Resids","Diagnostic plots for model.
	  Top left: histogram of standardised residuals compared to standard normal distribution.(SDSR: standard deviation of standardised residuals. MASR: median of absolute standardised residuals.)',
	  Bottom left: quantile-quantile plot of standardised residuals.
	  Top right: fitted values versus standardised residuals.
	  Bottom right: observed values versus fitted values.
  ") 

  #!todo Other diagnostics not implemented
  if(FALSE){
      #Outliers
      outliers = abs(resids)>3
      outliers = cbind(.$model$data[outliers,],resids[outliers])
      outliers = outliers[order(outliers[,"resids[outliers]"]),]

       #Plot standardised residuls versus each variable
      dev.new(width=7,height=10)
      par(mar=c(4,4,1,1),oma=c(2,2,0,0))
      for(field in names(dataModelFinal)){
	#print(field)
	if(sum(!is.na(dataModelFinal[,field]))>0) {
	  y = dataModelFinal[,field]
	  if(is.factor(y) | length(unique(y))<=20) {
	    par(mfrow=c(1,1))
	    y = factor(y)
	  }
	  else{
	    par(mfrow=c(2,1))
	    plot(y,resids,ylab='Standardised residual',xlab="")
	    #lines(smooth.spline(y,resids),col='red',lwd=1.5)
	    abline(h=0,col='blue',lty=2)
	    breaks = unique(quantile(y,p=seq(0,1,0.05),na.rm=T))
	    n = length(breaks)
	    breaks = c(breaks[1]-(breaks[2]-breaks[1]),breaks,breaks[n]+(breaks[n]-breaks[n-1]))
	    y = cut(y,breaks)
	  }
	  plot(y,resids,ylab='Standardised residual',xlab=paste(field,"(",round(sum(!is.na(y))/length(y)*100),"%)"),ylim=c(-3,3))
	  points(aggregate(resids,list(y),mean,na.rm=T),pch=16,col='red')
	  abline(h=0,col='blue',lty=2)
	  mtext(paste(field,"(",round(sum(!is.na(y))/length(y)*100),"%)"),outer=T,side=1,line=5)
	}
	.figure(paste("Diagnoser.Resids",field),'Residuals versus variable')
      }
  }
}

Diagnoser$report <- function(.,to=""){
  if(!is.null(.$model)){
    if(.$model$family$family=='gaussian') .$lognormal()
    else if(.$model$family$family=='binomial') .$binomial()
  }
}
