library(proto)

Diagnoser <- Worker$proto(
  label = "Diagnoser",
  model = NULL
)

Diagnoser$new <- function(.,model){
  inst = .$proto(model)
  inst$data = cbind(
    model$data,
    residual = rstandard(model),
    predict(model,type='terms',se.fit=T),
    observed = log(model$data$catch),
    fitted = fitted(model)
  )
  inst
}

Diagnoser$areaYearImpliedPlot <- function(.){
  oa = ddply(data,.(fyear),function(sub)with(sub,data.frame(est=mean(fit.fyear))))
  oa$fyear = as.integer(as.character(oa$fyear))
  sp = ddply(data,.(area,fyear),function(sub)with(sub,data.frame(mean=mean(fit.fyear+resid),se=sd(resid)/sqrt(length(resid)))))
  sp$fyear = as.integer(as.character(sp$fyear))
  print(ggplot(sp,aes(x=fyear,y=exp(mean)))+geom_point()+geom_line()+geom_errorbar(aes(ymin=exp(mean-se),ymax=exp(mean+se)),size=0.3,width=0.3)+
    geom_hline(yintercept=1,linetype=3,colour='grey')+geom_line(aes(y=exp(est)),data=oa,col='grey')+
    facet_wrap(~area)+labs(x='Fishing year',y='Multiplier'))
}

Diagnoser$areaMonthResidPlot <- function(.){
  sp = ddply(data,.(area,month),function(sub)with(sub,data.frame(mean=mean(resid),se=sd(resid)/sqrt(length(resid)))))
  print(ggplot(sp,aes(x=month,y=exp(mean)))+geom_point()+geom_line()+geom_errorbar(aes(ymin=exp(mean-se),ymax=exp(mean+se)),size=0.3,width=0.3)+
    geom_hline(yintercept=1,linetype=3,colour='grey')+
    facet_wrap(~area)+labs(x='Month',y='Multiplier'))
}

Diagnoser$posMonthResidPlot <- function(.){
  data = within(data,{
    latt = round(lat,1)
    lont = round(lon,1)
  })
  sp = ddply(data,.(latt,lont,month),function(sub)with(sub,data.frame(mean=mean(resid))))
  #spg = expand.grid(month=1:12,lat=seq(-42,-36,0.1),lon=seq(174,179,0.1))
  #sp=merge(spg,sp)
  #ddply(data,.(lat,lon,month),nrow)
  print(ggplot(sp,aes(x=lont,y=latt))+geom_tile(aes(fill=mean))+scale_fill_gradient2(low="blue",mid='grey',high="red")+facet_wrap(~month)+labs(x='',y='')+ylim(-42,-36))
}

Diagnoser$posSeasonResidPlot <- function(.){
  data = within(data,{
    latt = round(lat,1)
    lont = round(lon,1)
    season = factor(c('Su','Su','Au','Au','Au','Wi','Wi','Wi','Sp','Sp','Sp','Su')[month],levels=c('Sp','Su','Au','Wi'),ordered=T)
  })
  sp = ddply(data,.(latt,lont,season),function(sub)with(sub,data.frame(mean=mean(resid))))
  print(ggplot(sp,aes(x=lont,y=latt))+geom_tile(aes(fill=mean))+scale_fill_gradient2(low="blue",mid='grey',high="red")+facet_wrap(~season)+labs(x='',y='')+ylim(-42,-36))
}

Diagnoser$lognormal <- function(.,to){

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
  ",to=to) 

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
	.$figure(paste("Diagnoser.Resids",field),'Residuals versus variable')
      }
  }
}

Diagnoser$report <- function(.,to=""){
  .$header(c(),to=to)
  if(!is.null(.$model)){
    if(.$model$family$family=='gaussian') .$lognormal(to=to)
    else if(.$model$family$family=='binomial') .$binomial(to=to)
  }
}
