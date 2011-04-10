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
  inst$data = within(inst$data,{
    latt = round(lat,1)-0.05
    lont = round(lon,1)+0.05
    season = factor(c('Su','Su','Au','Au','Au','Wi','Wi','Wi','Sp','Sp','Sp','Su')[month],levels=c('Sp','Su','Au','Wi'),ordered=T)
  })
  inst
}

Diagnoser$residPlot <- function(.,to){
  #Standardised residuals(good luck explaining the influence plot)
  dev.new(width=16/2.54,height=16/2.54)
  par(mfcol=c(2,2),mar=c(4,4,1,1),oma=rep(1,4))
  with(.$data,{
    xrange = quantile(residual,p=c(0.001,0.999))
    xrange[1] = min(xrange[1],-4)
    xrange[2] = max(xrange[2],4)
    bars = hist(residual,probability=T,breaks=50,main="",xlab="Standardised residual",xlim=xrange,bty='o')
    lines(bars$mids,dnorm(bars$mids),col='blue',lty=2)
    qqnorm(residual,main="",ylab='Standardised residual sample quantile',xlab='Standardised residual theoretical quantile',xlim=xrange)
    abline(a=0,b=1,col='blue',lty=2)
    plot(fitted,residual,ylab='Standardised residual',xlab='Fitted value')
    abline(h=0,col='blue',lty=2)
    plot(fitted,observed,ylab='Observed value',xlab='Fitted value')
    abline(a=0,b=1,col='blue',lty=2)
  })
  Figure(
    "Diagnoser.resids",
    "Residual diagnostics.
	  Top left: histogram of standardised residuals compared to standard normal distribution.',
	  Bottom left: quantile-quantile plot of standardised residuals.
	  Top right: fitted values versus standardised residuals.
	  Bottom right: observed values versus fitted values."
  ) 
  dev.off()
}

Diagnoser$areaYearImpliedPlot <- function(.){
  oa = ddply(.$data,.(fyear),function(sub)with(sub,data.frame(est=mean(fit.fyear))))
  oa$fyear = as.integer(as.character(oa$fyear))
  sp = ddply(.$data,.(area,fyear),function(sub)with(sub,data.frame(mean=mean(fit.fyear+residual),se=sd(residual)/sqrt(length(residual)))))
  sp$fyear = as.integer(as.character(sp$fyear))
  dev.new(width=16/2.54,height=16/2.54)
  print(ggplot(sp,aes(x=fyear,y=exp(mean)))+geom_point()+geom_line()+geom_errorbar(aes(ymin=exp(mean-se),ymax=exp(mean+se)),size=0.3,width=0.3)+
    geom_hline(yintercept=1,linetype=3,colour='grey')+geom_line(aes(y=exp(est)),data=oa,col='grey')+ylim(c(0,max(exp(sp$mean))))+
    facet_wrap(~area)+labs(x='Fishing year',y='Multiplier'))
  Figure(
    "Diagnoser.areaYearImpliedPlot",
    "Implied indices of CPUE for each area in each fishing year. Implied indices are calculated as the model's fishing year coefficients plus the mean of the residuals
    in each fishing year in each area. The error bars indicate one standard error of residuals. The grey line indicates the model's overall fishing year index."
  ) 
}

Diagnoser$areaMonthImpliedPlot <- function(.){
  #Determine which terms consider
  terms = vector()
  if('fit.area' %in% names(.$data)) terms = c(terms,'fit.area')
  if('fit.month' %in% names(.$data)) terms = c(terms,'fit.month')
  if('fit.areaMonth' %in% names(.$data)) terms = c(terms,'fit.areaMonth')
  #Determine model fit and implied
  if(length(terms)==0) {
    imp = ddply(.$data,.(area,month),function(sub)with(sub,data.frame(fit=0,mean=mean(residual),se=sd(residual)/sqrt(length(residual)))))
  }  else if(length(terms)==1) {
    imp = ddply(.$data,.(area,month),function(sub)with(sub,data.frame(fit=mean(sub[,terms]),mean=mean(rowSums(sub[,c(terms,'residual')])),se=sd(residual)/sqrt(length(residual)))))
  } else {
    imp = ddply(.$data,.(area,month),function(sub)with(sub,data.frame(fit=mean(rowSums(sub[,terms])),mean=mean(rowSums(sub[,c(terms,'residual')])),se=sd(residual)/sqrt(length(residual)))))
  }
  imp$month = as.integer(imp$month)
  #Plot it
  dev.new(width=16/2.54,height=16/2.54)
  print(
    ggplot(imp,aes(x=month,y=exp(mean)))+geom_point()+geom_line()+geom_errorbar(aes(ymin=exp(mean-se),ymax=exp(mean+se)),size=0.3,width=0.3)+
      geom_hline(yintercept=1,linetype=3,colour='grey')+geom_line(aes(y=exp(fit)),col='grey60')+scale_x_continuous(breaks=seq(1,12,2),labels=levels(.$data$month)[seq(1,12,2)])+
      facet_wrap(~area)+labs(x='Month',y='Coefficient')
  )
  Figure(
    "Diagnoser.areaMonthImpliedPlot",
    "Implied coefficients for each area in each month. Implied coefficients are calculated as the model's fitted value for each
      month and area plus residuals (black line and points). The error bars indicate one standard error of residuals. The grey line indicates the
      models overall fit for month."
  ) 
}

Diagnoser$posMonthImpliedPlot <- function(.){
  #Determine which terms to adjust for
  terms = vector()
  if('fit.area' %in% names(.$data)) terms = c(terms,'fit.area')
  if('fit.month' %in% names(.$data)) terms = c(terms,'fit.month')
  if('fit.areaMonth' %in% names(.$data)) terms = c(terms,'fit.areaMonth')
  if(length(terms)==0) {
    imp = ddply(.$data,.(latt,lont,month),function(sub)with(sub,data.frame(fit=0,mean=mean(residual),se=sd(residual)/sqrt(length(residual)))))
  }  else if(length(terms)==1) {
    imp = ddply(.$data,.(latt,lont,month),function(sub)with(sub,data.frame(fit=mean(sub[,terms]),mean=mean(rowSums(sub[,c(terms,'residual')])),se=sd(residual)/sqrt(length(residual)))))
  } else {
    imp = ddply(.$data,.(latt,lont,month),function(sub)with(sub,data.frame(fit=mean(rowSums(sub[,terms])),mean=mean(rowSums(sub[,c(terms,'residual')])),se=sd(residual)/sqrt(length(residual)))))
  }
  #Determine suitable lt and lon ranges
  imp = subset(imp,latt<(-30) & latt>(-50) & lont>160 & lont<200)
  latr = quantile(imp$latt,p=c(0.01,0.99),na.rm=T)
  latr = c(latr[1]-0.5,latr[2]+0.5)
  lonr = quantile(imp$lont,p=c(0.01,0.99),na.rm=T)
  lonr = c(lonr[1]-0.5,lonr[2]+0.5)
  #Plot it
  dev.new(width=16/2.54,height=16/2.54)
  print (
    ggplot(imp,aes(x=lont,y=latt)) + geom_polygon(data=clipPolys(coast,ylim=latr,xlim=lonr),aes(x=X,y=Y,group=PID),fill='white',colour="grey80") + 
      geom_tile(aes(fill=mean))+scale_fill_gradient2('Coefficient',low="blue",mid='grey',high="red")+facet_wrap(~month)+labs(x='',y='') + 
      scale_y_continuous("",limits=latr,expand=c(0,0)) + 
      scale_x_continuous("",limits=lonr,expand=c(0,0)) +
      coord_map(project="mercator")
  )
  Figure(
    "Diagnoser.posMonthImpliedPlot",
    "Implied coefficients for each position in each month. Implied coefficients are calculated as the model's area or month coefficients (if any) plus the mean of the residuals
    in each position in each month."
  ) 
}
Diagnoser$posMonthCatchPlot <- function(.){
  sp = ddply(subset(.$data,as.integer(as.character(fyear))>=2008),.(latt,lont,month),function(sub)with(sub,data.frame(catch=sum(catch))))
  print(ggplot(sp,aes(x=lont,y=latt))+geom_tile(aes(fill=log(catch)))+scale_fill_gradientn(colours=rev(rainbow(10,end=0.7)))+facet_wrap(~month)+labs(x='',y='')+ylim(-42,-36))
}
Diagnoser$posSeasonResidPlot <- function(.){
  sp = ddply(.$data,.(latt,lont,season),function(sub)with(sub,data.frame(mean=mean(residual))))
  print(ggplot(sp,aes(x=lont,y=latt))+geom_tile(aes(fill=mean))+scale_fill_gradient2(low="blue",mid='grey',high="red")+facet_wrap(~season)+labs(x='',y='')+ylim(-42,-36))
}

Diagnoser$depthResidPlot <- function(.){
  quants = quantile(.$data$depth,p=seq(0.01,0.99,0.05),na.rm=T)
  mids = quants[1:(length(quants)-1)]+diff(quants)/2
  data = within(.$data,{
    depthc = mids[cut(depth,quants,labels=F)]
  })
  sp = ddply(data,.(depthc,month),function(sub)with(sub,data.frame(mean=mean(residual),se=sd(residual)/sqrt(length(residual)))))
  print(ggplot(sp,aes(x=depthc,y=mean))+geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-se,ymax=mean+se),size=0.3,width=0.3)+labs(x='',y='')+facet_wrap(~month))
  Figure(
    "Diagnoser.depthResidPlot",
    "Mean and standard error of residuals by depth. Each point represents 5 percentile of the depth distibution across all strata."
  ) 
}

Diagnoser$lognormal <- function(.,to){
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
