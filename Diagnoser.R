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
  #Standardised residuals
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
    geom_hline(yintercept=1,linetype=3,colour='grey')+geom_line(aes(y=exp(est)),data=oa,col='grey')+
    facet_wrap(~area)+labs(x='Fishing year',y='Multiplier'))
  Figure(
    "Diagnoser.areaYearImpliedPlot",
    "Implied indices of CPUE for each area in each fishing year. Implied indices are calculated as the model's fishing year coefficients plus the mean of the residuals
    in each fishing year in each area. The error bars indicate one standard error of residuals. The grey line indicates the model's overall fishing year index."
  ) 
}

Diagnoser$areaMonthResidPlot <- function(.){
  sp = ddply(.$data,.(area,month),function(sub)with(sub,data.frame(mean=mean(residual),se=sd(residual)/sqrt(length(residual)))))
  dev.new(width=16/2.54,height=16/2.54)
  print(ggplot(sp,aes(x=month,y=exp(mean)))+geom_point()+geom_line()+geom_errorbar(aes(ymin=exp(mean-se),ymax=exp(mean+se)),size=0.3,width=0.3)+
    geom_hline(yintercept=1,linetype=3,colour='grey')+
    facet_wrap(~area)+labs(x='Month',y='Multiplier'))
  Figure(
    "Diagnoser.areaMonthResidPlot",
    "Mean of the residuals in each month in each area. The error bars indicate one standard error of residuals."
  ) 
}
Diagnoser$areaMonthImpliedPlot <- function(.){
  if('fit.month' %in% names(.$data)) oa = ddply(.$data,.(month),function(sub)with(sub,data.frame(est=mean(rowSums(sub[,terms])))))
  else oa = ddply(.$data,.(month),function(sub)with(sub,data.frame(est=0)))
  oa$month =  as.integer(as.character(oa$month))

  if('fit.month' %in% names(.$data)) sp = ddply(.$data,.(area,month),function(sub)with(sub,data.frame(mean=mean(fit.month+residual),se=sd(residual)/sqrt(length(residual)))))
  else sp = ddply(.$data,.(area,month),function(sub)with(sub,data.frame(mean=mean(residual),se=sd(residual)/sqrt(length(residual)))))
  sp$month =  as.integer(as.character(sp$month))

  dev.new(width=16/2.54,height=16/2.54)
  print(ggplot(sp,aes(x=month,y=exp(mean)))+geom_point()+geom_line()+geom_errorbar(aes(ymin=exp(mean-se),ymax=exp(mean+se)),size=0.3,width=0.3)+
    geom_hline(yintercept=1,linetype=3,colour='grey')+geom_line(aes(y=exp(est)),data=oa,col='grey')+
    facet_wrap(~area)+labs(x='Month',y='Multiplier'))
  Figure(
    "Diagnoser.areaMonthImpliedPlot",
    "Implied indices of CPUE for each area in each month. Implied indices are calculated as the model's month coefficients plus the mean of the residuals
    in each month in each area. The error bars indicate one standard error of residuals. The grey line indicates the model's overall month coefficients."
  ) 
}

Diagnoser$posMonthResidPlot <- function(.){
  data = ddply(.$data,.(latt,lont,month),function(sub)with(sub,data.frame(mean=mean(residual))))

  data = subset(data,latt<(-30) & latt>(-50) & lont>160 & lont<200)
  latr = quantile(data$latt,p=c(0.01,0.99),na.rm=T)
  lonr = quantile(data$lont,p=c(0.01,0.99),na.rm=T)

  dev.new(width=16/2.54,height=16/2.54)
  print(
    ggplot(data,aes(x=lont,y=latt)) + geom_polygon(data=clipPolys(coast,ylim=latr,xlim=lonr),aes(x=X,y=Y,group=PID),fill='white',colour="grey80") + 
      geom_tile(aes(fill=mean))+scale_fill_gradient2(low="blue",mid='grey',high="red")+facet_wrap(~month)+labs(x='',y='') + 
      scale_y_continuous("",limits=latr,expand=c(0,0)) + 
      scale_x_continuous("",limits=lonr,expand=c(0,0)) +
      coord_map(project="mercator")
  )
  Figure(
    "Diagnoser.posMonthResidPlot",
    "Mean of the residuals in each month at each position. The error bars indicate one standard error of residuals."
  ) 
}
Diagnoser$posMonthImpliedPlot <- function(.){
  #Determine which terms to adjust for
  terms = c('residual')
  if('fit.area' %in% names(.$data)) terms = c(terms,'fit.area')
  if('fit.zone' %in% names(.$data)) terms = c(terms,'fit.zone')
  if('fit.month' %in% names(.$data)) terms = c(terms,'fit.month')
  
  data = ddply(.$data,.(latt,lont,month),function(sub) data.frame(mean=mean(rowSums(sub[,terms]))))
  data = subset(data,latt<(-30) & latt>(-50) & lont>160 & lont<200)
  latr = quantile(data$latt,p=c(0.01,0.99),na.rm=T)
  latr = c(latr[1]-0.5,latr[2]+0.5)
  lonr = quantile(data$lont,p=c(0.01,0.99),na.rm=T)
  lonr = c(lonr[1]-0.5,lonr[2]+0.5)
  
  dev.new(width=16/2.54,height=16/2.54)
  print (
    ggplot(data,aes(x=lont,y=latt)) + geom_polygon(data=clipPolys(coast,ylim=latr,xlim=lonr),aes(x=X,y=Y,group=PID),fill='white',colour="grey80") + 
      geom_tile(aes(fill=mean))+scale_fill_gradient2(low="blue",mid='grey',high="red")+facet_wrap(~month)+labs(x='',y='') + 
      scale_y_continuous("",limits=latr,expand=c(0,0)) + 
      scale_x_continuous("",limits=lonr,expand=c(0,0)) +
      coord_map(project="mercator")
  )
  Figure(
    "Diagnoser.posMonthImpliedPlot",
    "Implied indices of CPUE for each position in each month. Implied indices are calculated as the model's area or month coefficients (if any) plus the mean of the residuals
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
