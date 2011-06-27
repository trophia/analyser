library(moments)
library(robustbase)

Diagnoser <- Worker$proto(
  label = "Diagnoser",
  model = NULL
)

Diagnoser$new <- function(.,model){
  inst = .$proto()
  inst$data = cbind(
    model$data,
    residual = rstandard(model),
    predict(model,type='terms',se.fit=T),
    observed = log(model$data$catch),
    fitted = fitted(model)
  )
  inst$data$residual = inst$data$residual-mean(inst$data$residual)
  inst$data = within(inst$data,{
    latt = round(lat,1)-0.05
    lont = round(lon,1)+0.05
    season = factor(c('Su','Su','Au','Au','Au','Wi','Wi','Wi','Sp','Sp','Sp','Su')[month],levels=c('Sp','Su','Au','Wi'),ordered=T)
  })
  inst
}

Diagnoser$residPlot <- function(.,to){
  #Usual diagnostic plots
  dev.new(width=16/2.54,height=16/2.54)
  par(mfcol=c(2,2),mar=c(4,4,1,1),oma=rep(1,4))
  with(.$data,{
    xrange = quantile(residual,p=c(0.001,0.999))
    xrange[1] = min(xrange[1],-4)
    xrange[2] = max(xrange[2],4)
    bars = hist(residual,probability=T,breaks=50,main="",xlab="Standardised residual",xlim=xrange,bty='o')
    lines(bars$mids,dnorm(bars$mids),col='blue',lty=2)
    qqnorm(residual,main="",ylab='Standardised residual sample quantile',xlab='Standardised residual theoretical quantile',xlim=xrange,cex=0.3,pch=16)
    abline(a=0,b=1,col='blue',lty=2)
    plot(fitted,residual,ylab='Standardised residual',xlab='Fitted value',pch=16,col=rgb(0,0,0,0.2))
    abline(h=0,col='blue',lty=2)
    plot(fitted,observed,ylab='Observed value',xlab='Fitted value',pch=16,col=rgb(0,0,0,0.2))
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

Diagnoser$residMomentsPlot <- function(.,to){
  #Plots of std.dev, skewness and kurtosis of residuals by year
  data = ddply(.$data,.(fyear),function(sub)with(sub,data.frame(
    mean=mean(residual),
    stdev=sd(residual),
    skew=skewness(residual),
    mc=mc(residual),
    kurt=kurtosis(residual)
  )))
  dev.new(width=16/2.54,height=16/2.54)
  par(mfrow=c(4,1),mar=c(0,4,0,0),oma=c(5,1,1,1))
  data$fyear = as.integer(as.character(data$fyear))
  for(var in c('stdev','skew','mc','kurt')) {
    plot(data$fyear,data[,var],type='o',pch=16,cex=1.5,las=1,
      ylab=switch(var,mean='Mean',stdev='Standard deviation',skew='Skewness',mc='Medcouple',kurt='Kurtosis',var),
      ylim=switch(var,mean=range(data[,var]),stdev=range(data[,var]),skew=range(0,range(data[,var])),mc=range(0,range(data[,var])),kurt=range(3,range(data[,var])),range(data[,var])),
      xaxt=switch(var,kurt='s','n')
    )
    abline(h=switch(var,mean=0,stdev=1,skew=0,mc=0,kurt=3),col='blue',lty=2)
  }
  Figure(
    "Diagnoser.residMomentsPlot",
    "Moments of standardised residuals by fishing year. Blue dotted line indicates the expected value."
  ) 
  dev.off()
}

Diagnoser$implieds <- function(.,bys,fitteds){
  terms = vector()
  for(term in fitteds){
    if(paste('fit.',term,sep='') %in% names(.$data)) terms = c(terms,paste('fit.',term,sep=''))
  }
  if(length(terms)==0) {
    imp = ddply(.$data,bys,function(sub)with(sub,data.frame(fit=0,mean=mean(residual),se=sd(residual)/sqrt(length(residual)))))
  }  else if(length(terms)==1) {
    imp = ddply(.$data,bys,function(sub)with(sub,data.frame(fit=mean(sub[,terms]),mean=mean(rowSums(sub[,c(terms,'residual')])),se=sd(residual)/sqrt(length(residual)))))
  } else {
    imp = ddply(.$data,bys,function(sub)with(sub,data.frame(fit=mean(rowSums(sub[,terms])),mean=mean(rowSums(sub[,c(terms,'residual')])),se=sd(residual)/sqrt(length(residual)))))
  }
  imp
}

Diagnoser$areaYearImpliedPlot <- function(.){
  imp = .$implieds(bys=c('area','fyear'),fitteds=c('fyear'))
  imp$fyear = as.integer(as.character(imp$fyear))
  dev.new(width=16/2.54,height=16/2.54)
  print(ggplot(imp,aes(x=fyear,y=exp(mean)))+geom_point()+geom_line()+geom_errorbar(aes(ymin=exp(mean-se),ymax=exp(mean+se)),size=0.3,width=0.3)+
    geom_hline(yintercept=1,linetype=3,colour='grey')+geom_line(aes(y=exp(fit)),col='grey')+ylim(c(0,max(exp(imp$mean))))+
    facet_wrap(~area)+labs(x='Fishing year',y='Coefficient'))
  Figure(
    "Diagnoser.areaYearImpliedPlot",
    "Resdiual implied coefficients for each area in each fishing year. Implied coefficients are calculated as the sum of the fishing year coefficient plus the mean of the residuals
    in each fishing year in each area. The error bars indicate one standard error of residuals. The grey line indicates the model's overall fishing year coefficients."
  ) 
}

Diagnoser$areaMonthImpliedPlot <- function(.){
  imp = .$implieds(bys=c('area','month'),fitteds=c('month'))
  imp$monthi = as.integer(imp$month)
  dev.new(width=16/2.54,height=16/2.54)
  print(
    ggplot(imp,aes(x=monthi,y=exp(mean)))+geom_point()+geom_line()+geom_errorbar(aes(ymin=exp(mean-se),ymax=exp(mean+se)),size=0.3,width=0.3)+
      geom_hline(yintercept=1,linetype=3,colour='grey')+geom_line(aes(y=exp(fit)),col='grey60')+
      ylim(c(0,max(exp(imp$mean))))+scale_x_continuous(breaks=1:12,labels=levels(imp$month))+
      facet_wrap(~area)+labs(x='Month',y='Coefficient')+
      opts(axis.text.x=theme_text(angle=90))
  )
  Figure(
    "Diagnoser.areaMonthImpliedPlot",
    "Resdiual implied coefficients for area/month interactions. Implied coefficients are calculated as the mean of the sum of area and month coefficients (if any; grey line) plus residuals (black points and line). 
      The error bars indicate one standard error of residuals."
  ) 
}

Diagnoser$posMonthImpliedPlot <- function(.){
  imp = .$implieds(bys=c('latt','lont','month'),fitteds=c('area','month','areaMonth'))
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
    "Residual implied coefficients for each position in each month. Implied coefficients are calculated as the mean, in each position in each month, of the sum of the model fit and the residual for each stratum."
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
  quants = unique(quantile(.$data$depth,p=seq(0.01,0.99,0.05),na.rm=T)) #Unique is sometimes necessary because some of the quantiles can be the same. This causes problems with cut below.
  mids = quants[1:(length(quants)-1)]+diff(quants)/2
  data = within(.$data,{
    depthc = mids[cut(depth,quants,labels=F)]
  })
  sp = ddply(data,.(depthc,month),function(sub)with(sub,data.frame(mean=mean(residual),se=sd(residual)/sqrt(length(residual)))))
  print(ggplot(sp,aes(x=depthc,y=mean))+geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-se,ymax=mean+se),size=0.3,width=0.3)+labs(x='',y='')+facet_wrap(~month))
  Figure(
    "Diagnoser.depthResidPlot",
    "Mean and standard error of residuals by depth by month. Each point represents a 5th percentile of the depth distibution across all strata."
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
