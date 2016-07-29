Diagnoser <- function(model, data=NULL) {

  # Get data
  if(is.null(data)) data <- model$data
  if(is.null(data)) data <- model.frame(model)

  # Add observed, fitted, standardised residual
  type <- class(model)[1]
  if(type=='glm' | type=='negbin'){
    data <- cbind(
      data,
      observed = data$catch,
      fitted = fitted(model),
      residual = rstandard(model)
    )
  }
  else if(type=='survreg'){
    data <- cbind(
      data,
      observed = data$catch,
      fitted = predict(model,type='response'),
      residual = residuals(model,type='deviance')
    )
  }

  # Calculate term effects
  preds <- as.data.frame(predict(model,type='terms',se.fit=T)$fit)
  names(preds) <- paste('fit.',names(preds),sep='')
  data <- cbind(data,preds)

  # Remove wonky residuals
  data <- subset(data,is.finite(residual))

  # Normalise residuals and add terms
  data <- within(data,{
    residual <- residual-mean(residual,na.rm=T)
    # Season
    season <- factor(c('Su','Su','Au','Au','Au','Wi','Wi','Wi','Sp','Sp','Sp','Su')[month],levels=c('Sp','Su','Au','Wi'),ordered=T)
  })

  diagnostics_plot <- function(.,to){
    #Usual diagnostic plots
    dev.new(width=16/2.54,height=16/2.54)
    par(mfcol=c(2,2),mar=c(4,4,1,1),oma=rep(1,4))
    with(data,{
      xrange = quantile(residual,p=c(0.001,0.999),na.rm=T)
      xrange[1] = min(xrange[1],-4)
      xrange[2] = max(xrange[2],4)
      bars = hist(residual,probability=T,breaks=100,main="",xlab="Standardised residual",xlim=xrange,bty='o')
      lines(bars$mids,dnorm(bars$mids),col='blue',lty=2)
      qqnorm(residual,main="",ylab='Standardised residual sample quantile',xlab='Standardised residual theoretical quantile',xlim=xrange,cex=0.3,pch=16)
      abline(a=0,b=1,col='blue',lty=2)
      plot(fitted,residual,log='x',ylab='Standardised residual',xlab='Fitted value',pch=16,col=rgb(0,0,0,0.2))
      abline(h=0,col='blue',lty=2)
      plot(fitted,observed,log='xy',ylab='Observed value',xlab='Fitted value',pch=16,col=rgb(0,0,0,0.2))
      abline(a=0,b=1,col='blue',lty=2)
    })
  }

  residMomentsPlot <- function(.,to){
    # Plots of std.dev, skewness and kurtosis of residuals by year
    data = ddply(.$data,.(fyear),function(sub)with(sub,data.frame(
      mean=mean(residual),
      stdev=sd(residual),
      skew=skewness(residual),
      mc=mc(residual),
      kurt=kurtosis(residual)
    )))
    # data <- .$data %>%
    #   group_by(fyear) %>%
    #     summarise(
    #       mean = mean(residual),
    #       stdev = sd(residual),
    #       skew = skewness(residual),
    #       mc = mc(residual),
    #       kurt = kurtosis(residual)
    #       )
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
    # for(var in c('stdev','skew','mc','kurt')) {
    #   ggplot(data) +
    #   geom_point(aes(x=fyear, y=var)) +
    #   geom_line(aes(x=fyear, y=stdev)) +
    #   labs(x='Fishing Year', y=switch(var,mean='Mean',stdev='Standard deviation',skew='Skewness',mc='Medcouple',kurt='Kurtosis')) +
    #   geom_hline(yintercept = switch(var,mean=0,stdev=1,skew=0,mc=0,kurt=3),col='blue',lty=2)
    # }
    # Figure(
    #   "Diagnoser.residMomentsPlot",
    #   "Moments of standardised residuals by fishing year. Blue dotted line indicates the expected value."
    # )
  }

  implieds <- function(bys,fitteds){
    terms = vector()
    for(term in fitteds){
      if(paste('fit.',term,sep='') %in% names(data)) terms = c(terms,paste('fit.',term,sep=''))
    }
    if(length(terms)==0) {
      imp = ddply(data,bys,function(sub)with(sub,data.frame(fit=0,n=length(residual),mean=mean(residual,na.rm=T),se=sd(residual,na.rm=T)/sqrt(length(residual)))))
    }  else if(length(terms)==1) {
      imp = ddply(data,bys,function(sub)with(sub,data.frame(fit=mean(sub[,terms],na.rm=T),n=length(residual),mean=mean(rowSums(sub[,c(terms,'residual')]),na.rm=T),se=sd(residual,na.rm=T)/sqrt(length(residual)))))
    } else {
      imp = ddply(data,bys,function(sub)with(sub,data.frame(fit=mean(rowSums(sub[,terms]),na.rm=T),n=length(residual),mean=mean(rowSums(sub[,c(terms,'residual')]),na.rm=T),se=sd(residual,na.rm=T)/sqrt(length(residual)))))
    }
    imp
  }

  impliedPlot <- function(.,factor){
    imp = .$implieds(bys=c(factor,'fyear'),fitteds=c(factor,'fyear'))
    imp$fyear = as.integer(as.character(imp$fyear))
    dev.new(width=25/2.54,height=17/2.54)
    print(ggplot(imp,aes(x=fyear,y=exp(mean)))+geom_point()+geom_line()+geom_errorbar(aes(ymin=exp(mean-se),ymax=exp(mean+se)),size=0.3,width=0.3)+
            geom_hline(yintercept=1,linetype=3,colour='grey')+geom_line(aes(y=exp(fit)),col='grey')+
            ylim(c(0,max(exp(imp$mean))*1.1))+scale_y_log10()+
            facet_wrap(as.formula(paste('~',factor)),scales='free_y')+labs(x='Fishing year',y='Coefficient'))
    # Figure(
    #   "Diagnoser.impliedPlot",
    #   paste("Residual implied coefficients for ",factor," x fishing year interactions.
    #     Implied coefficients (points) are calculated as the normalised fishing year coefficient (grey line)
    #       plus the mean of the standardised residuals in each fishing year and ",factor,".
    #     These values approximate the coefficients obtained when an ",factor," x year interaction term is fitted,
    #       particularly for those ",factor," x year combinations which have a substantial proportion of the records.
    #     The error bars indicate one standard error of the standardised residuals.",sep='')
    # )
  }

  areaYearImpliedPlot <- function(.){
    imp = .$implieds(bys=c('area','fyear'),fitteds=c('area','fyear'))
    imp$fyear = as.integer(as.character(imp$fyear))
    imp = within(imp,{
      mean[n<10] = NA
      se[n<10] = NA
    })
    plot = ggplot(imp,aes(x=fyear,y=mean)) +
      geom_point(aes(size=n),alpha=0.5) + geom_line() + geom_errorbar(aes(ymin=mean-se,ymax=mean+se),size=0.3,width=0.3) +
      geom_line(aes(y=fit),col='grey') +
      geom_hline(yintercept=0,linetype=3,colour='grey') +
      scale_size_area() +
      facet_wrap(~area,scales='free_y') + # Free y-axis in case interaction effects fitted
      labs(x='Fishing year',y='Coefficient',size="Records")
    # dev.new(width=25/2.54,height=17/2.54)
    print(plot)
    # Figure(
    #   "Diagnoser.areaYearImpliedPlot",
    #   "Residual implied coefficients (in log space) for area x fishing year interactions.
    #   Implied coefficients (points) are calculated as the normalised fishing year coefficient (grey line)
    #   plus the mean of the standardised residuals in each fishing year and area.
    #   These values approximate the coefficients obtained when an area x year interaction term is fitted,
    #   particularly for those area x year combinations which have a substantial proportion of the records.
    #   The error bars indicate one standard error of the standardised residuals.
    #   Combinations with less than 10 records are not shown."
    # )
  }

  targetYearImpliedPlot <- function(.){
    imp = .$implieds(bys=c('target','fyear'),fitteds=c('target','fyear'))
    imp$fyear = as.integer(as.character(imp$fyear))
    imp = within(imp,{
      mean[n<10] = NA
      se[n<10] = NA
    })
    plot = ggplot(imp,aes(x=fyear,y=mean)) +
      geom_point(aes(size=n),alpha=0.5) + geom_line() + geom_errorbar(aes(ymin=mean-se,ymax=mean+se),size=0.3,width=0.3) +
      geom_line(aes(y=fit),col='grey') +
      geom_hline(yintercept=0,linetype=3,colour='grey') +
      scale_size_area() +
      facet_wrap(~target,scales='free_y') + # Free y-axis in case interaction effects fitted
      labs(x='Fishing year',y='Coefficient',size="Records")
    # dev.new(width=25/2.54,height=17/2.54)
    print(plot)
    # Figure(
    #   "Diagnoser.areaYearImpliedPlot",
    #   "Residual implied coefficients (in log space) for target x fishing year interactions.
    #   Implied coefficients (points) are calculated as the normalised fishing year coefficient (grey line)
    #     plus the mean of the standardised residuals in each fishing year and target
    #   These values approximate the coefficients obtained when an target x year interaction term is fitted,
    #     particularly for those target x year combinations which have a substantial proportion of the records.
    #   The error bars indicate one standard error of the standardised residuals.
    #   Combinations with less than 10 records are not shown."
    # )
  }

  targetMonthImpliedPlot <- function(.){
    imp = .$implieds(bys=c('target','month'),fitteds=c('target','month','areaMonth'))
    imp$monthi = as.integer(imp$month)
    imp = within(imp,{
      mean[n<10] = NA
      se[n<10] = NA
    })
    plot = ggplot(imp,aes(x=monthi,y=mean)) +
      geom_point(aes(size=n),alpha=0.5) + geom_line() + geom_errorbar(aes(ymin=mean-se,ymax=mean+se),size=0.3,width=0.3) +
      geom_line(aes(y=fit),col='grey') +
      geom_hline(yintercept=0,linetype=3,colour='grey') +
      scale_x_continuous(breaks=1:12,labels=levels(imp$month))+
      scale_size_area() +
      facet_wrap(~target,scales='free_y') + # Free y-axis in case interaction effects fitted
      labs(x='Month',y='Coefficient',size="Records") +
      theme(axis.text.x=element_text(angle=90))
    # dev.new(width=25/2.54,height=17/2.54)
    print(plot)
    # Figure(
    #   "Diagnoser.targetMonthImpliedPlot",
    #   "Residual implied coefficients (in log space) for target x month interactions.
    #     Implied coefficients (points) are calculated as the normalised month coefficients (grey line)
    #       plus the mean of the standaridised residuals in each month and target combination.
    #     These values approximate the coefficients obtained when a target x month interaction term is fitted,
    #       particularly for those target x month combinations which have a substantial proportion of the records.
    #     The error bars indicate one standard error of standardised residuals.
    #     Combinations with less than 10 records are not shown."
    # )
  }

  areaMonthImpliedPlot <- function(.){
    imp = .$implieds(bys=c('area','month'),fitteds=c('area','month','areaMonth'))
    imp$monthi = as.integer(imp$month)
    imp = within(imp,{
      mean[n<10] = NA
      se[n<10] = NA
    })
    plot = ggplot(imp,aes(x=monthi,y=mean)) +
      geom_point(aes(size=n),alpha=0.5) + geom_line() + geom_errorbar(aes(ymin=mean-se,ymax=mean+se),size=0.3,width=0.3) +
      geom_line(aes(y=fit),col='grey') +
      geom_hline(yintercept=0,linetype=3,colour='grey') +
      scale_x_continuous(breaks=1:12,labels=levels(imp$month))+
      scale_size_area() +
      facet_wrap(~area,scales='free_y') + # Free y-axis in case interaction effects fitted
      labs(x='Month',y='Coefficient',size="Records") +
      theme(axis.text.x=element_text(angle=90))
    # dev.new(width=25/2.54,height=17/2.54)
    print(plot)
    # Figure(
    #   "Diagnoser.areaMonthImpliedPlot",
    #   "Residual implied coefficients (in log space) for area x month interactions.
    #     Implied coefficients (black points) are calculated as the normalised month coefficients (grey line)
    #       plus the mean of the standaridised residuals in each month and area.
    #     These values approximate the coefficients obtained when an area x month interaction term is fitted,
    #       particularly for those area x month combinations which have a substantial proportion of the records.
    #     The error bars indicate one standard error of standardised residuals.
    #     Combinations with less than 10 records are not shown."
    # )
  }

  #' @param select Selection criteria for data
  #' @param size Size of cells
  #' @param box Restrict to lat,lon
  #' @param thresh Mininimum number of record per grid cell
  position_plot <- function(select, size=0.1, box, thresh=30) {
    # Implied coefficients include any terms with a spatial component
    terms = vector()
    for(term in c('area','area_month')){
      if(paste('fit.',term,sep='') %in% names(data)) terms = c(terms,paste('fit.',term,sep=''))
    }
    # Do filter
    if (missing(select)) {
      temp <- data
    } else {
      temp <- data[eval(substitute(select), envir=data),]
    }
    # Truncated lat/lon (to 0.1 degree) is used in some summaries
    temp <- temp %>%
      mutate(
        lat_t = sign(lat) * (floor(abs(lat)/size)+1)*size,
        lon_t = sign(lon) * floor(abs(lon)/size)*size
      ) %>%
      group_by(lat_t,lon_t) %>%
      summarise_(.dots=list(
        n = 'length(residual)',
        implied = paste('mean(', paste(terms,collapse='+'), ' + residual, na.rm=T)')
      ))
    #Remove cells with a low number of records
    temp <- subset(temp,n>=thresh)
    #Determine suitable lat and lon ranges
    if (missing(box)) {
      temp <- subset(temp,lat_t<(-30) & lat_t>(-50) & lon_t>160 & lon_t<200)
      latr <- quantile(temp$lat_t,p=c(0.01,0.99),na.rm=T)
      latr <- c(latr[1]-0.5,latr[2]+0.5)
      lonr <- quantile(temp$lon_t,p=c(0.01,0.99),na.rm=T)
      lonr <- c(lonr[1]-0.5,lonr[2]+0.5)
      box <- c(latr,lonr)
    }
    #Plot it
    ggplot(temp,aes(x=lon_t,y=lat_t)) +
      geom_tile(aes(fill=implied)) +
      scale_fill_gradient2('Coefficient',low="blue",mid='grey',high="red") +
      geom_polygon(data=clipPolys(coast,ylim=box[1:2],xlim=box[3:4]),aes(x=X,y=Y,group=PID),fill='white',colour="grey80") +
      scale_y_continuous("",limits=box[1:2],expand=c(0,0)) +
      scale_x_continuous("",limits=box[3:4],expand=c(0,0)) +
      labs(x='',y='') + coord_map(project="mercator") + theme_bw()
  }

  posMonthImpliedPlot <- function(.,thresh=30){
    imp = .$implieds(bys=c('latt','lont','month'),fitteds=c('area','month','areaMonth'))
    #Remove cells with a low number of records
    imp = subset(imp,n>=thresh)
    #Determine suitable lat and lon ranges
    imp = subset(imp,latt<(-30) & latt>(-50) & lont>160 & lont<200)
    latr = quantile(imp$latt,p=c(0.01,0.99),na.rm=T)
    latr = c(latr[1]-0.5,latr[2]+0.5)
    lonr = quantile(imp$lont,p=c(0.01,0.99),na.rm=T)
    lonr = c(lonr[1]-0.5,lonr[2]+0.5)
    #Plot it
    # dev.new(width=18/2.54,height=25/2.54)
    print (
      ggplot(imp,aes(x=lont,y=latt)) +
        geom_tile(aes(fill=mean))+scale_fill_gradient2('Coefficient',low="blue",mid='grey',high="red") +
        geom_polygon(data=clipPolys(coast,ylim=latr,xlim=lonr),aes(x=X,y=Y,group=PID),fill='white',colour="grey80") +
        scale_y_continuous("",limits=latr,expand=c(0,0)) +
        scale_x_continuous("",limits=lonr,expand=c(0,0)) +
        facet_wrap(~month) +
        labs(x='',y='') + coord_map(project="mercator")
    )
    # Figure(
    #   "Diagnoser.posMonthImpliedPlot",
    #   paste("Residual implied coefficients for latitude and longitude grid cells in each month.
    #       Only cells with at least",thresh,"records are shown.
    #       Implied coefficients are calculated as the sum of the normalised coefficients for any
    #         model terms relating to area and month (month, area and area x month terms)
    #         plus the mean of the standardised residual for position in each month.
    #       This plot is intended to show what the combination of model fit and residuals imply about seasonality in localised catch rates.")
    # )
  }

  posMonthCatchPlot <- function(.){
    sp = ddply(subset(.$data,as.integer(as.character(fyear))>=2008),.(latt,lont,month),function(sub)with(sub,data.frame(catch=sum(catch))))
    print(ggplot(sp,aes(x=lont,y=latt))+geom_tile(aes(fill=log(catch)))+scale_fill_gradientn(colours=rev(rainbow(10,end=0.7)))+facet_wrap(~month)+labs(x='',y='')+ylim(-42,-36))
  }
  posSeasonResidPlot <- function(.){
    sp = ddply(.$data,.(latt,lont,season),function(sub)with(sub,data.frame(mean=mean(residual))))
    print(ggplot(sp,aes(x=lont,y=latt))+geom_tile(aes(fill=mean))+scale_fill_gradient2(low="blue",mid='grey',high="red")+facet_wrap(~season)+labs(x='',y='')+ylim(-42,-36))
  }

  depthResidPlot <- function(.,thresh=30){
    #breaks = unique(quantile(.$data$depth,p=seq(0.01,0.99,0.05),na.rm=T)) #Unique is sometimes necessary because some of the quantiles can be the same. This causes problems with cut below.
    lo = quantile(.$data$depth,p=0.01,na.rm=T)
    hi = quantile(.$data$depth,p=0.99,na.rm=T)
    breaks = seq(lo,hi,(hi-lo)/30)
    mids = breaks[1:(length(breaks)-1)]+diff(breaks)/2
    data = within(.$data,{
      depthc = mids[cut(depth,breaks,labels=F)]
    })

    dev.new(width=17/2.54,height=12/2.54)

    #sp = ddply(data,.(depthc),function(sub)with(sub,data.frame(mean=mean(residual),se=sd(residual)/sqrt(length(residual)))))
    #print(ggplot(sp,aes(x=depthc,y=mean))+geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-se,ymax=mean+se),size=0.3,width=0.3)+labs(x='',y='')+geom_hline(yintercept=0,col='grey',linetype=2))
    #Figure(
    #  "Diagnoser.depthResidPlot",
    #  "Mean and standard error of residuals by depth."
    #)

    sp = ddply(data,.(depthc,month),function(sub)with(sub,data.frame(mean=mean(residual),n=length(residual),se=sd(residual)/sqrt(length(residual)))))
    sp = subset(sp,n>=thresh)
    print(ggplot(sp,aes(x=depthc,y=mean))+geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-se,ymax=mean+se),size=0.3,width=0.3)+labs(x='Depth (m)',y='Residual')+facet_wrap(~month)+geom_hline(yintercept=0,col='grey',linetype=2))
    # Figure(
    #   "Diagnoser.depthMonthResidPlot",
    #   paste("Mean and standard error of residuals by depth and month. Only points with at least",thresh,"records are shown")
    # )

    sp = ddply(data,.(depthc,area),function(sub)with(sub,data.frame(mean=mean(residual),n=length(residual),se=sd(residual)/sqrt(length(residual)))))
    sp = subset(sp,n>=thresh)
    print(ggplot(sp,aes(x=depthc,y=mean))+geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-se,ymax=mean+se),size=0.3,width=0.3)+labs(x='Depth (m)',y='Residual')+facet_wrap(~area)+geom_hline(yintercept=0,col='grey',linetype=2))
    # Figure(
    #   "Diagnoser.depthAreaResidPlot",
    #   paste("Mean and standard error of residuals by depth and area. Only points with at least",thresh,"records are shown")
    # )

    sp = ddply(data,.(depthc,target),function(sub)with(sub,data.frame(mean=mean(residual),n=length(residual),se=sd(residual)/sqrt(length(residual)))))
    sp = subset(sp,n>=thresh)
    print(ggplot(sp,aes(x=depthc,y=mean))+geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-se,ymax=mean+se),size=0.3,width=0.3)+labs(x='Depth (m)',y='Residual')+facet_wrap(~target)+geom_hline(yintercept=0,col='grey',linetype=2))
    # Figure(
    #   "Diagnoser.depthTargetResidPlot",
    #   paste("Mean and standard error of residuals by depth and target species. Only points with at least",thresh,"records are shown")
    # )

  }

  lognormal <- function(.,to){
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

  environment()
}
