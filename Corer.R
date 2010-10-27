library(proto)
library(reshape)
library(ggplot2)

Corer <- Worker$proto(
  trips = NULL, #Minimum number of trips in a year
  years = NULL  #Minimum number of qualifying years
)

Corer$summaryPlot = function(.){
  #Separate method because called in do() and in report()
  par(mfrow=c(2,1))
  with(subset(.$summary,trips %in% c(3,5,10)),{
    par(mar=c(0,4,4,1))
    plot(catch*100~years,pch=(trips),xaxt='n',ylab='Catch (%)',las=1)
    legend('topright',legend=1:length(unique(trips)),pch=unique(trips),bty='n',title='Trips')
    par(mar=c(4,4,0,1))
    plot(vessels~years,pch=(trips),xlab='Years',ylab='Vessels',las=1)
  })
}

Corer$do <- function(.,data){
  #Calculate trips per year per vessel
  temp = aggregate(list(trips=data$trip),list(vessel=data$vessel,fyear=data$fyear),function(trip)length(unique(trip)))
  #Calculate numbers of vessels and catch using alternative qualification criteria
  vesselsList = list()
  .$summary = ddply(expand.grid(trips=1:10,years=1:20),.(trips,years),function(sub){
    vessels = subset(ddply(subset(temp,trips>=sub$trips),.(vessel),function(subsub)data.frame(years=nrow(subsub))),years>=sub$years)$vessel
    catch = sum(subset(data,vessel %in% vessels)$catch,na.rm=T)
    vesselsList[[paste(sub$trips,sub$years)]] <<- vessels
    data.frame(catch=catch,vessels=length(vessels))
  })
  #Calculate catch as a proportion
  .$summary = within(.$summary,{catch=catch/sum(data$catch,na.rm=T)})
  #Prompt for parameters if need be
  if(is.null(.$trips) | is.null(.$years)){
     #Generate graphs for user
    .$summaryPlot()
    .$trips = as.numeric(readline("Enter minimum number of trips per year per vessel"))
    .$years = as.numeric(readline("Enter minimum number of qualifying years (years with minimumm number of trips) per vessel"))
  }
  #Determine qualitfying vessels
  .$vessels = vesselsList[[paste(.$trips,.$years)]]
  #Subset data accordingly
  data = subset(data,vessel %in% .$vessels)
  #Return data
  return(data)
}

Corer$report <- function(.,data,to=""){
  cat("<h1>Corer</h1>",file=to)

  cat("<p>Trips:</p>",.$trips,"<p>Years:</p>",.$years,file=to)
  cat("<p>Core vessels :</h1>",paste(.$vessels,collapse=","),file=to)

  dev.new(width=8,height=5)
  .$summaryPlot()
  .$figure('Corer.Selection','Examination of parameters for defining core vessels.',to=to)

  dev.new(width=8,height=5)
  p = ggplot(ddply(data,.(vessel,fyear),function(sub)c(trips=length(unique(sub$trip)))),aes(x=fyear,y=factor(vessel))) + geom_point(aes(size=trips),shape=1) + scale_area('Trips',to=c(0,15))  + labs(x='Fishing year',y='Vessel')
  print(p)
  .$figure('Corer.Bubble','Distribution of strata by fishing year for core vessels. Area of circles is proportional to the proportion of records over all fishing years and vessels.',to=to)

  dev.new(width=8,height=5)
  vesselYears = aggregate(list(n=data$fyear),list(fyear=data$fyear,vessel=data$vessel),length)
  vesselYears = aggregate(list(n=vesselYears$fyear),list(vessel=vesselYears$vessel),length)
  hist(vesselYears$n,breaks=1:length(unique(data$fyear)),col='grey',xlab='Fishing years',main='')
  .$figure('Corer.Histogram','Histogram of the number of years with data for each core vessel.',to=to)

}
