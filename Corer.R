Corer <- Worker$proto(
  label = "Corer",

  data = NULL,
  trips = NULL, #Minimum number of trips in a year
  years = NULL,  #Minimum number of qualifying years

  summary = NULL
)

Corer$new <- function(.,data,trips=NULL,years=NULL){
  inst = .$proto(data=data,trips=trips,years=years)
  inst$init()
  inst
}

Corer$init <- function(.){
  #Calculate trips per year per vessel
  temp = aggregate(list(trips=.$data$trip),list(vessel=.$data$vessel,fyear=.$data$fyear),function(trip)length(unique(trip)))
  #Calculate numbers of vessels and catch using alternative qualification criteria
  vesselsList = list()
  .$combinations = ddply(expand.grid(trips=1:10,years=1:20),.(trips,years),function(sub){
    vessels = subset(ddply(subset(temp,trips>=sub$trips),.(vessel),function(subsub)data.frame(years=nrow(subsub))),years>=sub$years)$vessel
    catch = sum(subset(.$data,vessel %in% vessels)$catch,na.rm=T)
    vesselsList[[paste(sub$trips,sub$years)]] <<- vessels
    data.frame(catch=catch,vessels=length(vessels))
  })
  #Calculate catch as a proportion
  .$combinations = within(.$combinations,{catch=catch/sum(.$data$catch,na.rm=T)})
  #Prompt for parameters if need be
  if(is.null(.$trips) | is.null(.$years)){
     #Generate graphs for user
    .$combinationsPlot()
    .$trips = as.numeric(readline("Enter minimum number of trips per year per vessel"))
    .$years = as.numeric(readline("Enter minimum number of qualifying years (years with minimumm number of trips) per vessel"))
  }
  #Determine qualifying vessels
  .$vessels = vesselsList[[paste(.$trips,.$years)]]
  #Subset data accordingly
  .$data = subset(.$data,vessel %in% .$vessels)
  #Create a summary
  .$summary = ddply(.$data,.(fyear),function(sub) data.frame(
    strata=nrow(sub),
    vessels=length(unique(sub$vessel)),
    trips=length(unique(sub$trip)),
    catch=sum(sub$catch,na.rm=T)/1000,
    effort_number=sum(sub$num,na.rm=T),
    effort_duration=sum(sub$duration,na.rm=T),
    percent_zero=sum(sub$catch<=0,na.rm=T)/nrow(sub)*100,
    events=sum(sub$events,na.rm=T),
    events_per_strata = sum(sub$events,na.rm=T)/nrow(sub),
    strata_pos = sum(sub$catch>0,na.rm=T),
    trips_pos = length(unique(subset(sub,catch>0)$trip)),
    effort_number_pos = sum(subset(sub,catch>0)$num,na.rm=T),
    effort_duration_pos = sum(subset(sub,catch>0)$duration,na.rm=T)
  ))
  #Dump data to file
  write.csv(.$data,file='Corer.Data.csv',row.names=F)
}

Corer$combinationsPlot = function(.){
  #Separate method because called in do() and in report()
  par(mfrow=c(2,1))
  with(subset(.$combinations,trips %in% c(3,5,10)),{
    par(mar=c(0,4,4,1))
    plot(catch*100~years,pch=trips,xaxt='n',ylab='Catch (%)',las=1)
    legend('topright',legend=unique(trips),pch=unique(trips),bty='n',title='Trips')
    par(mar=c(4,4,0,1))
    plot(vessels~years,pch=trips,xlab='Years',ylab='Vessels',las=1)
  })
}

Corer$report <- function(.){
  Html('<p>')

  chosen = subset(.$combinations,trips==.$trips & years==.$years)
  Html(
    'Alternative core vessel selection criteria were investigated by considering the reduction in the number of vessels and percentage of catch (@Corer.Selection). 
    The most appropriate combination of criteria was considered to be to define the core fleet as those vessels that had fished for at least ',.$trips, ' trips in each of at least ',.$years,' years. 
    These criteria resulted in a core fleet size of ',chosen$vessels,' vessels which took ',round(chosen$catch*100),'% of the catch (@Corer.Selection). 
    A histogram of the number of years in which each core vessel had data in the dataset is provided (@Corer.Histogram) as is the overlap of data among core vessels (@Corer.Bubble).'
  )

  dev.new(width=16/2.54,height=10/2.54)
  .$combinationsPlot()
  Figure(
    'Corer.Selection',
    'Examination of parameters for defining core vessels.'
  )

  dev.new(width=16/2.54,height=7/2.54)
  vesselYears = with(.$data,aggregate(list(n=fyear),list(fyear=fyear,vessel=vessel),length))
  vesselYears = with(vesselYears,aggregate(list(n=fyear),list(vessel=vessel),length))
  hist(vesselYears$n,breaks=1:length(unique(.$data$fyear)),col='grey',xlab='Fishing years',main='')
  Figure(
    'Corer.Histogram',
    'Histogram of the number of years with data for each core vessel.'
  )

  dev.new(width=16/2.54,height=16/2.54)
  vesselTrips = ddply(.$data,.(vessel,fyear),function(sub)c(trips=length(unique(sub$trip))))
  vesselTrips$fyear = as.integer(as.character(vesselTrips$fyear))
  vesselRange = ddply(vesselTrips,.(vessel),function(sub)with(sub,data.frame(begin=min(fyear),end=max(fyear))))
  vesselRange = vesselRange[with(vesselRange,order(end,begin)),]
  vesselTrips$vessel = factor(vesselTrips$vessel,levels=vesselRange$vessel,ordered=T)
  p = ggplot(vesselTrips,aes(x=fyear,y=factor(vessel))) + 
    geom_point(aes(size=trips),shape=1) + scale_area('Trips',to=c(0,10))  + labs(x='Fishing year',y='Vessel')
  print(p)
  Figure(
    'Corer.Bubble',
    'Number of trips by fishing year for core vessels. Area of circles is proportional to the proportion of records over all fishing years and vessels.'
  )

  Table(
    .$summary,
    label = 'Corer.Summary',
    caption = 'Summary of core vessel data by fishing year.',
    header = c('Fishing year','Strata','Vessels','Trips','Catch (t)','Effort num','Effort duration (hrs)',
      'Zero catch<br>(landed,% records)','Events','Events per stratum','Strata (+ve)','Trips (+ve)',
      'Effort num (+ve)','Effort duration (+ve)'
    )
  )
}

