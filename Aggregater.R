Aggregater <- Worker$proto(
  label = "Aggregater",

  data = NULL,
  by = NULL,
  first = NULL,
  sum = NULL,
  mean = NULL,

  summary = NULL
)

Aggregater$new <- function(.,data,by=NULL,first = NULL,sum = NULL,mean = NULL){
  inst = .$proto(data=data,by=by,first=first,sum=sum,mean=mean)
  inst$init()
  inst
}

Aggregater$init <- function(.){
  if(!is.null(.$by)){
    #Create a list of by variables
    by = list()
    for(name in .$by) by[[name]] = .$data[,name]
    #Do aggregations
    levels = aggregate(rep(NA,nrow(.$data)),by,function(x)NULL)[,.$by]
    if(!is.null(.$first)) levels = cbind(levels,aggregate(.$data[,.$first],by,head,n=1)[,.$first])
    if(!is.null(.$sum)) levels = cbind(levels,aggregate(.$data[,.$sum],by,sum,na.rm=T)[,.$sum])
    if(!is.null(.$mean)) levels = cbind(levels,aggregate(.$data[,.$mean],by,mean,na.rm=T)[,.$mean])
    #Return aggregated data
    .$data = levels
  }
  #Create summary
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
}

Aggregater$report <- function(.){
  Paragraph(
    'Data were aggregated into strata were each strata was defined as a unque combination of ',paste(.$by,collapse=', '),'. ',
    '@Aggregater.Summary summarise the extent of "roll-up" (the number of orginial events associated with each stratum) and the number, proportion and 
    effort from strata that had positive catches.'
  )

  .$summary$fyear = as.character(.$summary$fyear) #Prevents 'commaring'
  Table(
    .$summary[,c('fyear','strata','events_per_strata','strata_pos','percent_zero','trips_pos','effort_number_pos','effort_duration_pos')],
    label = 'Aggregater.Summary',
    caption = 'Summary by fishing year of the data after aggregation to strata.',
    header = c('Fishing year','Strata','Events per stratum',#'Events','Vessels','Trips','Catch (t)','Effort num','Effort duration (hrs)',
      'Strata with positive catch','Strata with zero catch (%)','Trips with positive catch','Total effort units from strata with positive catch','Total effort duration from strata with positive catch')
  )
}
