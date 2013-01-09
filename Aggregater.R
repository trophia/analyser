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
    vessels=length(unique(sub$vessel)),
    trips=length(unique(sub$trip)),
    strata=nrow(sub),
    events=sum(sub$events,na.rm=T),
    events_per_strata = sum(sub$events,na.rm=T)/nrow(sub),
    effort_number=sum(sub$num,na.rm=T),
    effort_duration=sum(sub$duration,na.rm=T),
    catch=sum(sub$catch,na.rm=T)/1000,
    trips_pos = length(unique(subset(sub,catch>0)$trip))/length(unique(sub$trip))*100,
    strata_pos = sum(sub$catch>0,na.rm=T)/nrow(sub)*100
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
    .$summary,
    label = 'Aggregater.Summary',
    caption = 'Summary of aggregated data by fishing year.',
    header = c('Fishing year','Vessels','Trips','Strata','Events','Events per stratum','Effort (num)','Effort(hrs)',
               'Catch (t)','Trips with catch (%)','Strata with catch (%)'
               )
    )
}
