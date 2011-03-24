library(proto)

Subsetter <- Worker$proto(
  label = "Subsetter",
  criteria = NULL,
  size = NULL
)

Subsetter$do <- function(.,data){
  #Exclude records where cpueno==1
  data = subset(data,is.na(cpueno)|cpueno==0)
  #Subset using specified expression
  if(!is.null(.$criteria)) data = subset(data,eval(.$criteria))
  #Sample to Size if specified
  if(!is.null(.$size)) data = data[sample(1:nrow(data),min(nrow(data),.$size)),]
  #Create summary
  .$summary = ddply(data,.(fyear),function(sub) data.frame(
    events=nrow(sub),
    vessels=length(unique(sub$vessel)),
    trips=length(unique(sub$trip)),
    catch=sum(sub$catch/1000,na.rm=T),
    effort_number=sum(sub$num,na.rm=T),
    effort_duration=sum(sub$duration,na.rm=T),
    percent_zero=sum(sub$catch==0,na.rm=T)/nrow(sub)*100
  ))
  #Return data
  data
}

Subsetter$report <- function(.,to=""){
  .$header(c('criteria','size'),to=to)
  .$table(
    .$summary,
    caption='Summary of the resulting subset of data by fishing year.',
    header=c('Fishing year','Events','Vessels','Trips','Catch (t)',
	      'Effort number','Effort duration (hrs)','Zero catch<br>(landed,% records)'),
    to=to
   )
}
