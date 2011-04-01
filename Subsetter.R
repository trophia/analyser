library(proto)

Subsetter <- Worker$proto(
  label = "Subsetter",

  data = NULL,
  criteria = NULL,
  size = NULL
)

Subsetter$new <- function(.,data,criteria=NULL,size=NULL){
  inst = .$proto(data=data,criteria=criteria,size=size)
  inst$init()
  inst
}

Subsetter$init <- function(.){
  #Exclude records where cpueno==1
  data = subset(.$data,is.na(cpueno)|cpueno==0)
  #Subset using specified expression
  if(!is.null(.$criteria)) .$data = subset(.$data,eval(.$criteria))
  #Sample to Size if specified
  if(!is.null(.$size)).$data = .$data[sample(1:nrow(.$data),min(nrow(.$data),.$size)),]
  #Create summary
  .$summary = ddply(.$data,.(factor(fyear)),function(sub) data.frame(
    events=nrow(sub),
    vessels=length(unique(sub$vessel)),
    trips=length(unique(sub$trip)),
    catch=sum(sub$catch/1000,na.rm=T),
    effort_number=sum(sub$num,na.rm=T),
    effort_duration=sum(sub$duration,na.rm=T),
    percent_zero=round(sum(sub$catch==0,na.rm=T)/nrow(sub)*100,2)
  ))
}

Subsetter$report <- function(.,to=""){
  .$header(c('criteria','size'),to=to)
  .$table(
    .$summary,
    caption='Summary of the resulting subset of data by fishing year.',
    header=c('Fishing year','Events','Vessels','Trips',
	      'Effort number','Effort duration (hrs)',
	      'Catch (t)','Zero catch<br>(landed,% records)'),
    to=to
   )
}

Subsetter$far <- function(.,to="",prefix='',tables=0,figures=0){
  cat('<p>The data used for this CPUE standardisation was defined by the following criteria:</p><ul>',file=to)
  criteria = .$criteria
  criteria = gsub('==','was equal to',criteria)
  criteria = gsub('%in%','was among the set',criteria)
  criteria = gsub('c\\(','\\(',criteria)
  criteria = gsub('\\|','or',criteria)
  criteria = strsplit(criteria,'&')[[1]]
  for(criterion in criteria) cat('<li>',criterion,'</li>',file=to)
  cat('</ul>',file=to)

  vesRange = range(.$summary$vessels)
  vessMinYear = with(.$summary,fyear[which.min(vessels)])
  p0Range = round(range(.$summary$percent_zero,na.rm=T))
  cat(
    'Table ',.$prefix,'1 summarises the number of fishing events, vessels, trips, effort and catch in the resultant dataset by fishing year.',
    'The minimum number of vessels was',vesRange[1],'in',vessMinYear,'.',
    'The percentage of zero catches ranged from ',p0Range[1],'% to',p0Range[2],'%.'
  ,file=to)

  cat('</p>',file=to)

  tables = tables + 1
  .$table(
    .$summary,
    caption=paste('Table',prefix,tables,': Summary of the resulting subset of data by fishing year.'),
    header=c('Fishing year','Events','Vessels','Trips',
	      'Effort number','Effort duration (hrs)',
	      'Catch (t)','Zero catch<br>(landed,% records)'),
    to=to
   )
}