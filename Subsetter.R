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
  .$summary = ddply(.$data,.(fyear),function(sub) data.frame(
    vessels=length(unique(sub$vessel)),
    trips=length(unique(sub$trip)),
    events=sum(sub$events),
    effort_number=sum(sub$num,na.rm=T),
    effort_duration=sum(sub$duration,na.rm=T),
    catch=sum(sub$catch/1000,na.rm=T),
    percent_positive=round(sum(sub$catch>0,na.rm=T)/nrow(sub)*100,2)
  ))
}

Subsetter$report <- function(.){
  Html('<p>The data used for this CPUE standardisation was defined by the following criteria:<ul>')
  criteria = .$criteria
  criteria = gsub('==','was equal to',criteria)
  criteria = gsub('%in%','was among the set',criteria)
  criteria = gsub('c\\(','\\(',criteria)
  criteria = gsub('\\|','or',criteria)
  criteria = strsplit(criteria,'&')[[1]]
  for(criterion in criteria) Html('<li>',criterion,'</li>')
  Html('</ul>')

  .$summary$fyear = as.character(.$summary$fyear) #Prevents 'commaring'
  vesRange = range(.$summary$vessels)
  vessMinYear = with(.$summary,fyear[which.min(vessels)])
  p0Range = round(range(.$summary$percent_positive,na.rm=T))
  Html(
    '@Subsetter.Summary summarises the number of fishing events, vessels, trips, effort and catch in the resultant dataset. 
    The minimum number of vessels was ',vesRange[1],' in ',vessMinYear,'. 
    The percentage of poisitive catches ranged from ',p0Range[1],'% to ',p0Range[2],'%.')

  Html('</p>')

  Table(
    .$summary,
    label = 'Subsetter.Summary',
    caption = 'Summary by fishing year of the data subset used for this analysis.',
    header = c('Fishing year','Vessels','Trips','Events',
	      'Effort (num)','Effort (hrs)',
	      'Catch (t)','Events with catch<br>(landed,%)')
   )
}