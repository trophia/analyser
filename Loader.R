Loader <- Worker$proto(
  label = "Loader",
  
  file = NULL,
  
  data = NULL,
  summary = NULL
)

Loader$new <- function(.,file){
  inst = .$proto(file=file)
  inst$init()
  inst
}

Loader$init <- function(.){
   #Read in data file
  .$data = read.table(.$file,header=T,colClasses="character",sep="\t")
  #Create list of field types
  fields = list(
	  event = 'integer',
	  version = 'integer',
	  trip = 'integer',
	  fyear = 'integer',
	  month = 'integer',
	  method = 'character',
	  target = 'character',
	  area = 'character',
	  vessel = 'numeric',
	  form = 'character',
	  date = 'character',
	  time = 'numeric',
	  depth = 'numeric',
	  height = 'numeric',
	  width = 'numeric',
	  length = 'numeric',
	  speed = 'numeric',
	  temp = 'numeric',
	  bottom = 'numeric',
	  lat = 'numeric',
	  lon = 'numeric',
	  inshore = 'integer',
	  zone = 'character',
	  days = 'numeric',
	  duration = 'numeric',
	  num = 'integer',
	  num2 = 'integer',
	  total = 'integer',
	  hooks = 'integer',
	  netlength = 'numeric',
	  cpueno = 'integer',
	  events = 'integer' #Used for data that is already aggregated and therefore has this field
  )
  #Create (if needed) and convert each specified field. This ensures data has the expected fields and field types
  for(field in names(fields)){
    if(!(field %in% names(.$data))) .$data[,field] = rep(NA,nrow(.$data))
    .$data[,field] = get(paste('as.',fields[[field]],sep=''))(.$data[,field])
  }
  #Fields not specified (usually species specific fields) are assumed to be numeric
  for(field in names(.$data)){
    if(!(field %in% names(fields))) .$data[,field] = as.numeric(.$data[,field])
  }
  #Fishing year is converted to character codes
  #.$data = within(.$data,{
  # fyear = paste(substr(as.character(fyear-1),3,4),substr(as.character(fyear),3,4),sep='/')
  #})
  #Create summary
  .$summary = ddply(.$data,.(fyear),function(sub) data.frame(
      events=nrow(sub),
      vessels=length(unique(sub$vessel)),
      trips=length(unique(sub$trip)),
      effort_number=sum(sub$num,na.rm=T),
      effort_duration=sum(sub$duration,na.rm=T),
      cpueno=sum(sub$cpueno,na.rm=T)
  ))
}

Loader$report <- function(.,to=""){
  .$header(c('file'),to=to)

  .$table(
    .$summary,
    caption = 'Summary of the the data read in by fishing year.',
    header = c('Fishing year','Events','Vessels','Trips',
	      'Effort number','Effort duration (hrs)',
	      'Records not suitable for CPUE analysis due to effort grooming'),
    to=to
  )
}

