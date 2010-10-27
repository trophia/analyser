library(proto)

Loader <- Worker$proto(
  file = 'cpue.txt' #The filename for the data
)

Loader$do <- function(.){
  #Read in data file
  data = read.table(.$file,header=T,colClasses="character",sep="\t")
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
    if(!(field %in% names(data))) data[,field] = rep(NA,nrow(data))
    data[,field] = get(paste('as.',fields[[field]],sep=''))(data[,field])
  }
  #Fields not specified (usually species specific fields) are assumed to be numeric
  for(field in names(data)){
    if(!(field %in% names(fields))) data[,field] = as.numeric(data[,field])
  }
  #Exclude records where cpueno==1
  data = subset(data,is.na(cpueno)|cpueno==0)
  #Return data
  data
}

Loader$report <- function(.,to=""){
  cat("<h1>Loader</h1>",file=to)
  cat("<p>File : ",.$file,"</p>",file=to)
}

