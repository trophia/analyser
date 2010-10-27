library(proto)

Aggregater <- proto(
  by = NULL,
  first = NULL,
  sum = NULL,
  mean = NULL
)

Aggregater$do <- function(.,data){
  if(!is.null(.$by)){
    #Create a list of by variables
    by = list()
    for(name in .$by) by[[name]] = data[,name]
    #Do aggregations
    levels = aggregate(rep(NA,nrow(data)),by,function(x)NULL)[,.$by]
    if(!is.null(.$first)) levels = cbind(levels,aggregate(data[,.$first],by,head,n=1)[,.$first])
    if(!is.null(.$sum)) levels = cbind(levels,aggregate(data[,.$sum],by,sum,na.rm=T)[,.$sum])
    if(!is.null(.$mean)) levels = cbind(levels,aggregate(data[,.$mean],by,mean,na.rm=T)[,.$mean])
    #Return aggregated data
    return(levels)
  }
  #Return data
  return(data)
}

Aggregater$report <- function(.,to=""){
  cat("<h1>Aggregater</h1>",file=to)
  cat("<p>By : ",paste(.$by,collapse=","),"</p>",file=to)
  cat("<p>First : ",paste(.$first,collapse=","),"</p>",file=to)
  cat("<p>Sum : ",paste(.$sum,collapse=","),"</p>",file=to)
  cat("<p>Mean : ",paste(.$mean,collapse=","),"</p>",file=to)
}
