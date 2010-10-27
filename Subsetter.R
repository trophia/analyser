library(proto)

Subsetter <- Worker$proto(
  criteria = NULL,
  size = NULL
)

Subsetter$do <- function(.,data){
  #Subset using specified expression
  if(!is.null(.$criteria)) data = subset(data,eval(.$criteria))
  #Sample to Size if specified
  if(!is.null(.$size)) data = data[sample(1:nrow(data),min(nrow(data),.$size)),]
  #Reurn data
  data
}

Subsetter$report <- function(.,to=""){
  cat("<h1>Subsetter</h1>",file=to)
  cat("<p>Criteria:",as.character(.$criteria),"</p>",file=to)
  cat("<p>Size:",.$size,"</p>",file=to)
}
