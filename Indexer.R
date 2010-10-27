library(proto)

Indexer <- Worker$proto(
  data = NULL,
  effort = NULL,
  lognormal = NULL,
  binomial = NULL
)

Indexer$do <- function(.){
  #Create a data.frame that will hold all the CPUE indices that are calculated
  .$indices = NULL

  #Calculate unstandardised indices for each data set
  for(name in names(.$data)){
    data = .$data[[name]]
    #Create effort field for convienience
    data$effort = data[,.$effort]
    this = ddply(data,.(fyear),function(sub)with(sub,data.frame(
      success = sum(sub$catch>0)/nrow(sub),
      ratio = sum(catch)/sum(effort),
      ratioForSuccesses = with(subset(sub,catch>0),sum(catch)/sum(effort)),
      arithmetic = mean(catch/effort),
      geometric = with(subset(sub,catch>0),exp(mean(log(catch/effort))))
    )))
    this$expected = with(this,success*geometric)
    names(this) = paste(name,names(this),sep='.')
    .$indices = if(is.null(.$indices)) this else merge(.$indices,this,by='fyear',all.x=T)
  }

  #!todo extract indices from models
}

Indexer$report <- function(.,to=""){
  cat("<h1>Indexer</h1>",file=to)
  if(!is.null(.$indices)){
    .$table(.$indices,"CPUE indices",to=to)
  }
  else cat("Not do()ne",file=to)
}
