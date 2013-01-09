Indexer <- Worker$proto(
  label = 'Indexer',

  datasets = NULL,
  models = NULL,
  term = 'fyear',
  others = NULL,

  indices = NULL
)

Indexer$new <- function(.,datasets=NULL,models=NULL,term='fyear',others=NULL){
  inst = .$proto(datasets=datasets,models=models,term=term,others=others)
  inst
}

Indexer$calc <- function(.){
  #Create a data.frame that will hold all the CPUE indices that are calculated
  .$indices = NULL

  geomean = function(x) exp(mean(log(x)))

  #Calculate unstandardised indices for each data set
  for(name in names(.$datasets)){
    data = .$datasets[[name]]
    #Calculate indices
    indices = ddply(data,.$term,function(sub)with(sub,data.frame(
      success.prop = sum(sub$catch>0)/nrow(sub),
      ratio.rate = sum(catch)/sum(effort),
      ratio.positive.rate = with(subset(sub,catch>0),sum(catch)/sum(effort)),
      arithmetic.rate = with(subset(sub,effort>0),mean(catch/effort)),
      geometric.rate = with(subset(sub,effort>0 & catch>0),geomean(catch/effort))
    )))
    indices = within(indices,{
      expected.rate = with(indices,success.prop*geometric.rate)
      arithmetic.index = arithmetic.rate/geomean(arithmetic.rate)
      geometric.index = geometric.rate/geomean(geometric.rate)
    })
    #Merge into .$indices
    names(indices) = c(.$term,paste(name,names(indices)[-1],sep='.'))
    .$indices = if(is.null(.$indices)) indices else merge(.$indices,indices,by=.$term,all=T)
  }

  #Calculate standardised indices for models
  for(name in names(.$models)){
    model = .$models[[name]]
    coef = Influencer$coeffs(model,.$term)
    index = exp(coef-mean(coef))
    se = Influencer$ses(model,.$term)
    indices = data.frame(
      term = sort(unique(model.frame(model)[,.$term])),
      index = index,
      se = se
    )
    names(indices) = c(.$term,'index','se')
    #rateBase = with(model$data,geomean(catch/effort))
    #indices = within(indices,{
    #  rate = rateBase * index
    #})
    #Merge into .$indices
    names(indices) = c(.$term,paste(name,names(indices)[-1],sep='.'))
    .$indices = if(is.null(.$indices)) indices else merge(.$indices,indices,by=.$term,all=T)
  }

  #Merge in others
  if(!is.null(.$others)){
    #For others with '.index' in their name, normalise to a geometic mean of 1 for shared time period
    #for(name in names(.$others){
    #  if(grepl('.index',name)){
	#Determine years where overlap
	#overlap = .$others$fyear[.$others$fyear %in% .$indices$fyear]
	#Get 
     # }
    #}
    .$indices = if(is.null(.$indices)) .$others else merge(.$indices,.$others,by=.$term,all=T)
  }
  
  #Convert term to character for table niceness
  .$indices[,.$term] = as.character(.$indices[,.$term])
}

Indexer$comparisonPlot <- function(.,indices=NULL,match=NULL,labels=NULL,ylab=''){
  if(is.null(match)) match = '.index'
  if(is.null(indices)) indices = names(.$indices)[grep(match,names(.$indices))]
  dev.new(width=16/2.54,height=13/2.54)
  data = melt(.$indices[c(.$term,indices)],id.vars=.$term)
  data$variable = labels[match(data$variable,indices)]
  if(.$term=='fyear') data$fyear = as.integer(as.character(data$fyear))
  print(ggplot(data,aes(x=fyear,y=value,group=variable,colour=variable,shape=variable)) + geom_point(size=3) + 
    geom_line() + scale_shape_manual(values=1:30) + 
    labs(x='Fishing year',y=ylab,colour='',shape='') + ylim(0,max(data$value,na.rm=T)))
}

Indexer$report <- function(.){
  if(!is.null(.$indices)){
    #Table of indices
    Table(.$indices,"CPUE indices")
    #Plot of indices divided into the separate groups
    for(type in c('prop','rate','index')){
      indices = .$indices[,c(1,grep(paste('.',type,sep=''),names(.$indices)))]
      with(indices,{
        plot(indices[,.$term],rep(NA,nrow(indices)),ylim=c(0,max(indices[2:ncol(indices)])))
        for(name in names(indices)[2:ncol(indices)]){
            lines(indices[,.$term],indices[,name])
        }
      })
    }
  }
}
