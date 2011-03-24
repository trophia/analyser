library(proto)

Indexer <- Worker$proto(
  data = NULL,
  models = NULL
)

Indexer$do <- function(.){
  #Create a data.frame that will hold all the CPUE indices that are calculated
  .$indices = NULL

  geomean = function(x) exp(mean(log(x)))

  #Calculate unstandardised indices for each data set
  for(name in names(.$data)){
    data = .$data[[name]]
    #Calculate indices
    indices = ddply(data,.(fyear),function(sub)with(sub,data.frame(
      success.prop = sum(sub$catch>0)/nrow(sub),
      ratio.rate = sum(catch)/sum(effort),
      ratio.positive.rate = with(subset(sub,catch>0),sum(catch)/sum(effort)),
      arithmetic.rate = mean(catch/effort),
      geometric.rate = with(subset(sub,catch>0),geomean(catch/effort))
    )))
    indices = within(indices,{
      expected.rate = with(indices,success.prop*geometric.rate)
      arithmetic.index = arithmetic.rate/geomean(arithmetic.rate)
      geometric.index = geometric.rate/geomean(geometric.rate)
    })
    #Merge into .$indices
    names(indices) = c('fyear',paste(name,names(indices)[-1],sep='.'))
    .$indices = if(is.null(.$indices)) indices else merge(.$indices,indices,by='fyear',all.x=T)
  }

  coeffs = function(model,term){
    coeffs = summary(model)$coeff
    rows = substr(row.names(coeffs),1,nchar(term))==term
    c(0,coeffs[rows,1])
  }
  ses = function(model,term){
    summ = summary(model)
    V = summ$cov.scaled
    row = substr(row.names(V),1,nchar(term))==term
    V = V[row,row]
    n = sum(row)+1
    Q = matrix(-1/n, nrow=n, ncol=n-1)
    Q[-1,] = Q[-1,] + diag(rep(1,n-1))
    V0 = (Q%*%V) %*% t(Q)
    se = sqrt(diag(V0))
    se
  }

  #Calculate standardised indices for models
  for(name in names(.$models)){
    model = .$models[[name]]
    coef = coeffs(model,'fyear')
    index = exp(coef-mean(coef))
    indices = data.frame(
      fyear = sort(unique(model$data$fyear)),
      index = index
    )
    rateBase = with(model$data,geomean(catch/effort))
    indices = within(indices,{
      rate = rateBase * index
    })
    #Merge into .$indices
    names(indices) = c('fyear',paste(name,names(indices)[-1],sep='.'))
    .$indices = if(is.null(.$indices)) indices else merge(.$indices,indices,by='fyear',all.x=T)
  }
}

Indexer$report <- function(.,to=""){
  .$header(c('data','model'),to=to)
  if(!is.null(.$indices)){
    #Table of indices
    .$table(.$indices,"CPUE indices",to=to)
    #Plot of indices divided into the separate groups
    for(type in c('prop','rate','index')){
      indices = .$indices[,c(1,grep(paste('.',type,sep=''),names(.$indices)))]
      with(indices,{
	plot(fyear,rep(NA,nrow(indices)),ylim=c(0,max(indices[2:ncol(indices)])))
	for(name in names(indices)[2:ncol(indices)]){
	    lines(fyear,indices[,name])
	}
      })
    }
  }
  else cat("Not done",file=to)
}
