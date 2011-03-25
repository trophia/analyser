Stepper <- Worker$proto(
  label = "Stepper",
  variable = expression(catch),
  family = gaussian(link='identity'),
  terms = expression(fyear),
  r2thresh = 0.01
)

Stepper$do <- function(.,data){
  #Clean data by removing records which have NAs or a indefinite (e.g. log(0)) for any terms
  .$cleaning = data.frame(Term=NA,NAs=NA,LogZeros=NA,stringsAsFactors=F)
  .$dataClean = data
  #Split terms expression into individual terms.
  terms = strsplit(as.character(.$terms),'[ ]*\\+[ ]*')[[1]]
  for(term in terms){
    if(length(grep(':',term) )==0) { #Ignore interaction terms
      #Extract the field name form the term
      field = gsub("(log|poly|,[ ]*[0123456789]+)|\\(|\\)","",term)
      #Remove records where the field is NA
      .$dataClean = .$dataClean[!is.na(.$dataClean[,field]),]
      #If the term involves a log then remove records where the field is <0
      logged = length(grep(paste('log\\(',field,'\\)',sep=''),term))>0
      if(logged) .$dataClean = .$dataClean[.$dataClean[,field]>0,]
      #Add to cleaning summary
      .$cleaning = rbind(.$cleaning,c(Term=term,NAs=sum(is.na(data[,field])),LogZeros=if(logged)sum(data[,field]<=0) else '-'))
    }
  }
  .$cleaning = .$cleaning[-1,]
  data = .$dataClean

  #Do stepwise selection of terms
  null = glm(as.formula(paste(as.character(.$variable),'~1')),data=data,family=.$family)
  .$summary =  data.frame(Term="-",AIC=null$aic,N=length(null$residuals),DF=0,Deviance=null$deviance,R2=0.0,SDSR=NA,MASR=NA,stringsAsFactors=F)
  .$indices = data.frame(fyear=sort(unique(data$fyear)))
  #Keeper functions records each stepLast
  extractCoeffs = function(model,name){
    coeffs = summary(model)$coeff
    index = substr(row.names(coeffs),1,nchar(name))==name
    coeffs = c(0,coeffs[index,1])
    coeffs
  }
  stepLast = c()
  keeper = function(model,aic){
    modelTerms = attr(model$terms,"term.labels")
    ##Can't just use the last term in modelTerms as they are not always in order (interactions always seem to be put at the end)
    ##So have to use stepLast
    term = modelTerms[!(modelTerms %in% stepLast)]
    stepLast <<- modelTerms
    
    n = length(model$residuals)
    df = n - model$df.residual
    r2 = (model$null.deviance-model$deviance)/model$null.deviance
    resids = rstandard(model)
    sdsr = sd(resids,na.rm=T)
    masr = median(abs(resids),na.rm=T)
    .$summary <<- rbind(.$summary,c(term,aic,n,df,model$deviance,r2,sdsr,masr))

    .$indices[,paste('+',term)] = extractCoeffs(model,"fyear")

    aic #Seems that you have to return something
  }
  stepped = step(
    glm(as.formula(paste(as.character(.$variable),'~fyear')),data=data,family=.$family),
    as.formula(paste(as.character(.$variable),'~',as.character(.$terms))),
    direction='forward',
    keep=keeper,
  )
  .$summary$Term = as.character(.$summary$Term)
  .$summary$AIC = as.numeric(.$summary$AIC)
  .$summary$DF = as.integer(.$summary$DF)
  .$summary$Deviance = as.numeric(.$summary$Deviance)
  .$summary$R2 = as.numeric(.$summary$R2)

  #Create a final model based on R2 increase criterion
  if(!is.null(.$r2thresh)){
    .$summary$Final = rep('',length(.$summary$Term))
    #Fishing year is always in
    .$summary$Final[.$summary$Term=='fyear'] = '*'
    finalTerms = vector()
    for(i in 3:length(.$summary$Term)){
      if(.$summary$R2[i]-.$summary$R2[i-1]>=.$r2thresh) {
	finalTerms = c(finalTerms,.$summary$Term[i])
	.$summary$Final[i] = '*'
      } 
      else break
    }
  }
  
  #Create final model
  .$finalFormula = as.formula(paste(as.character(.$variable),'~fyear+',paste(finalTerms,collapse='+')))
  .$final = glm(.$finalFormula,data=data,family=.$family)

  #Return the final model
  .$final
}

Stepper$report <- function(.,to=""){
  .$header(c("variable","family","terms","r2thresh"),to=to)

  cat("<h2>Stepwise selection<h2>",file=to)
  .$table(.$summary,"Summary of stepwise selection. Model terms are listed in the order of accdeptance to the model.AIC: Akaike Information Criterion, R2: Proportion of deviance explained. Final: Whether or not variable was included in final model.",to=to)
  print(ggplot(melt(.$indices,id.vars='fyear'),aes(x=fyear,y=value,group=variable,shape=variable,linetype=variable)) + geom_point() + geom_line() + scale_shape_manual(values=1:30) + labs(x='Fishing year',y='Coefficient',shape="Term",linetype="Term"))
  .$figure("Stepper.Indices","Annual coefficients at each step in the term selection process.",to=to)
  
  cat("<h2>Final model<h2><pre>",file=to)
  sink(to)
  print(anova(.$final))
  sink()
  cat("</pre>",file=to)
}
