Stepper <- Worker$proto(
  label = "Stepper",
  
  data = NULL,
  initial = NULL,
  terms = NULL,
  r2thresh = NULL
)

Stepper$new <- function(.,
  data,
  initial,
  terms = expression(fyear),
  r2thresh = NULL)
{
  inst = .$proto(data=data,initial=initial,terms=terms,r2thresh=r2thresh)
  inst
}

Stepper$calc <- function(.){
  #Clean data by removing records which have NAs or a indefinite (e.g. log(0)) for any terms
  .$cleaning = data.frame(Term=NA,NAs=NA,LogZeros=NA,stringsAsFactors=F)
  dataOrig = .$data
  #Split terms expression into individual terms.
  terms = strsplit(as.character(.$terms)[3],'[ ]*\\+[ ]*')[[1]]
  for(term in terms){
    if(length(grep('[:\\*]+',term) )==0) { #Ignore interaction terms
      #Extract the field name form the term
      field = gsub("(log|poly|,[ ]*[0123456789]+)|\\(|\\)","",term)
      #Remove records where the field is NA
      .$data = .$data[!is.na(.$data[,field]),]
      #If the term involves a log then remove records where the field is <0
      logged = length(grep(paste('log\\(',field,'\\)',sep=''),term))>0
      if(logged) .$data = .$data[.$data[,field]>0,]
      #Add to cleaning summary
      .$cleaning = rbind(.$cleaning,c(Term=term,NAs=sum(is.na(dataOrig[,field])),LogZeros=if(logged)sum(dataOrig[,field]<=0) else '-'))
    }
  }
  .$cleaning = .$cleaning[-1,]

  #Do stepwise selection of terms
  null = .$initial
  .$summary =  NULL
  .$indices = data.frame(fyear=sort(unique(.$data$fyear)))
  #Keeper functions records each stepLast
  extractCoeffs = function(model,term){
    #Get coefficients
    coeffs = model$coefficients
    #Determine the index of the term
    index = match(term,attr(model$terms,"term.labels"))
    #Use model matrix to determine which coeffs are associated with this term
    row = attr(model.matrix(model),"assign")==index
    #Get the relevant coefficents
    coeffs = c(0,coeffs[row])
    coeffs
  }
  stepLast = c()
  keeper = function(model,aic){
    modelTerms = attr(model$terms,"term.labels")
    ##Can't just use the last term in modelTerms as they are not always in order (interactions always seem to be put at the end)
    ##So have to use stepLast
    term = modelTerms[!(modelTerms %in% stepLast)]
    stepLast <<- modelTerms
    
    n = length(residuals(model))
    df = n - model$df.residual
    dev = model$deviance
    r2 = (model$null.deviance-model$deviance)/model$null.deviance
    .$summary <<- rbind(.$summary,c(Term=term,N=n,DF=df,Deviance=dev,R2=r2,AIC=aic))
    .$indices[,paste('+',term)] = extractCoeffs(model,"fyear")
    
    aic #Seems that you have to return something
  }
  stepped = step(
    .$initial,
    .$terms,
    direction='forward',
    #keep=keeper,
  )
  .$summary$Term = as.character(.$summary$Term)
  .$summary$AIC = as.numeric(.$summary$AIC)
  .$summary$DF = as.integer(.$summary$DF)
  .$summary$Deviance = as.numeric(.$summary$Deviance)
  .$summary$R2 = round(as.numeric(.$summary$R2)*100,2)

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
  .$finalFormula =  formula(stepped)#as.formula(paste(as.character(.$variable),'~fyear+',paste(finalTerms,collapse='+')))
  .$final = stepped#glm(.$finalFormula,data=.$data,family=.$family)
}

Stepper$report <- function(.){
  formula = as.character(.$finalFormula)
  formula = paste(formula[2],'~',formula[3])
  Paragraph(
    'Forward stepwise selection of model terms was done on the basis of the Akaike Information Criterion (AIC). The maximal set of model terms offered to the stepwise selection algorithm was <p><i>',as.character(.$terms),'</i></p> 
    with the term <i>fyear</i> forced into the model. Terms were only added to the model if they increased the percent deviance explained by ',.$r2thresh,'%. ',
    'Stepper.Summary provides a summary of the changes in the deviance explained and in AIC as each term was added to the model. The final model formula was <p><i>',formula,'</i></p>'
  )

  #Table(
  #  .$summary[,c('Term','DF','Deviance','R2','AIC','Final')],
  #  label = 'Stepper.Summary',
  #  caption= "Summary of stepwise selection. Model terms are listed in the order of acceptance to the model. AIC: Akaike Information Criterion; *: Term included in final model.",
  #  header = c('Term','DF','Deviance','Deviance<br>explained (%)','AIC','')
  #)

  #print(ggplot(melt(.$indices,id.vars='fyear'),aes(x=fyear,y=value,group=variable,shape=variable,linetype=variable)) + geom_point() + geom_line() + scale_shape_manual(values=1:30) + labs(x='Fishing year',y='Coefficient',shape="Term",linetype="Term"))
  #Figure(
  #  "Stepper.Indices",
  #  "Annual coefficients at each step in the term selection process."
  #)
}
