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
  r2thresh = 0
){
  inst = .$proto(data=data,initial=initial,terms=terms,r2thresh=r2thresh)
  inst
}

#Add a method for extracting the logLike of a survreg model so that 
#Stepper$calc will work for those types of models
logLik.survreg = function(model) model$loglik[2]
fitted.survreg = function(model) predict(model,type='response')

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
  .$summary =  NULL
  #An intercept only model used for Nagelkerke's pseudo-R2
  llIntercept = as.numeric(logLik(update(.$initial,.~1)))
  #Keeper functions records each step
  stepLast = c()
  keeper = function(model,aic){
    modelTerms = attr(model$terms,"term.labels")
    ##Can't just use the last term in modelTerms as they are not always in order (interactions always seem to be put at the end)
    ##So have to use stepLast
    term = modelTerms[!(modelTerms %in% stepLast)]
    stepLast <<- modelTerms
    n = length(residuals(model))
    df = n - df.residual(model)
    ll = as.numeric(logLik(model))
    
    #Deviance pseudo-R2 can only be calculated fro some models
    r2Dev = (model$null.deviance-model$deviance)/model$null.deviance
    if(length(r2Dev)==0) r2Dev = NA
    #Negelkerke pseudo-R2 should be available for all models
    r2Negel = (1-exp((llIntercept-ll)*(2/n)))/(1-exp(llIntercept*(2/n)))
    .$summary <<- rbind(.$summary,c(Term=term,N=n,DF=df,LL=ll,AIC=aic,R2Dev=r2Dev,R2Negel=r2Negel))
    
    aic #Seems that you have to return something
  }
  stepped = step(
    .$initial,
    .$terms,
    direction='forward',
    keep=keeper,
  )
  .$summary = within(as.data.frame(.$summary,stringsAsFactors=F),{
    Term = as.character(Term)
    DF = as.integer(DF)
    LL = as.numeric(LL)
    AIC = as.numeric(AIC)
    R2Dev = round(as.numeric(R2Dev)*100,2)
    R2Negel = round(as.numeric(R2Negel)*100,2)
  })

  #Create a final model based on R2 increase criterion
  .$summary$Final = rep('',length(.$summary$Term))
  finalTerms = vector()
  #First term, fyear, is always in 
  .$summary[.$summary$Term=='fyear','Final'] = '*'
  finalTerms = c(finalTerms,'fyear')
  for(i in 2:length(.$summary$Term)){
    if(.$summary$R2Negel[i]-.$summary$R2Negel[i-1]>=.$r2thresh) {
    	finalTerms = c(finalTerms,.$summary$Term[i])
    	.$summary$Final[i] = '*'
    } 
    else break
  }
  
  #Create final model
  .$formula =  as.formula(paste('~',paste(finalTerms,collapse='+')))
  .$final = update(.$initial,.$formula)
}

Stepper$report <- function(.){
  Paragraph(
    'Forward stepwise selection of model terms was done on the basis of the Akaike Information Criterion (AIC). 
    The maximal set of model terms offered to the stepwise selection algorithm was <p><i>',as.character(.$terms),'</i></p> 
    with the term <i>fyear</i> forced into the model. Terms were only added to the model if they increased the percent deviance explained by ',.$r2thresh,'%. ',
    'Stepper.Summary provides a summary of the changes in the deviance explained and in AIC as each term was added to the model. 
    The final model formula was <p><i>',as.character(.$formula),'</i></p>'
  )

  Table(
    .$summary[,c('Term','DF','LL','AIC','R2Dev','R2Negel','Final')],
    label = 'Stepper.Summary',
    caption= "Summary of stepwise selection. Model terms are listed in the order of acceptance to the model. 
      AIC: Akaike Information Criterion; *: Term included in final model.",
    header = c('Term','DF','Log likelihood','AIC','Deviance pseudo-R2 (%)','Nagelkerke pseudo-R2 (%)','')
  )
}
