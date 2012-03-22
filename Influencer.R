#A simple wrapper around the 'influ' package
require(influ)

Influencer <- Influence$proto()

Influencer$new <- function(.,model,data,focus=NULL){
  #Attach a data attribute so that influ works on survreg objects
  attr(model,"data") = data
  
  #Define response based on type of model
  #response = switch(class(model)[1],
  #  glm = 'catch',
  #  survreg = 'Surv(catch)'
  #)
  
  inst =.$proto(model=model,response=NULL,focus=focus)
  inst$init()
  
  #Set some options
  inst$orders['vessel'] = 'coef'
  inst$labels['fyear'] = 'Fishing year'

  inst
}

Influencer$report <- function(.){
  summary = within(.$summary,{
    overall = round(overall*100,2)
  })
  Table(
    summary[,c('term','df','ss','r2','loglike','aic','overall')],
    label = 'Influencer.Summary',
    header = c('Term','Degrees of freedom','Residual sums of squares','R2 (%)','LogLikelihood','AIC','Influence overall (%)'),
    caption = 'Summary of the influence of each term in the standardisation model.'
  )

  dev.new(width=12/2.54,height=12/2.54)
  .$stanPlot()
  Figure(
    'Influencer.Standardization',
    'Overall standardization effect of the model. The unstandardised index is based on the geometric mean of the catch per strata and is not adjusted for effort.'
  )

  dev.new(width=8/2.54,height=16/2.54)
  .$stepPlot()
  Figure(
    'Influencer.Step',
    'Annual indices of CPUE as each term is succesively added to the model. The indices are normalised to an overall geometric mean of 1.'
  )

  dev.new(width=8/2.54,height=16/2.54)
  .$influPlot()
  Figure(
    'Influencer.Influence',
    'Annual influence for each term in the model.'
  )

  dev.new(width=16/2.54,height=16/2.54)
  .$stepAndInfluPlot()
  Figure(
    'Influencer.StepAndInfluence',
    'Step and influence plot'
  )

  dev.new(width=16/2.54,height=16/2.54)
  .$cdiPlotAll(done=function(term){
    Figure(
    	paste('Influencer.',term,sep=''),
    	paste('Coefficient-distribution-influence plot for <i>',term,'</i>.',sep='')
    )
  })

}
