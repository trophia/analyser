#A simple wrapper around the 'influ' package
#require(influ)

Influencer <- Influence$proto()

Influencer$new <- function(.,model,data,focus=NULL){
  #Attach a data attribute so that influ works on survreg objects
  attr(model,"data") = data

  inst =.$proto(model=model,response=NULL,focus=focus)
  inst$init()
  
  #Set some options
  inst$orders['vessel'] = 'coef'
  inst$labels['fyear'] = 'Fishing year'

  inst
}

Influencer$report <- function(.){
  table = within(.$summary[,c('term','k','logLike','aic','r2','r2Dev','r2Negel','overall')],{
    r2 = round(r2*100,2)
    r2Dev = round(r2Dev*100,2)
    r2Negel = round(r2Negel*100,2)
    overall = round(overall*100,2)
  })
  Table(
    table,
    label = 'Influencer.Summary',
    header = c('Term','Coefficients','Log likelihood','AIC','R2 (%)', 'Deviance pseudo-R2 (%)', 'Negelkerke pseudo-R2 (%)','Overall influence (%)'),
    caption = 'Summary of the explanatory power and influence of each term in the standardisation model.
      Coefficients is the number of coefficients associated with the term added.
      Log likelihood and AIC values are for the fit as each term is successively added.
      Coefficient of determination (R2) values represent the change in R2 from the the previous model.
      R2: square of the correlation coefficient between log(observed) and log(fitted).'
  )

  dev.new(width=16/2.54,height=12/2.54)
  .$stanPlot()
  Figure(
    'Influencer.Standardization',
    'Overall standardization effect of the model. The unstandardised index is based on the geometric mean of the catch per strata and is not adjusted for effort.'
  )

  #dev.new(width=8/2.54,height=16/2.54)
  #.$stepPlot()
  #Figure(
  #  'Influencer.Step',
  #  'Annual indices of CPUE as each term is succesively added to the model. The indices are normalised to an overall geometric mean of 1.'
  #)

  #dev.new(width=8/2.54,height=16/2.54)
  #.$influPlot()
  #Figure(
  #  'Influencer.Influence',
  #  'Annual influence for each term in the model.'
  #)

  dev.new(width=16/2.54,height=18/2.54)
  .$stepAndInfluPlot()
  Figure(
    'Influencer.StepAndInfluence',
    'Step and influence plot'
  )

  dev.new(width=25/2.54,height=16/2.54)
  .$cdiPlotAll(done=function(term){
    Figure(
    	paste('Influencer.',term,sep=''),
    	paste('Coefficient-distribution-influence plot for <i>',term,'</i>.',sep='')
    )
  })

}
