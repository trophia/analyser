#A simple wrapper around the 'influ' package

Influencer <- Influence$proto()

Influencer$new <- function(.,model){
  inst =.$proto(model=model,response=NULL,focus=NULL)
  inst$init()
  #Set some options
  inst$orders['vessel'] = 'coef'
  inst$labels['fyear'] = 'Fishing year'
  #Do calculations
  inst$calc()
  inst
}

Influencer$report <- function(.){
  Table(
    .$summary,
    label = 'Influencer.Summary',
    header = c('Term','Degrees of freedom','Deviance explained','Deviance explained (%)','AIC','Influence overall','Influence trend'),
    caption = 'Summary of the influence of each term in the standardisation model.'
  )

  dev.new(width=16/2.54,height=7/2.54)
  par(mar=c(4,4,1,1))
  .$stepPlot()
  Figure(
    'Influencer.Step',
    'Changes in annual indices of CPUE as each term is added to the model.'
  )

  .$influPlot()
  Figure(
    'Influencer.Influence',
    'Annual influence for each term in the model'
  )

  .$stanPlot()
  Figure(
    'Influencer.Standardization',
    'The standardization effect of the model.'
  )

  dev.new(width=16/2.54,height=16/2.54)
  .$cdiPlotAll(done=function(term){
    Figure(
	paste('Influencer.',term,sep=''),
	paste('Coefficient-distribution-influence plot for <i>',term,'</i>.',sep='')
    )
  })
}
