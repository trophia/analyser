#A simple wrapper around the 'influ' package

Influencer <- Influence$proto()

Influencer$new <- function(.,model){
  #As a temporary kludge, create log(catch) so unstandardised index is clauclated OK
  if(!('log(catch)' %in% names(model$model))) model$model[,'log(catch)'] = log(model$model$catch)
  inst =.$proto(model=model,response='log(catch)',focus=NULL)
  inst$init()
  #Set some options
  inst$orders['vessel'] = 'coef'
  inst$labels['fyear'] = 'Fishing year'
  #Do calculations
  inst$calc()
  inst
}

Influencer$report <- function(.){
  summary = within(.$summary,{
    devPerc = round(devProp * 100,2)
    overall = round(overall*100,2)
  })
  Table(
    summary[,c('term','df','dev','devPerc','aic','overall')],
    label = 'Influencer.Summary',
    header = c('Term','Degrees of freedom','Deviance explained','Deviance explained (%)','AIC','Influence overall (%)'),
    caption = 'Summary of the influence of each term in the standardisation model.'
  )

  dev.new(width=16/2.54,height=16/2.54)
  molt = melt(.$indices[,c(1,6:ncol(.$indices))],id.vars='level')
  molt$level = as.integer(as.character(molt$level))
  print(
    ggplot(molt,aes(x=level,y=value,group=variable,shape=variable)) + geom_point(size=4) + geom_line() + scale_shape_manual(values=1:30) + 
      labs(x='Fishing year',y='CPUE Index',shape='') + ylim(0,max(molt$value)) + geom_hline(yintercept=1,col='grey')
  )
  Figure(
    'Influencer.Step',
    'Annual indices of CPUE as each term is succesively added to the model. The indices are normalised to an overall geometric mean of 1.'
  )

  dev.new(width=16/2.54,height=16/2.54)
  .$cdiPlotAll(done=function(term){
    Figure(
	paste('Influencer.',term,sep=''),
	paste('Coefficient-distribution-influence plot for <i>',term,'</i>.',sep='')
    )
  })

  dev.new(width=16/2.54,height=13/2.54)
  molt = melt(.$influences,id.vars='level')
  molt$level = as.integer(as.character(molt$level))
  print(
    ggplot(molt,aes(x=level,y=exp(value),group=variable,shape=variable)) + geom_point(size=4) + geom_line() + scale_shape_manual(values=1:30) + 
      labs(x='Fishing year',y='Influence',shape='') + ylim(0,max(exp(molt$value))) + geom_hline(yintercept=1,col='grey')
  )
  Figure(
    'Influencer.Influence',
    'Annual influence for each term in the model.'
  )

  dev.new(width=16/2.54,height=13/2.54)
  .$stanPlot()
  Figure(
    'Influencer.Standardization',
    'Overall standardization effect of the model. The unstandardised index is based on the geometric mean of the catch per strata and is not adjusted for effort.'
  )
}
