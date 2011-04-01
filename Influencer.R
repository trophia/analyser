#A simple wrapper around the 'influ' package

Influencer <- Worker$proto()

Influencer$new <- function(.,model){
  inst = .$proto(model=model)
  inst.$influence = Influence$new(model)
  inst.$influence$calc()
  inst
}

Influencer$report <- function(.,to=""){
  .$table(.$influence$summary)
  .$influence$stanPlot()
  .$influence$stepPlot()
  .$influence$influPlot()
  .$influence$cdiPlotAll()#Todo output to a PDF file and wrap as an image.
}

Influencer$far <- function(.,to="",tables=0,figures=0){
  .$influence$stanPlot()
  .$figure('Influencer.Standardization','Standardization effect',to=to)

  .$influence$influPlot()
  .$figure('Influencer.Influence','Annual influence',to=to)


  .$influence$cdiPlotAll(done=function(term){
    .$figure(paste('Influencer.',term,sep=''),paste('Coefficient-distribution-influence plot for',term,'.'),to=to)
  })
}
