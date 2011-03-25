#A simple wrapper around the 'influ' package

Influencer <- Worker$proto()

Influencer$do <- function(.,model){
  .$influence = Influence$new(model)
  .$influence$calc()
}

Influencer$report <- function(.,to=""){
  .$table(.$influence$summary)
  .$influence$stanPlot()
  .$influence$stepPlot()
  .$influence$influPlot()
  .$influence$cdiPlotAll()#Todo output to a PDF file and wrap as an image.
}
