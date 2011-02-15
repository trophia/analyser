
Reporter <- Worker$proto(
  folder = '.',
  workers = NULL
)

Reporter$do = function(.){
  html = file(paste(.$folder,"/index.html",sep='/'),"w")
  for(worker in .$workers) worker$report(to=html)#folder=.$folder,
}
