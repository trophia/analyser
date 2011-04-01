Reporter <- Worker$proto(
  workers = NULL
)

Reporter$do = function(.){
  html = file("report.html","w")
  for(worker in .$workers) worker$report(to=html)
}
