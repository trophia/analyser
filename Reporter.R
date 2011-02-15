
Reporter <- Worker$proto(
  workers = NULL
)

Reporter$do = function(.){
  to = file("report.html","w")
  for(worker in workers) worker$report(to)
}
