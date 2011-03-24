library(proto)

Adder <- Worker$proto(
  label = "Adder",
  files = NULL #The filename for the additional data
)

Adder$do <- function(.,data){
  .$done = list()
  for(file in .$files){
    #Read in data file
    extra = read.table(file,header=T,sep="\t")
    #Merge in extra data
    #Relies on the colunns in the file having the same headers as the data
    nd = names(data)
    ne = names(extra)
    data = merge(data,extra,all.x=T)
    #Record
    .$done[[file]] = list(
      by = ne[ne %in% nd],
      extra = ne[!(ne %in% nd)]
    )
  }
  #Return data
  data
}

Adder$report <- function(.,to=""){
  .$header(c('files','done'),to=to)
  for(file in names(.$done)){
    details = .$done[[file]]
    cat("<h2>",file,"</h2>",file=to)
    cat("<p>Merged by:",paste(details$by,collapse=","),"</p>",file=to)
    cat("<p>Added fields:",paste(details$extra,collapse=","),"</p>",file=to)
  }
}

