Combiner <- Worker$proto(
  label = "Combiner",

  data = NULL,
  thresh = 0.01,
  levels = NULL,
  alsos = NULL,
  nots = NULL
)

Combiner$new <- function(.,data,thresh = 0.01,levels = NULL,alsos = NULL,nots=NULL){
  inst = .$proto(data=data,thresh=thresh,levels=levels,alsos=alsos,nots=nots)
  inst$init()
  inst
}

Combiner$init <- function(.){
  for(name in names(.$data)){
    if(is.character(.$data[,name]) & !(name %in% .$nots)){
      if(name %in% names(.$levels)){
	#Use the specified levels and put everything else into other
	.$data[!(.$data[,name] %in% .$levels[[name]]),name] = 'Other'
      }
      else if(!is.null(.$thresh)){
	#Determine the sorted proportional frequency on each value and group those levels that are less than .$thresh into 'Other'
	freq = sort(base::table(.$data[,name]),decreasing=T)
	freq = freq/sum(freq)
	others = names(freq[freq<.$thresh])
	.$data[.$data[,name] %in% others,name] = 'Other'
      }
      #Convert to a factor with Other the last in the list of levels
      levels = sort(unique(.$data[,name]))
      if(sum(.$data[,name]=='Other')>0) levels = c(levels[levels!='Other'],'Other')
      .$data[,name] = factor(.$data[,name],levels=levels,ordered=T)
    }
    else if(name %in% .$alsos){
      #Convert to a factor
      .$data[,name] = factor(.$data[,name])
    }
  }
}

Combiner$report <- function(.){

}
