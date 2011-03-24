Combiner <- Worker$proto(
  label = "Combiner",
  thresh = 0.01,
  levels = NULL,
  others = NULL
)

globalTableFunc = table
Combiner$do <- function(.,data){
  for(name in names(data)){
    if(is.character(data[,name])){
      if(name %in% names(.$levels)){
	#Use the specified levels and put everything else into other
	data[!(data[,name] %in% .$levels[[name]]),name] = 'Other'
      }
      else if(!is.null(.$thresh)){
	#Determine the sorted proportional frequency on each value and group those levels that are less than .$thresh into 'Other'
	freq = sort(globalTableFunc(data[,name]),decreasing=T)
	freq = freq/sum(freq)
	others = names(freq[freq<.$thresh])
	data[data[,name] %in% others,name] = 'Other'
      }
      #Convert to a factor with Other the last in the list of levels
      levels = sort(unique(data[,name]))
      levels = c(levels[levels!='Other'],'Other')
      data[,name] = factor(data[,name],levels=levels,ordered=T)
    }
    else if(name %in% .$others){
      #Convert to a factor
      data[,name] = factor(data[,name])
    }
  }
  #Return data
  data
}

Combiner$report <- function(.,to=""){
  .$header(c('thresh','levels'),to=to)
}
