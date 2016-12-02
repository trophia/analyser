#' Subset data
Subsetter <- function(data, criteria, size=NULL){

    criteria <- substitute(criteria)
    summary <- NULL

    (function(){
        # Exclude records where cpueno==1
        data <<- subset(data,is.na(cpueno)|cpueno==0)

        # Subset using specified criteria expression
        if(!is.null(criteria)) data <<- subset(data,eval(criteria))

        # Sample to size if specified
        if(!is.null(size)) data <<- data[sample(1:nrow(data),min(nrow(data),size)),]
    })()

    fyear_summary <- function(){
      shared_fyear_summary(data)
    }

    environment()
}
