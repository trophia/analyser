library(dplyr)

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

        #Create summary
        summary <<- data %>% group_by(fyear) %>% summarise(
            vessels=length(unique(vessel)),
            trips=length(unique(trip)),
            events=sum(events),
            effort_number=sum(num,na.rm=T),
            effort_duration=sum(duration,na.rm=T),
            catch=sum(catch/1000,na.rm=T),
            catch_positive=round(sum(catch>0,na.rm=T)/length(catch)*100,2)
        )
    })()

    environment()
}
