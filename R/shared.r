library(dplyr)

shared_fyear_summary <- function(data){
  data %>% group_by(fyear) %>% summarise(
    vessels=length(unique(vessel)),
    trips=length(unique(trip)),
    events=sum(events),
    effort_number=sum(num,na.rm=T),
    effort_duration=sum(duration,na.rm=T),
    catch=sum(catch,na.rm=T),
    catch_positive=round(sum(catch>0,na.rm=T)/length(catch)*100,2)
  )
}
