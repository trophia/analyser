library(dplyr)

shared_fyear_summary <- function(data){
  p1 <- data %>% group_by(fyear) %>% summarise(
    Vessels=length(unique(vessel)),
    Trips=length(unique(trip)),
    Events=sum(events),
    'Effort (num)'=sum(num,na.rm=T),
    'Duration (hrs)'=sum(duration,na.rm=T),
    'Catch (t)'=sum(catch,na.rm=T)/1000,
    'Catch Positive'=round(sum(catch>0,na.rm=T)/length(catch)*100,2)
  )
p2 <- data %>% group_by(fyear, trip) %>% summarise('positive' = ifelse(sum(catch) > 0, 1, 0)) %>%
      group_by(fyear) %>% summarise('Trips caught' = (sum(positive) / n_distinct(trip)) * 100)
      
p3 <- data %>% group_by(fyear, events) %>% summarise('positive' = ifelse(sum(catch) > 0, 1, 0)) %>%
      group_by(fyear) %>% summarise('Events caught' = (sum(positive) / n_distinct(events)) * 100)

bind_cols(p1, p2[ , 2], p3[ , 2])
}
