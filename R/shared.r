#' Calculate the geometric mean of a vector
geomean <- function(x) {
    exp(mean(log(x[x>0]), na.rm=T))
}


shared_fyear_summary <- function(data){
    p1 <- data %>% group_by('Fishing year' = fyear) %>% summarise(
        Vessels = length(unique(vessel)),
        Trips = length(unique(trip)),
        Records = round(sum(events),0),
        'Effort (num)'=sum(num,na.rm=T),
        'Effort (hrs)'= round(sum(duration,na.rm=T),0),
        'Catch (t)'= round(sum(catch,na.rm=T)/1000, 1)
#         'Catch Positive'= round(length(catch>0)/length(catch)*100,2)
    )
    p2 <- data %>% group_by(fyear, trip) %>% summarise('positive' = ifelse(sum(catch) > 0, 1, 0)) %>%
        group_by(fyear) %>% summarise('Trips with catch (landed, \\%)' = round((sum(positive) / n_distinct(trip)) * 100,2))

    p3 <- data %>% group_by(fyear, vessel, date) %>% summarise('positive' = ifelse(sum(catch) > 0, 1, 0)) %>%
        group_by(fyear) %>% summarise('Days with catch (landed, \\%)' = round((sum(positive) / length(positive)) * 100, 2))

    bind_cols(p1, p2[ , 2], p3[ , 2])
}
