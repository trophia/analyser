library(dplyr)

#' Aggregate data
Aggregater <- function(data_in,by){

    data <- NULL
    
    (function(){
        summaries <- list()
        
        # For factors, if they are not in `by`, then calculate their mode
        mod <- function(x) {
          ux <- unique(x)
          ux[which.max(tabulate(match(x, ux)))]
        }
        for(var in c('fyear','month','date','trip','vessel','area','target','area_month')){
            if(!(var %in% by)) summaries[[var]] <- eval(parse(text=paste0('~mod(',var,')')))
        }

        # Add the usual summaries of continuous variables
        for(var in c('events','catch','effort','duration','num','num2','total','hooks','netlength')){
          summaries[[var]] <- formula(paste0('~sum(',var,')'))
        }
        for(var in c('depth','height','width','length','speed','temp','bottom','lat','lon')){
          summaries[[var]] <- formula(paste0('~mean(',var,')'))
        }

        data <<- data_in %>% group_by_(.dots=by) %>% summarise_(.dots=summaries)

    })()
  
    fyear_summary <- function(){
      shared_fyear_summary(data)
    }

    rollup_plots <- function(){
      temp1 <- data_in %>% group_by(fyear) %>% summarise(
        events_positive = length(unique(event[catch>0]))/length(unique(event))*100
      )
      temp2 <- data %>% group_by(fyear) %>% summarise(
        events_per_stratum = sum(events)/length(events),
        strata_positive = sum(catch>0)/length(catch)*100,
        trips_positive = length(unique(trip[catch>0]))/length(unique(trip))*100
      )
      temp <- left_join(temp1, temp2)

      plot1 <- ggplot(temp,aes(x=fyear,y=events_per_stratum)) +
        geom_point() + geom_line() + 
        ylim(0,NA) + 
        labs(x='Fishing year', y='Events per stratum')

      plot2 <- ggplot(temp,aes(x=fyear)) +
        geom_point(aes(y=events_positive,colour='Events',shape='Events')) + geom_line(aes(y=events_positive,colour='Events')) + 
        geom_point(aes(y=strata_positive,colour='Strata',shape='Strata')) + geom_line(aes(y=strata_positive,colour='Strata')) + 
        geom_point(aes(y=trips_positive,colour='Trips',shape='Trips')) + geom_line(aes(y=trips_positive,colour='Trips')) + 
        ylim(0,NA) + 
        scale_shape_manual(values=1:3) +
        labs(x='Fishing year', y='Percentage with positive catch',shape='',colour='')

      list(events=plot1, positive=plot2)
    }

    environment()
}
