library(dplyr)
library(ggplot2)

#' Select a core vessel set
#' 
#' @param data Data to operate on
#' @param catch_min Minimum catch for a trip to qualify
#' @param trips_levels Levels of qualifying trips per year to summarise across
#' @param years_levels Levels of qualifying years to summarise across
#' @param trips Qualifying trips per year for selection
#' @param years Qualifying years for selection
Corer <- function(data, catch_min=1, trips_min=3, years_min=3){
  
  vessel_years_trips <- NULL
  vessels <- NULL
  
  (function(){
    # Summary of qualifying trips by year for each vessel
    # Saved because reused in summary plots and tables
    vessel_years_trips <<- data %>% 
      filter(catch>=catch_min) %>% 
      group_by(vessel,fyear) %>% 
      summarise(trips=length(unique(trip)))
    
    # Core vessel list given the specified trips and years criteria
    vessels <<- as.data.frame(vessel_years_trips %>% 
      filter(trips>=trips_min) %>% 
      group_by(vessel) %>%
      summarise(years=length(fyear)) %>%
      filter(years>=years_min)
    )$vessel
    
    # Core vessel data
    data <<- data %>% filter(vessel %in% vessels)
    
  })()
  
  fyear_summary <- function(){
    shared_fyear_summary(data)
  }
  
  criteria_plot <- function(trips_levels=c(1,3,5,10), years_levels=1:10){
    # Lists of vessels that meet certain criteria
    qualify <- function(.){
      trips_ <- max(.$trips)
      years_ <- max(.$years)
      vessel_years_trips %>% 
        filter(trips>=trips_) %>% 
        group_by(vessel) %>%
        summarise(years=length(fyear)) %>%
        filter(years>=years_) %>%
        select(vessel)
    }
    criteria_vessels <- expand.grid(trips_min=trips_levels, years_min=years_levels) %>%
      group_by(trips_min, years_min) %>%
      do(vessels=qualify(.))
  }
  
  bubble_plot <- function(){
    temp <- vessel_years_trips %>% filter(vessel %in% vessels)
    temp$vessel <- factor(
      match(
        temp$vessel,
        (temp %>% group_by(vessel) %>% summarise(last=max(fyear),first=min(fyear)) %>% arrange(last,first))$vessel
      )
    )
    ggplot(temp,aes(x=fyear,y=vessel,size=trips)) +
      geom_point(alpha=0.5) + scale_size_area(max_size = 10) + 
      labs(x='Fishing year', y='Vessel', size='Trips') +
      theme(axis.text.y=element_text(size=0))
  }
  
  environment()
}
