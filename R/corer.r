#' Select a core vessel set
#' 
#' @param data Data to operate on
#' @param catch_min Minimum catch for a trip to qualify
#' @param trips_levels Levels of qualifying trips per year to summarise across
#' @param years_levels Levels of qualifying years to summarise across
#' @param trips Qualifying trips per year for selection
#' @param years Qualifying years for selection
Corer <- function(data_in, catch_min=1, trips_min=3, years_min=3){
  
  data <- NULL
  vessel_years_trips <- NULL
  vessels <- NULL
  
  (function(){
    # Summary of qualifying trips by year for each vessel
    # Saved because reused in summary plots and tables
    vessel_years_trips <<- data_in %>% 
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
    data <<- data_in %>% filter(vessel %in% vessels)
    
  })()
  
  fyear_summary <- function(){
    shared_fyear_summary(data)
  }
  
  criteria_plot <- function(trips_levels=c(1,3,5,10), years_levels=1:10){
    # Lists of vessels that meet each combination of criteria
    qualify <- function(.){
      trips_ <- max(.$trips)
      years_ <- max(.$years)
      vessel_years_trips %>% 
        filter(trips>=trips_) %>% 
        group_by(vessel) %>%
        summarise(years=length(fyear)) %>%
        filter(years>=years_)
    }
    criteria_vessels <- expand.grid(trips_min=trips_levels, years_min=years_levels) %>%
      group_by(trips_min, years_min) %>%
      do(vessels=qualify(.))
    # Calculate number of vessels and catch for each combination
    criteria_vessels <- criteria_vessels %>% bind_cols(
      criteria_vessels %>%
        do(data.frame(
          num = nrow(.$vessels),
          catch = sum(data_in$catch[data_in$vessel %in% .$vessels$vessel])
        ))
    )
    # Normalise catches
    criteria_vessels <- within(criteria_vessels, {
      catch <- catch / sum(data_in$catch) * 100
    })

    # Plot it
    plot_base <- ggplot(criteria_vessels,aes(x=years_min,colour=factor(trips_min),shape=factor(trips_min))) + 
      scale_shape_manual(values=1:10) +
      labs(x='Minimum year', colour='Minimum trips', shape='Minimum trips')

    plot1 <- plot_base + geom_point(aes(y=catch),size=2,alpha=0.7) + 
      ylim(0,NA) + labs(x='', y='Percentage of catch') + theme(legend.position='top')

    plot2 <- plot_base + geom_point(aes(y=num),size=2,alpha=0.7) +
      ylim(0,NA) + labs(y='Number of vessels') + theme(legend.position='none')

    grid.arrange(plot1, plot2, heights = c(0.6, 0.5))
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

  cpue_plot <- function(){
    temp <- bind_rows(
      data_in %>%
        mutate(
          vessels = ifelse(vessel %in% vessels, 'core', 'others')
        ) %>%
        group_by(fyear,vessels) %>%
        summarise(
          num = length(effort),
          caught = mean(catch>0),
          rate = geomean(catch/effort)
        ),
      data_in %>%
        mutate(
          vessels = 'all'
        ) %>%
        group_by(fyear,vessels) %>%
        summarise(
          num = length(effort),
          caught = mean(catch>0),
          rate = geomean(catch/effort)
        )
    )

    plot1 <- ggplot(temp, aes(x=fyear, y=caught, colour=vessels, shape=vessels)) +
      geom_point(aes(size=num)) + geom_line() +
      scale_size_area() +
      ylim(0,NA) + 
      scale_shape_manual(values=1:3) +
      labs(x='', y='Catch probability (proportion catch>0)', colour='', shape='', size='Strata')

    plot2 <- ggplot(temp, aes(x=fyear, y=rate, colour=vessels, shape=vessels)) +
      geom_point(aes(size=num)) + geom_line() +
      scale_size_area() +
      ylim(0,NA) +
      scale_shape_manual(values=1:3) +
      labs(x='Fishing year', y='Catch magnitude (kg per effort unit)', colour='', shape='', size='Strata')

    grid.arrange(plot1, plot2, heights = c(0.5, 0.5))
  }
  
  environment()
}
