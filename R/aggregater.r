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
        for(var in c('fyear','month','date','trip','vessel','area','target','area_month', 'zone2')){
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

    #' Rollup plots summarise the degree of 'rolling up' done by aggreagation
    #' and may indicate reporting behaviour in terms of events by strata (as defined)
    #' and catches by strata (compared to by events and trips)
    rollup_plots <- function(){
      temp1 <- data_in %>% group_by(fyear) %>% summarise(
        events_positive = length(unique(event[catch>=1]))/length(unique(event))*100
      )
      temp2 <- data %>% group_by(fyear) %>% summarise(
        events_per_stratum = sum(events)/length(events),
        strata_positive = sum(catch>=1)/length(catch)*100,
        trips_positive = length(unique(trip[catch>=1]))/length(unique(trip))*100
      )
      temp <- left_join(temp1, temp2)

      plot_events <- ggplot(temp,aes(x=fyear,y=events_per_stratum)) +
        geom_point() + geom_line() +
        ylim(0,NA) +
        labs(x='Fishing year', y='Events per stratum')

      plot_catch <- ggplot(temp,aes(x=fyear)) +
        geom_point(aes(y=events_positive,colour='Events',shape='Events')) + geom_line(aes(y=events_positive,colour='Events')) +
        geom_point(aes(y=strata_positive,colour='Strata',shape='Strata')) + geom_line(aes(y=strata_positive,colour='Strata')) +
        geom_point(aes(y=trips_positive,colour='Trips',shape='Trips')) + geom_line(aes(y=trips_positive,colour='Trips')) +
        ylim(0,NA) +
        scale_shape_manual(values=1:3) +
        labs(x='Fishing year', y='Percentage with positive catch',shape='',colour='')

      list(
        events = plot_events,
        catch = plot_catch
      )
    }

    #' Strata plot examine alternative stratum defintions
    strata_plots <- function(){

      # Determine which combinations of factors to use as potential
      # strata definitions
      combos <- c('trip','trip*date')
      methods <- length(unique(data_in$method))>1
      areas <- length(unique(data_in$area))>1
      targets <- length(unique(data_in$target))>1
      vessels <- length(unique(data_in$vessel))>1
      if(methods) combos <- c(combos, 'trip*date*method')
      if(areas) combos <- c(combos, 'trip*date*area')
      if(targets) combos <- c(combos, 'trip*date*target')
      if(vessels) combos <- c(combos, 'vessel*date')
      if(methods & areas) combos <- c(combos, 'trip*date*method*area')
      if(methods & targets) combos <- c(combos, 'trip*date*method*target')
      if(areas & targets) combos <- c(combos, 'trip*date*area*target')
      if(methods & areas & targets) combos <- c(combos, 'trip*date*method*area*target')

      # For each combination, do an aggregation and summarise
      # effort and catch by stratum
      temp_all <- NULL
      for(combo in combos){
        combo_factors <- strsplit(combo, '\\*')[[1]]
        temp <- data_in %>%
          group_by_(.dots = c('fyear',combo_factors)) %>%
          summarise(
            effort = sum(effort, na.rm = TRUE),
            catch = sum(catch, na.rm = TRUE)
          ) %>%
            group_by(fyear) %>%
            summarise(
              effort = mean(effort),
              catch = mean(catch>1)
            )
        temp$combo <- combo
        temp_all <- bind_rows(temp_all,temp)
      }

      plot_effort <- ggplot(temp_all, aes(x=fyear, y=effort, color=combo, shape=combo)) +
        geom_point() + geom_line() +
        ylim(0,NA) +
        labs(x='Fishing year', y='Mean effort units per stratum',color='Stratum definition',shape='Stratum definition') +
        theme(legend.position='top', legend.title = element_text(size = 6), legend.text = element_text(size = 6))

      plot_catch <- ggplot(temp_all, aes(x=fyear, y=catch, color=combo, shape=combo)) +
        geom_point() + geom_line() +
        ylim(0,NA) +
        labs(x='Fishing year', y='Strata with catch (%)')

      list(
        effort = plot_effort,
        catch = plot_catch
      )

    }

    environment()
}
