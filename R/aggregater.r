library(dplyr)

#' Aggregate data
Aggregater <- function(data,by){

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

        data <<- data %>% group_by_(.dots=by) %>% summarise_(.dots=summaries)

    })()
  
    fyear_summary <- function(){
      shared_fyear_summary(data)
    }

    environment()
}
