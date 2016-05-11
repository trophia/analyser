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
        summaries <- c(summaries,
            catch = ~sum(catch)
        )

        data <<- data %>% group_by_(.dots=by) %>% summarise_(.dots=summaries)

    })()

    environment()
}
