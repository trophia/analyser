#' Loads catch and effort data, adds derived variables and summarises
#' Includes functionality of both legacy `Loader.R` and legacy `Deriver.R`
Loader <- function(file, extras){

    extras <- substitute(extras)
    data <- NULL
    summary <- NULL
    
    (function(){
        #Read in data file
        data <<- read.table(file,header=T,colClasses="character",sep="\t")
        fields <- list(
            event = 'integer',
            version = 'integer',
            trip = 'integer',
            fyear = 'integer',
            month = 'integer',
            method = 'character',
            target = 'character',
            area = 'character',
            vessel = 'numeric',
            form = 'character',
            date = 'character',
            time = 'numeric',
            depth = 'numeric',
            height = 'numeric',
            width = 'numeric',
            length = 'numeric',
            speed = 'numeric',
            temp = 'numeric',
            bottom = 'numeric',
            lat = 'numeric',
            lon = 'numeric',
            inshore = 'integer',
            zone = 'character',
            days = 'numeric',
            duration = 'numeric',
            num = 'integer',
            num2 = 'integer',
            total = 'integer',
            hooks = 'integer',
            netlength = 'numeric',
            cpueno = 'integer',
            events = 'integer' #Used for data that is already aggregated and therefore has this field
        )
        # Create (if necessary) and convert each specified field. This ensures data has the expected fields and field types
        for(field in names(fields)){
            if(!(field %in% names(data))) data[,field] <<- rep(NA,nrow(data))
            data[,field] <<- get(paste('as.',fields[[field]],sep=''))(data[,field])
        }
        # Fields not specified (usually species specific fields) are assumed to be numeric
        for(field in names(data)){
            if(!(field %in% names(fields))) data[,field] <<- as.numeric(data[,field])
        }
        # If fishing events is not defined then set as 1
        if(sum(data$events,na.rm=T)==0) data$events <<- 1

        #Add some derived fields
        moon_phase <- function(date){
            newmoon <- ISOdatetime(1988,1,4,1,40,0,tz="GMT") #First full moon in 1988. From http://aa.usno.navy.mil/cgi-bin/aa_moonphases.pl?year=1988&ZZZ=END
            period <- 29.5306 #Synodic period from http://en.wikipedia.org/wiki/Synodic_period
            utc <- as.POSIXct(date,tz="UTC")-12*60*60 #Convert NZ time to UTC
            phase <- (as.numeric(utc-newmoon) %% period)/period
            phase[!is.na(phase) & phase<0] <- 1+phase[!is.na(phase) & phase<0]
            phase
        }
        data <<- within(data,{
            # Distance etc
            distance <- duration * speed
            # Moon phase
            moon <- moon_phase(date) 
            # Recode vessel to obfuscate vessel_key
            vessel <- as.integer(factor(vessel))
            # Area x month combination for easy incorporation of area:month interactions
            area_month <- paste(area,month,sep=":")
        })

        # Run extras as specified
        data <<- within(data,eval(extras))

        # Ensure catch and effort variables are defined
        if((!'catch' %in% names(data))|(!'effort' %in% names(data))) {
            stop(simpleError("An expression for both catch and effort must be defined"))
        }
        
    })()
    
    fyear_summary <- function(){
      shared_fyear_summary(data)
    }

    environment()
}
