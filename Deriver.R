library(proto)

Deriver <- Worker$proto(
  func = NULL
)

Deriver$do <- function(.,data){
  #Some automatically calculated fields
  moonPhase = function(date){
    newmoon = ISOdatetime(1988,1,4,1,40,0,tz="GMT") #First full moon in 1988. From http://aa.usno.navy.mil/cgi-bin/aa_moonphases.pl?year=1988&ZZZ=END
    period = 29.5306 #Synodic period from http://en.wikipedia.org/wiki/Synodic_period
    utc = as.POSIXct(date,tz="UTC")-12*60*60 #Convert NZ time to UTC (aka GMT)
    phase = (as.numeric(utc-newmoon) %% period)/period
    phase[!is.na(phase) & phase<0] = 1+phase[!is.na(phase) & phase<0]
  }
  data = within(data,{
    distance = duration * speed
    moon = moonPhase(date)    
  })
  #Run custom function
  if(!is.null(.$func)) data = within(data,eval(.$func))
  #Ensure catch is in function
  if(!'catch' %in% names(data)) error("An expression for catch has not been defined")
  #Return data
  data
}

Deriver$report <- function(.,to=""){
  cat("<h1>Deriver</h1>",file=to)
  cat("<p>Function:",as.character(.$func),"</p>",file=to)
}
