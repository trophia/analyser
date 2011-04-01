library(proto)

Deriver <- Worker$proto(
  label = "Deriver",

  data = NULL,
  func = NULL
)

Deriver$new <- function(.,data,func=NULL){
  inst = .$proto(data=data,func=func)
  inst$init()
  inst
}

Deriver$init <- function(.){
  #Some automatically calculated fields
  moonPhase = function(date){
    newmoon = ISOdatetime(1988,1,4,1,40,0,tz="GMT") #First full moon in 1988. From http://aa.usno.navy.mil/cgi-bin/aa_moonphases.pl?year=1988&ZZZ=END
    period = 29.5306 #Synodic period from http://en.wikipedia.org/wiki/Synodic_period
    utc = as.POSIXct(date,tz="UTC")-12*60*60 #Convert NZ time to UTC (aka GMT)
    phase = (as.numeric(utc-newmoon) %% period)/period
    phase[!is.na(phase) & phase<0] = 1+phase[!is.na(phase) & phase<0]
  }
  .$data = within(.$data,{
    distance = duration * speed
    moon = moonPhase(date)    
  })
  #Run custom function
  if(!is.null(.$func)) .$data = within(.$data,eval(.$func))
  #Ensure catch is in function
  if((!'catch' %in% names(.$data))|(!'effort' %in% names(.$data))) stop(simpleError("An expression for both catch and effort must be defined"))
}

Deriver$report <- function(.,to=""){
  .$header(to=to)
}
