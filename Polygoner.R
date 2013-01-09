Polygoner <- Worker$proto(
  label = "Polygoner",
  
  data = NULL,
  file = NULL,
  order = NULL
)

Polygoner$new <- function(.,data,file,order=NULL){
  inst = .$proto(data=data,file=file,order=order)
  inst$init()
  inst
}

Polygoner$init <- function(.){
  # Read in polygons
  .$polys = read.table(.$file,header=T)
  # Make polys into a PBSMapping PolySet
  .$polyset = data.frame(
    PID=as.integer(.$polys$zone),
    Y=.$polys$lat,
    X=.$polys$lon
  )
  .$polyset$POS = 1:nrow(.$polys)
  .$polyset = as.PolySet(.$polyset)
  .$polyset = fixPOS(.$polyset)
  # Applies areas to the dataset and returns a new version of the data with a new column called "zone"
  ed = as.EventData(data.frame(
    EID = .$data$event,
    X = .$data$lon,
    Y = .$data$lat
  ))
  ps = findPolys(ed,.$polyset,maxRows=nrow(ed)*2)
  .$data$zone = levels(.$polys$zone)[ps$PID[match(.$data$event,ps$EID)]]
  # Apply an other zone
  .$data$zone[is.na(.$data$zone)] = 'Other'
  .$data$zone = factor(.$data$zone,levels=c(.$order,'Other'),ordered=T)
  .$data
}

Polygoner$report = function(.){
  latr = c(-50,-30)
  lonr = c(160,190)
  
  plot = ggplot() + 
    geom_polygon(data=clipPolys(d1000,ylim=latr,xlim=lonr),aes(x=X,y=Y,group=SID),fill='grey90') +
    geom_polygon(data=clipPolys(d500,ylim=latr,xlim=lonr),aes(x=X,y=Y,group=SID),fill='grey80') +
    geom_polygon(data=clipPolys(d200,ylim=latr,xlim=lonr),aes(x=X,y=Y,group=SID),fill='grey70') +
    geom_point(data=.$data,aes(x=lon,y=lat),colour=hsv(0.6,0.8,0.8,0.1)) +
    geom_polygon(data=clipPolys(coast,ylim=latr,xlim=lonr),aes(x=X,y=Y,group=PID),fill='grey60') + 
    geom_polygon(data=.$polys,aes(x=lon,y=lat,fill=zone),alpha=0.2,colour='black') +
    scale_y_continuous("",limits=latr,expand=c(0,0)) + 
    scale_x_continuous("",limits=lonr,expand=c(0,0)) +
    coord_map(project="mercator")
  print(plot)
  Figure(
    "Polygoner.map",
    "Map of zones"
  )
  
  temp <- ddply(.$data,.(zone),nrow)
  names(temp) <- c('Zone','Records')
  Table(
      temp,
      "Polygoner.count",
      "Records by lat/lon zone as defined in in figure above"
  )
}
