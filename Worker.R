library(proto)
library(plyr)
library(reshape)
library(gtools)
library(ggplot2)
theme_set(theme_bw())
theme_update(
  panel.border = theme_rect(fill = NA, colour = 'black'),
  panel.grid.major = theme_line(colour='white',size=0.1),
  panel.grid.minor = theme_line(colour='white',size=0.1),
  strip.background = theme_rect(fill = 'grey80', colour = 'black'),
  legend.key = theme_rect(colour = 'white'),
  legend.title = theme_text(size=12,hjust = 0)
)
vplayout = function(rows,cols) {grid.newpage(); pushViewport(viewport(layout=grid.layout(ncol=cols,nrow=rows)))}
subplot = function(row,col) viewport(layout.pos.col=col,layout.pos.row=row)


library(PBSmapping)
data(worldLL)

latr = c(-60,-30)
lonr = c(160,180)
coast = clipPolys(worldLL,ylim=latr,xlim=lonr)

if(FALSE){
  #The following are curently not used
  load("/Trophia/Tanga/Data/shared/NZFMA.Rdata")
  load("/Trophia/Tanga/Data/shared/NZbathy.Rdata")
  d100 = clipPolys(as.PolySet(subset(NZbathy$ps,PID==1),projection='LL'),ylim=latr,xlim=lonr)
  d200 = clipPolys(as.PolySet(subset(NZbathy$ps,PID==2),projection='LL'),ylim=latr,xlim=lonr)
  d1000 = clipPolys(as.PolySet(subset(NZbathy$ps,PID==4),projection='LL'),ylim=latr,xlim=lonr)
  stats = clipPolys(statarea.general$ps,ylim=latr,xlim=lonr)
  fmas = clipPolys(fma.general$ps,ylim=latr,xlim=lonr)
  statlabels = subset(calcCentroid(stats),!is.na(X) & !is.na(Y))
  statlabels$label = statarea.general$pd$label[match(statlabels$PID,statarea.general$pd$PID)]
}

Worker <- proto(
  label = "Worker"
)

Worker$report = function(.,attrs=NULL,to=""){
  cat("<p class='worker'>",.$label,"</p>",file=to)
  if(is.null(attrs)) attrs = .$ls()
  for(attr in attrs) {
    cat("<p class='attr'>.$",attr,"<pre>",file=to)
    sink(file=to)
    print(.[[attr]])
    sink(file=NULL)
    cat("</pre></p>",file=to)
  }
}