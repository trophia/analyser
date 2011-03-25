library(proto)
library(plyr)
library(reshape)

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


Worker <- proto(
  label = "Worker"
)

#' Create a header for the report of this Worker
Worker$header = function(.,attrs=NULL,to=""){
  cat("<h1 class='worker'>",.$label,"</h1>",file=to)
  if(is.null(attrs)) attrs = .$ls()
  for(attr in attrs) {
    cat("<p class='attr'>.$",attr,"<pre>",file=to)
    sink(file=to)
    print(.[[attr]])
    sink(file=NULL)
    cat("</pre></p>",file=to)
  }
}

Worker$table = function(.,data,caption="",header=names(data),folder='.',to=""){
    cat("<p><b>Table: ",caption,"</b></p>\n<table>",sep='',file=to)
    cat(paste("<tr><th>",paste(header,collapse="</th><th>"),"</th></tr>\n"),file=to)
    #Format each column based on data type
    formatted = data.frame(Ignore=1:nrow(data))
    for(col in 1:ncol(data)){
      values = data[,col]
      formatted_values = format(values,digits=6,big.mark=",")
      formatted_values[is.na(values)] = '-'
      formatted = cbind(formatted,formatted_values)
    }
    formatted$Ignore <- NULL
    for(row in 1:nrow(formatted)) cat("<tr><td>",paste(format(formatted[row,]),collapse="</td><td>"),"</td></tr>\n",file=to) #Need to format again for unknown reason
    cat("</table><br>\n",file=to)
}

Worker$figure = function(.,name,caption,folder='.',to=""){
  #Create a PNG file for report
  filename = paste(folder,"/",name,".png",sep="")
  dimensions = par("din") * par("cra") / par("cin") 
  dev.print(png,filename=filename,width=dimensions[1],height=dimensions[2])
  cat("<img src='",filename,"'>\n",sep="",file=to)
  cat("<p><b>Figure :",caption,"</b></p>\n",file=to)
  #Also copy to PDF for better graphics
  dev.copy2pdf(file=paste(folder,"/",name,".pdf",sep=""))
}

Worker$view = function(.){
  temp = file('temp.html','w')
  .$report(temp)
  close(temp)
  system("firefox temp.html",wait=F)
}
