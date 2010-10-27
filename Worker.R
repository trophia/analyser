library(proto)

Worker <- proto()

Worker$table = function(.,data,caption="",header=names(data),to=""){
      cat("<p><b>Table: ",caption,"</b></p>\n<table>",sep='',file=to)
      cat(paste("<tr><th>",paste(header,collapse="</th><th>"),"</th></tr>\n"),file=to)
      #Format each column based on data type
      formatted = data.frame(Ignore=1:nrow(data))
      for(col in 1:ncol(data)){
	      values = data[,col]
	      formatted_values = format(values,digits=3,big.mark=",")
	      formatted_values[is.na(values)] = '-'
	      formatted = cbind(formatted,formatted_values)
      }
      formatted$Ignore <- NULL
      for(row in 1:nrow(formatted)) cat("<tr><td>",paste(format(formatted[row,]),collapse="</td><td>"),"</td></tr>\n",file=to) #Need to format again for unknown reason
      cat("</table><br>\n",file=to)
}

Worker$figure = function(.,name,caption,to=""){
  #Create a PNG file for report
  filename = paste(name,".png",sep="")
  dimensions = par("din") * par("cra") / par("cin") 
  dev.print(png,filename=filename,width=dimensions[1],height=dimensions[2])
  cat("<img src='",filename,"'>\n",sep="",file=to)
  cat("<p><b>Figure :",caption,"</b></p>\n",file=to)
  #Also copy to PDF for better graphics
  dev.copy2pdf(file=paste(name,".pdf",sep=""))
}

Worker$view = function(.){
  temp = file('temp.html','w')
  .$report(temp)
  close(temp)
  system("firefox temp.html",wait=F)
}
