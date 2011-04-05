report = NULL
reportTables = 0
reportFigures = 0
reportHeadings = c(0,0,0,0,0,0,0)

Html <- function(...) cat(...,file=report,sep='')
Title <- function(text) cat('<p class="title">',text,'</p>\n',file=report)

HeadingGen <- function(level,text) {
  reportHeadings[level] <<- reportHeadings[level] + 1
  reportHeadings[(level+1):7] <<- 0
  cat('<h',level,'>',paste(reportHeadings[1:level],collapse='.'),'. ',text,'</h',level,'>\n',sep='',file=report)
}
Heading1 <- function(text) HeadingGen(1,text)
Heading2 <- function(text) HeadingGen(2,text)
Heading3 <- function(text) HeadingGen(3,text)
Heading4 <- function(text) HeadingGen(4,text)
Heading5 <- function(text) HeadingGen(5,text)
Heading6 <- function(text) HeadingGen(6,text)
Paragraph <- function(...) {
  cat('<p>',file=report)
  cat(...,file=report)
  cat('</p>\n',file=report)
}

Table <- function(data,label,caption="",header=names(data)){
    reportTables <<- reportTables + 1

    cat("<div class='table'><p class='caption'><a id='",label,"'>Table ",reportTables,"</a>: ",caption,"</p>\n<table>",sep='',file=report)
    cat(paste("<tr><th>",paste(header,collapse="</th><th>"),"</th></tr>\n"),file=report)
    #Format each column based on data type
    formatted = data.frame(Ignore=1:nrow(data))
    for(col in 1:ncol(data)){
      values = data[,col]
      #Determine appropriate number of trailing digits
      #digits = nchar(format(round(max(values)),scientific=F))
      formatted_values = format(values,digits=4,big.mark=",")
      formatted_values[is.na(values)] = '-'
      formatted = cbind(formatted,formatted_values)
    }
    formatted$Ignore <- NULL
    for(row in 1:nrow(formatted)){
      Html("<tr>\n")
      for(col in 1:ncol(formatted)){
	just = if(col==1) 'left' else 'right'
	string = format(formatted[row,col]) #For some reason this needs another call to format!??##@@!
	string = gsub(",",", ",string)
	Html('<td class="',just,'">',string,"</td>\n")
      }
      Html("</tr>\n")
    }
    Html("</table></div>\n")
}

Figure <- function(label,caption){
  reportFigures <<- reportFigures + 1

  pngFilename = paste(sprintf('%03d',reportFigures),'.',label,".png",sep="")
  pdfFilename = paste(sprintf('%03d',reportFigures),'.',label,".pdf",sep="")
  #Create a PNG file for report
  pixels = par("din") * par("cra") / par("cin") 
  dev.print(png,filename=pngFilename,width=pixels[1],height=pixels[2])
  #Also copy to PDF for better graphics
  dev.copy2pdf(file=pdfFilename,pointsize=10)
  #Write
  dims = round(par("din")*2.54,1)
  cat("<div class='figure'><img src='",pngFilename,"' style='width:",dims[1],"cm; height:",dims[2],"cm'>\n","<p class='caption'><a id='",label,"'>Figure ",reportFigures,"</a>: ",caption,"</p></div>\n",sep='',file=report)
}

ReportStart <- function(title=NULL){
  report <<- file('report.html','w')
  reportTables <<- 0
  reportFigures <<- 0
  reportHeadings <<- c(0,0,0,0,0,0,0)

  Html('<html><head>')
  if(!is.null(title)) Html('<title>',title,'</title>')
  Html('
      <style type="text/css">
	body {
		font: 11pt "Times New Roman",Georgia,serif;
		text-align: justify; 
		width: 16cm;
	}
	h1,h2,h3,h4,h5,h6 {
	  margin-top: 2em; 
	  margin-bottom: 1em;
	}
	a {
	  text-decoration: none;
	}
	p.title, h1, h2, h3, h4 {
		font: 11pt Arial,sans-serif;
		font-weight:bold;
	}
	p.caption {
		font-weight:bold;
		text-align:left;
	}
	div.table, div.figure {
	  page-break-inside: avoid;
	}
	table {
		width:95%;
	}
	td {
		font-size: 10pt;
	}
	table .left {
		text-align:left;
	}
	table .right {
		text-align:right;
	}
	table td {
		border: none;
	}
      </style>
  ')
  Html('</head><body>')
}

ReportFinish <- function(){
  close(report)
  inp = paste(readLines(file('report.html')),collapse='')
  while(1){
      begin = regexpr('@',inp)
      if(begin>0){
	#Get label
	begin = begin + 1
	end = begin + regexpr('[^[:alnum:]_\\.]+',substr(inp,begin,begin+500)) - 2
	label = substr(inp,begin,end)
	#Find the next item with this label
	tagBegin = regexpr(paste("<a id='",label,"'>",sep=''),substr(inp,end,end+1e6))
	if(tagBegin==-1) stop(paste('Can not find label: "',label,'"'))
	tagBegin = end + tagBegin + attr(tagBegin,'match.length') - 1
	tagEnd = tagBegin + regexpr('</a>',substr(inp,tagBegin,tagBegin+500)) - 2
	tag = substr(inp,tagBegin,tagEnd)
	#Get number for this label
	#table = reportTables[[label]]
	#figure = reportFigures[[label]]
	#if(is.null(table) & is.null(figure)) stop(paste('Can not find label: "',label,'"'))
	#if(!is.null(table) & !is.null(figure)) stop(paste('Conflicting label: "',label,'"'))
	#num = max(table,figure)
	#Do replacement
	inp = gsub(paste('@',label,sep=''),paste("<a href='#",label,"'>",tag,"</a>",sep=''),inp)
      } else {
	break    
      }
  }
  cat(inp,file='reportLinked.html')

  #system('mogrify -density 200 -format png *.pdf')
  #system('wkhtmltopdf reportLinked.html report.pdf')
}



