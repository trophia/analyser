report = NULL
reportTag = NULL
reportPrefix = ""
reportTables = 0
reportFigures = 0
reportHeadings = c(0,0,0,0,0,0,0)

Html <- function(...) cat(...,file=report,sep='')

HeadingGen <- function(level,text) {
  reportHeadings[level] <<- reportHeadings[level] + 1
  reportHeadings[(level+1):7] <<- 0
  cat('<h',level,'>',reportPrefix,paste(reportHeadings[1:level],collapse='.'),'. ',text,'</h',level,'>\n',sep='',file=report)
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

    cat("<div class='table'><p class='caption'><a id='",label,"'>Table ",reportPrefix,reportTables,"</a>: ",caption,"</p>\n<table>",sep='',file=report)
    #Output header
    cat("<tr>",paste("<th class='",c('left',rep('right',length(header)-1)),"'>",header,"</th>",sep='',collapse=''),"</tr>\n",file=report)
    #Format each column based on data type
    formatted = list()
    for(col in 1:ncol(data)){
      values = data[,col]
      #Determine appropriate number of trailing digits
      #digits = nchar(format(round(max(values)),scientific=F))
      formatted_values = format(values,digits=4,big.mark=",")
      formatted_values[is.na(values)] = '-'
      formatted[[col]] = formatted_values
    }
    #Output values
    for(row in 1:nrow(data)){
      Html("<tr>\n")
      for(col in 1:ncol(data)){
	string = format(formatted[[col]][row])
	string = gsub(",",",&nbsp;",string) #It is important to use nbsp so that the space is not breaking. Otherwise can get funky column widths that break a number after comma
	Html('<td class="',if(col==1) 'left' else 'right','">',string,"</td>\n")
      }
      Html("</tr>\n")
    }
    Html("</table></div>\n")
}

Figure <- function(label,caption){
  reportFigures <<- reportFigures + 1

  pngFilename = paste(sprintf('Fig%03d',reportFigures),'.',label,".png",sep="")
  pdfFilename = paste(sprintf('Fig%03d',reportFigures),'.',label,".pdf",sep="")
  #Create a PNG file for report
  pixels = par("din") * par("cra") / par("cin") 
  dev.print(png,filename=pngFilename,width=pixels[1],height=pixels[2])
  #Also copy to PDF for better graphics
  dev.copy2pdf(file=pdfFilename,pointsize=10)
  #Write
  dims = round(par("din")*2.54,1)
  cat("<div class='figure'><img src='",pngFilename,"' style='width:",dims[1],"cm; height:",dims[2],"cm'>\n","<p class='caption'><a id='",label,"'>Figure ",reportPrefix,reportFigures,"</a>: ",caption,"</p></div>\n",sep='',file=report)
}

ReportStart <- function(tag,prefix="",head=NULL){
    
  report <<- file(paste(tag,'.html',sep=''),'w')
  reportTag <<- tag
  reportPrefix <<- prefix
  reportTables <<- 0
  reportFigures <<- 0
  reportHeadings <<- c(0,0,0,0,0,0,0)

  Html('<html><head>')
  if(!is.null(head)) Html('<title>',head,'</title>')
  Html('
      <style type="text/css">
	body {
		font: 11pt "Times New Roman",Georgia,serif;
		text-align: justify; 
		width: 16cm;
	}

	div.cover {
	  page-break-after:always
	}
	div.cover p.warn1,
	div.cover p.warn2 {
	  font-weight: bold;
	  font-size: 12pt;
	  text-align: center;
	}
 	div.cover p.title,
	div.cover p.authors,
	div.cover p.date {
	  text-align: center;
	  font-size: 14pt;
	  margin-top: 3em;
	  margin-bottom: 3em;
	}
	div.cover p.title {
	  margin-top: 10em;
	}
	div.cover p.warn2 {
	  margin-top: 15em;
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
	th, td {
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

ReportWGCover = function(wg,title=NULL,authors=NULL,date=NULL){
  Html('<div class="cover">')
  Html('
  <p class="warn1">
       Draft report to the ',wg,'. 
       For working group discussions only. Not for release without written approval.
  </p>')
  if(!is.null(title)) Html('<p class="title">',title,'<p>')
  if(!is.null(authors))	Html('<p class="authors">',authors,'<p>')
  if(!is.null(date))	Html('<p class="date">',date,'<p>')
  Html('
  <p class="warn2">
    This draft report is not for publication or release in any other form,
    unless specifically authorised in writing by the authors and the Ministry of Primary Industries (MPI).
  </p>
  <p class="warn3">
    Working Group papers are works in progress whose role is to facilitate
    the discussion of the Working Groups. They often contain preliminary
    results that are receiving peer review for the first time and, as
    such, may contain errors or preliminary analyses that will be
    superseded by more rigorous work.  For these reasons, no one may
    release information contained in Working Group papers to external
    media. In general, Working Group papers should never be cited.
    Exceptions may be made in rare instances by obtaining permission in
    writing from the MPI Chief Scientist and the authors of the paper.
  </p>')
  Html('</div>')
}

ReportFinish <- function(){
  close(report)
  inp = paste(readLines(file(paste(reportTag,'.html',sep=''))),collapse='')
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
  cat(inp,file=paste(reportTag,'.link.html',sep=''))

  graphics.off()
  system('mogrify -density 200 -format png Fig*.pdf')
  
  system(paste('wkhtmltopdf -L 20 ',reportTag,'.link.html ',reportTag,'.pdf',sep=''))
}

File <- function(name,table){
	write.table(table,name,row.names=F,quote=F)
}


