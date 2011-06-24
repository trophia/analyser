Distributioner <- Worker$proto(
  label = "Distributioner",
  data = NULL
)

Distributioner$new <- function(.,data){
  inst = .$proto()
  inst$data = data
  inst
}

Distributioner$meanVariancePlot <- function(.){
  do = function(vars,row,col){
    data = ddply(.$data,vars,function(sub)with(sub,data.frame(mean=mean(catch,na.rm=T),var=var(catch,na.rm=T),n=nrow(sub))))
    data = subset(data,n>3)
    coeffs = summary(lm(log(var)~log(mean),data))$coef
    p = ggplot(data,aes(x=log(mean),y=log(var)))+geom_point(aes(size=n),shape=1)+scale_area()+
      geom_abline(intercept=coeffs[1,1],slope=coeffs[2,1],colour='blue')+
      geom_text(aes(x=x,y=y,label=label),data=data.frame(x=min(log(data$mean)),y=max(log(data$var)),label=paste(round(coeffs[2,1:2],2),collapse=' +/- ')),size=3.5,hjust=0)+
      opts(axis.title.x=theme_text(size=10),axis.title.y=theme_text(size=10,angle=90),title=paste(vars,collapse='*'),plot.title=theme_text(size=10))+
      labs(size='')
    print(p,vp=subplot(row,col))
  }
  dev.new(width=16/2.54,height=16/2.54)
  vplayout(2,2)
  do(c('fyear'),1,1)
  do(c('fyear','month'),1,2)
  do(c('fyear','month','area'),2,1)
  do(c('fyear','month','area','vessel'),2,2)
  Figure(
    "Distributioner.meanVariancePlot",
    "Mean-variance plots. Lines indicate the unweighted fit to the points. Area of circles is proporiotal to the number of strata"
  ) 
  dev.off()
}
