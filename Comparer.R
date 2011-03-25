Comparer <- Worker$proto(
  indices = NULL
)

Comparer$do <- function(.){
  #Todo. Normalise the indices so that where they overlap they all have a geometric mean of 1.
  .$merged = NULL
  for(index in names(.$indices)){
    indexDf = data.frame(fyear=.$indices[[index]]$fyear,index=.$indices[[index]]$index)
    names(indexDf) = c('fyear',index)
    .$merged = if(is.null(.$merged)) indexDf else merge(.$merged,indexDf,by='fyear',all=T)
  }
}

Comparer$report <- function(.,to=""){
  .$header(c('indices'),to=to)
  with(.$merged,{
    plot(NULL,NULL,xlim=range(fyear),ylim=c(0,max(.$merged[,2:ncol(.$merged)],na.rm=T)),ylab='CPUE Index',xlab='Fishing year')
    labels = vector()
    pchs = 1:10
    cols = rep(1,10)
    num = 1
    for(index in names(.$indices)) {
      lines(fyear,.$merged[,index],col=cols[num],pch=pchs[num],cex=1.5,type='o')
      labels = c(labels,.$indices[[index]]$label)
      num = num + 1
    }
    legend('bottom',labels,pch=pchs,col=cols,bty='n')
  })
}
