for(file in c(
  'Worker.R',
  'Loader.R',
  'Adder.R',
  'Deriver.R',
  'Subsetter.R',
  'Combiner.R',
  'Aggregater.R',
  'Corer.R',
  'Indexer.R',
  'Diagnoser.R',
  'influ.R'#At present, for convienience, influ is loaded as a source file (which itself is a simple copy of the influ package code) but eventually should be loaded as a package
  'Influencer.R'
)){
  source(paste(TangaAnalyserHome,file,sep=''))
}
