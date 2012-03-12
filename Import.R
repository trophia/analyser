load(paste(TangaAnalyserHome,"NZFMA.Rdata",sep=''))
load(paste(TangaAnalyserHome,"NZbathy.Rdata",sep=''))

for(file in c(
  'Worker.R',
  'Loader.R',
  'Adder.R',
  'Deriver.R',
  'Subsetter.R',
  'Combiner.R',
  'Aggregater.R',
  'Corer.R',
  'Stepper.R',
  'Indexer.R',
  'Diagnoser.R',
  'Distributioner.R',
  'Influencer.R',

  'Reporting.R'
)){
  source(paste(TangaAnalyserHome,file,sep=''))
}
