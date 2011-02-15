import = function(file){
  ##Set the directory path below to specify where Tanga Analyser source code file are
  source(paste('/Trophia/Tanga/Analyser/tangaanalyser/',file,sep=''))
}

#Load source files
import('Worker.R')

#Load main data file
import('Loader.R')
loader = Loader$proto(
  ##The path of the main data file
  ##Must be defined
  file = '/Trophia/Tanga/Data/jdo1_mfish_jdo200903/cpue.txt'
)
dataLoad = loader$do()

#Add additional data files
import('Adder.R')
adder = Adder$proto(
  ##A vector of other file paths for files that provide data to be merged into the main data
  ##If not defined, no extra data added in.
  #files = c('extra.txt') 
)
dataAdd = adder$do(dataLoad)

#Derive other variables
import('Deriver.R')
deriver = Deriver$proto(
  ##A function that defines new fields based on existing fields. 
  ##At a minimum the field 'catch' must be defined, usually as XXX_prop or XXX_est 
  ##where XXX is the species code, but potenitally a combination of species
  func = expression({
    catch = JDO_prop
    #area = width*distance
  })
)
dataAll = deriver$do(dataAdd)

#Subset the data to the records of interest (and possibly also take a further random subsample)
import('Subsetter.R')
subsetter = Subsetter$proto(
  ##An expression defining which records to use e.g. expression(method=='BT' & target %in% c('TRE','SNA'))
  ##If not defined then all records used
  criteria = expression(
    fyear %in% 1990:2009
    & method=='BT' 
    & target %in% c('GUR','SNA','TRE','TAR','JDO','BAR') 
    & zone %in% c('042','043', '044', '045','046','047','048')
  )
  ##The size of a random sample to take after applying subsetting expression. Useful when doing testing to reduce run times. 
  ##If not defined, then no random sub-sampling.
  #,size = 1000
)
dataSub = subsetter$do(dataAll)

#Combine minor levels of factors into an 'Other' level
import('Combiner.R')
combiner = Combiner$proto(
  ##Threshold proportion. Levels of factors which have a proportion of records less than this are combined into 'Other'
  thresh = 0.01 
  ##A list of the levels to be retained for specific factors (all other levels are combined into 'Other').
  ##If defined for a field then overides thresh for that field.
  #,levels = list(form = c('CEL','TCE','TCP')) 
  ##Other fields to be converted into factors (all character fields are converted to characters automatically)
  ,others = c('fyear','month','vessel')
)
dataComb = combiner$do(dataSub)

#Aggregate data into strata for CPUE analysis
import('Aggregater.R')
aggregater = Aggregater$proto(
  ##Vectors of field names:
  ##	by : The fields to aggregate by. If not defined then no aggregation done. 
  ##          Usually c('trip','area','method','target') or c('vessel','date','area','method','target')
  ##	first: The fields for which the first value in each stratum will be used e.g factors not being aggregated by
  ##	sum: The fields for which the sum of values in each stratum will be used e.g catch and effort magnitude fields
  ##	mean: The fields for which the mean of values in each stratum will be used e.g. effort description fields
  ##If speed is an issue, reduce the number of fields in each list to the essentials.
  ##To use a field in the model terms (below it must be in one of theses lists.
  by = c('trip','area','method','target'),
  first = c('vessel','fyear','month','form','zone'),
  sum = c('catch','duration','num','num2','total','hooks','netlength','distance'),
  mean = c('depth','height','width','length','speed','temp','bottom','moon')
)
dataAgg = aggregater$do(dataComb)

#Reduce the data to that from a list of core vessels
import('Corer.R')
corer = Corer$proto(
  ##Define the minimum number of trips per year and number of qualifying years for a vessel to be considered 'core'.
  ##If not defined, then plot will be shown and you will be prompted for these variables. 
  ##You can then define these for subsequent runs.
  #trips = 10,
  #years = 3
)
dataCore = corer$do(dataAgg)

#Stepwise generalised linear model selection
import('Stepper.R')
logStepper = Stepper$proto(
  variable = expression(catch),
  family = gaussian(link='log'),
  terms = expression(fyear+month+zone+target+vessel+poly(log(num),3)+poly(log(duration),3))
)
logModel = logStepper$do(subset(dataCore,catch>0))

binStepper = Stepper$proto(
  variable = expression(catch>0),
  family = binomial(link='logit'),
  terms = expression(fyear+month+zone+target+vessel+poly(log(num),3)+poly(log(duration),3))
)
binModel = binStepper$do(dataCore)

#Diagnostics on final models
import('Diagnoser.R')
logDiagnoser = Diagnoser$proto(model = logModel)
binDiagnoser = Diagnoser$proto(model = binModel)

#influencer = Influencer$proto()
#influencer$do(logModel)

#Calculate CPUE indices in a variety of ways
import('Indexer.R')
indexer = Indexer$proto(
  data = list(
    all = dataAgg,
    core = dataCore
    ,north = subset(dataCore,area %in% c('041','042'))
  ),
  effort = 'num',
  lognormal = logModel
  #binomial = binModel
)
indexer$do()

#Report everything
import('Reporter.R')
reporter = Reporter$proto(
  #List of workers, created above, that will add content to the report
  workers=c(
    loader,adder,deriver,subsetter,combiner,aggregater,
    corer,
    logStepper,logDiagnoser,
    #binStepper,binDiagnoser,
    #influencer,
    indexer
  )
)
reporter$do()

