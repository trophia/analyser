# An example of how to chain the modules together

loader <- Loader('cpue.txt', {
  catch <- BNS_prop
  effort <- num
})
loader$fyear_summary()

subsetter <- Subsetter(loader$data,
  method=='BLL' & fyear %in% 2010:2015
)
subsetter$fyear_summary()

aggregater <- Aggregater(subsetter$data, 
  by = c('vessel','date','area','target')
)
aggregater$fyear_summary()

corer <- Corer(aggregater$data,
  trips_min = 5,
  years_min= 5
)
corer$fyear_summary()
corer$criteria_plot()
