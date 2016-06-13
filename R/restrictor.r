#' Restricting data before putting into models
#'
#' Removes data points with extreme values for variables
#' and make things factors where appropriate
Restrictor <- function(data){

	data <- within(data,{
		fyear <- factor(fyear)
		
		# Create a fishing month factor
		month <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')[month]
		month <- factor(
		  month, 
		  levels = c('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep'),
		  ordered = T
		)
		
		area <- factor(area)
		area_month <- factor(area_month)
		
		vessel <- factor(vessel)
	})

  environment()
}
