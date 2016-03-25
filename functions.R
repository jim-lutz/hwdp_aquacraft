# functions.R
# functions for synchronize hot & mains events 

rectangularize <- function(DT_AQ ) {
  # add fields to Aquacraft event data to make be able to make rectangles
  # input is a data.table with 
  # start.time (POSIXct), Duration (seconds), Volume (gallons)
  # adds start.time, end.time, flow
  # returns the same data.table with modifications

  # make end.time
  DT_AQ[, end.time:= start.time + dseconds(Duration)]
  
  return(DT_AQ)
}
