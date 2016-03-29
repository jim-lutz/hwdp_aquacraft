# functions.R
# functions for synchronize hot & mains events 

rectangularize <- function(DT_AQ ) {
  # add fields to Aquacraft event data to make be able to make rectangles
  # input is a data.table with 
  # start.time (POSIXct), Duration (seconds), Volume (gallons)
  # adds end.time and flow 
  # returns the same data.table with modifications

  # make end.time
  DT_AQ[, end.time:= start.time + dseconds(Duration)]
  
  # calculate flow, assumes constant flow in GPM during event
  DT_AQ[ , flow:= Volume/(Duration/60)]
  
  return(DT_AQ)
}
logwarn('loaded rectangularize()')


chop.merge <- function(DT_A=DT_A, DT_B=DT_B) {
  # merges two data.tables of values by irregular timesteps with different starts and stops
  # each data.table has fields start.time, end.time, and flow for the same house.
  # times are POSIXct, each series is time ordered, no overlaps, doesn't have to be contiguous
  # divides time into bins which are the chopped union of all timesteps
  # returns a new data.table of t1,t2,flowA,flowB
  
  # get all edges to make bins
  edges <- c(DT_A$start.time, DT_A$end.time, DT_B$start.time, DT_B$end.time)
  
  # keep only unique edges
  edges <- unique(sort(edges))
  
  # number of bins  
  nbins = length(edges)-1
  
  # build empty data.table for bins
  DT_bins <- data.table(
    t1=edges[1:nbins], 
    t2=edges[2:(nbins+1)], 
    bin.n=1:nbins
  )
  
  setkey(DT_bins,t1)
  # str(DT_bins)
  
  # make a new data.table of DT_A
  DT_a <- DT_A[ , list(start.time, end.time, flowA_=flow, drawA_ = .I)]
  setkey(DT_a, start.time)
  # str(DT_a)

  # make a new data.table of DT_B
  DT_b <- DT_B[ , list(start.time, end.time, flowB_=flow, drawB_ = .I)]
  setkey(DT_b, start.time)
  # str(DT_b)
  
  # loop through all the bins in DT_bins, checking current draw in DT_A & DT_B for each bin
  for ( i in 1:nrow(DT_bins) ) {  # i is row counter DT_bins

    with(DT_bins[i,], {
  
      # find draw in DT_a the current bin is in
      if( nrow(DT_a[start.time<=t1 & t2<=end.time, ])>0 ){ # skip if not in a draw
        
        with(DT_a[start.time<=t1 & t2<=end.time, ], {
          DT_bins[i, drawA:=drawA_]
          DT_bins[i, flowA:=flowA_]
          })
        
      } # end if in draw in DT_a
      
      # find draw in DT_b the current bin is in
      if( nrow(DT_b[start.time<=t1 & t2<=end.time, ])>0 ){ # skip if not in a draw
        
        with(DT_b[start.time<=t1 & t2<=end.time, ], {
          DT_bins[i, drawB:=drawB_]
          DT_bins[i, flowB:=flowB_]
        })
        
      } # end of if in draw in DT_b
      
    }) # end of with DT_bins[i,]
    
  }

  return(DT_bins)
  
} # end of function
logwarn('loaded chop.merge()')


wrap.chop.merge <- function(k, .DT_DW_events=DT_DW_events) {
  # wrapper function for chop.merge()
  # runs chop.merge on one house in DT_DW_events
  # k is the keycode for the desired house.

  DT_bins <- chop.merge(.DT_DW_events[Keycode==k & meterID=="hot",],
                        .DT_DW_events[Keycode==k & meterID=="mains",])
  
  # change column names
  setnames(DT_bins, old = c("drawA",   "flowA",   "drawB",     "flowB"),
                    new = c("draw.hot","flow.hot","draw.mains","flow.mains")
           )
  
  # add Keycode
  DT_bins[, Keycode:=k]
  
  return(DT_bins)
             
}

