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
  # each data.table has fields start.time, end.time, and flow for one house.
  # times are POSIXct, each series is time ordered, no overlaps, doesn't have to be contiguous
  # divides time into bins which are the chopped union of all timesteps
  # returns a data.table of start.time,end.time,flowA,flowB
  
  # get all edges to make bins
  edges <- c(DT_A$start.time, DT_A$end.time, DT_B$start.time, DT_B$end.time)
  
  # keep only unique edges
  edges <- unique(sort(edges))
  
  #   str(edges)
  #   str(with_tz(edges, tzone="UTC"))
  
  # number of bins  
  nbins = length(edges)-1
  
  # build empty data.table for bins
  DT_bins <- data.table(
    t1=with_tz(edges[1:nbins], tzone="UTC"),
    t2=with_tz(edges[2:(nbins+1)], tzone="UTC"),
    bin.n=1:nbins
  )
  
  DT_bins[1:10]
  setkey(DT_bins,t1)
  str(DT_bins)
  
  # make a new data.table of DT_A
  DT_a <- DT_A[ , list(start.time, end.time, flowA=flow, binA= .I)]
  setkey(DT_a, start.time)

  # make a new data.table of DT_B
  DT_b <- DT_B[ , list(start.time, end.time, flowB=flow, binB= .I)]
  setkey(DT_b, start.time)
  
  # set counters for DT_a & DT_b
  j.a=1  # first bin to check in DT_a
  j.b=1  # first bin to check in DT_b
  # cat("start A\n", file="out.txt")
  
  # loop through the bins in DT_bins, checking DT_A & DT_B for each step
  for ( i in 1:nrow(DT_bins) ) {  # i is row counter DT_bins
    i=1
    # check against bin i in DT_bins
    with(DT_bins[i,], {
      
      # check against bin j_a in DT_a
      with(DT_a[j.a,], {
        
        if( t2 <= start.time ) { # current bin is before bin in DT_a
          # don't do anything
        }
        
        if( t1 <= start.time & t2 <=  end.time) { # current bin is inside bin in DT_a
          DT_bins[i, flowA := flowA ] # assign current flowA to this bin in DT_bins
        }
        
        if( t2 <= end.time ) { # current bin is past the bin in DT_a
          j.a <- j.a + 1  # increment the bin in DT_a
        }
        
        ) # end of tests on bin with DT_a[j_a,]
      
      }) # end of tests on bin i with DT_bins[i,]
      
    } # end of loop (i in 1:nrow(DT_bins)

    
    
    
     
  # now allocate DT_B across bins
  k=1  # lowest bin to check
  # cat("start B\n", file="out.txt", append=TRUE)
  
  for ( i in 1:nrow(DT_B) ) {  # i is row counter for B, fewer rows of B than bins
    
    # for ease of reading code
    B.t1 <- DT_B[i, ][[1]]
    B.t2 <- DT_B[i, ][[2]]
    B.V  <- DT_B[i, ][[3]]
    B.n  <- DT_B[i, ][[4]]
    
    for ( j in k:nrow(DT_bins) ) { # j is bin counter, check all the unexamined bins
      
      # cat("i:",i,"\tj:", j,"\tk:", k,"\n", file="out.txt", append=TRUE)
      
      # for ease of reading code
      bin.t1 <- DT_bins[j, ]$t1
      bin.t2 <- DT_bins[j, ]$t2
      bin.n  <- DT_bins[j, ]$bin.n
      
      
      # cat("\tB = \t",B.t1,"\t",B.t2,"\t#",B.n,"\n", file="out.txt", append=TRUE)
      # cat("\tbin = \t",bin.t1,"\t",bin.t2,"\t#",bin.n,"\n", file="out.txt", append=TRUE)
      
      if( bin.t2 <= B.t1 ) {
        # cat("\t\tbin is before B\n", file="out.txt", append=TRUE)
        next
      }
      if( B.t1 <= bin.t1 & bin.t2 <=  B.t2) {
        # cat("\t\tbin is in B\n", file="out.txt", append=TRUE)
        DT_bins[j, B:= DT_B[i, ][[3]] ] # assign this value of B to this bin
        next        
      }
      if( B.t2 <= bin.t1 ) {
        # cat("\t\tbin is after B\n", file="out.txt", append=TRUE)
        k=j  # don't bother looking at earlier bins for the next B
        break
      }
      
    } # end of j loop, bins
    
  } # end of i loop, B
  
  return(DT_bins[,list(t1,t2,A,B)])
  
} # end of function
logwarn('loaded chop.merge()')

