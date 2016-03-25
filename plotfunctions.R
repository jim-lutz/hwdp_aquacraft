#functions.R
# functions for CECHWT24 analysis

chop.merge <- function(DT_A=DT_A, DT_B=DT_B) {
  # merges two data.tables of values by irregular timesteps with different starts and stops
  # each data.table has fields time1, time2, and value in first 3 fields
  # times are POSIXct, each series is time ordered, no overlaps, doesn't have to be contiguous
  # divides time into bins which are the chopped union of all timesteps
  # returns a data.table of time1,time2,valueA,valueB
  
  # get all edges to make bins
  edges <- c(DT_A[[1]], DT_A[[2]], DT_B[[1]], DT_B[[2]])
  
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
  
  # allocate DT_A across bins
  k=1  # lowest bin to check
  # cat("start A\n", file="out.txt")
  
  for ( i in 1:nrow(DT_A) ) {  # i is row counter for A, fewer rows of A than bins
    
    # for ease of reading code
    A.t1 <- DT_A[i, ][[1]]
    A.t2 <- DT_A[i, ][[2]]
    A.V  <- DT_A[i, ][[3]]
    A.n  <- DT_A[i, ][[4]]
    
    for ( j in k:nrow(DT_bins) ) { # j is bin counter, check all the unexamined bins
      
      # cat("i:",i,"\tj:", j,"\tk:", k,"\n", file="out.txt", append=TRUE)
      
      # for ease of reading code
      bin.t1 <- DT_bins[j, ]$t1
      bin.t2 <- DT_bins[j, ]$t2
      bin.n  <- DT_bins[j, ]$bin.n
      
      
      # cat("\tA = \t",A.t1,"\t",A.t2,"\t#",A.n,"\n", file="out.txt", append=TRUE)
      # cat("\tbin = \t",bin.t1,"\t",bin.t2,"\t#",bin.n,"\n", file="out.txt", append=TRUE)
      
      if( bin.t2 <= A.t1 ) {
        # cat("\t\tbin is before A\n", file="out.txt", append=TRUE)
        next
      }
      if( A.t1 <= bin.t1 & bin.t2 <=  A.t2) {
        # cat("\t\tbin is in A\n", file="out.txt", append=TRUE)
        DT_bins[j, A:= DT_A[i, ][[3]] ] # assign this value of A to this bin
        next        
      }
      if( A.t2 <= bin.t1 ) {
        # cat("\t\tbin is after A\n", file="out.txt", append=TRUE)
        k=j  # don't bother looking at earlier bins for the next A
        break
      }
      
    } # end of j loop, bins
    
  } # end of i loop, A
  
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



date_format_tz <- function(format = "%Y-%m-%d", tz = "UTC") {
  # allows use of time zone in scale_x_datetime.
  # https://stackoverflow.com/questions/10339618/what-is-the-appropriate-timezone-argument-syntax-for-scale-datetime-in-ggplot
  function(x) format(x, format, tz=tz)
  
  #   then called as:
  #     
  #     scale_x_datetime(breaks = date_breaks("1 day"),
  #                      labels = date_format_tz("%d", tz="UTC"))
  
}
logwarn('loaded date_format_tz()')



fill.gaps <- function(DT_data) {
  # fills gaps in a data.table with interpolated data.
  # DT_data   = time series data.table, exclude any big gaps you don't want filled.
  #             uses 1 minute time step, date_time is time variable.
  #         
  # output
  # DT_data_filled    = original data.table with small gaps filled
  
  # make a date_time series of minutes
  dt_mins <- seq(min(DT_data$date_time),max(DT_data$date_time),"min")
  dt_mins[1]
  dt_mins[length(dt_mins)]
  
  # make a zoo object with NA for those times
  z_NA <- zoo(NA, order.by=dt_mins)
  
  #  DT_data to zoo, just the data to interpolate
  z_data <- zoo(DT_data[,list(OAT,Flow,hpwhW,Tin,Tintake,Tout)],order.by=DT_data$date_time)
  
  # merge the z_NA and z_data zoo objects
  z_data_gaps <- merge(z_data,z_NA)
  
  # now fill in gaps 
  z_data_fill <- na.approx(z_data_gaps[,1:6])
  
  # convert back to data.table
  #https://stackoverflow.com/questions/14064097/r-convert-between-zoo-object-and-data-frame-results-inconsistent-for-different
  DT_data_fill <- data.table(data.frame(date_time=time(z_data_fill),z_data_fill, row.names=NULL))
  
  return(DT_data_fill)
  
}
logwarn('loaded fill.gaps()')


plot_zoom <- function (s=siteID, DT_info=DT_site_info, DT_data=DT_siteID, center.time , zoom=10) {
  # function to plot power and water flow for one siteID 
  # center.time             - time for center of chart
  #                         - string ofYYYY-MM-DD hh:mm:ss 
  #                           or
  #                         - POSIXt  
  # zoom = 10               - number of minutes each side of center.time to show
  # uses plot_kWGPM()
  
  # get timezone of siteID
  tz=as.character(DT_info[siteID==s,]$tz)
  
  # get POSIXt times from center.time
  if(is.POSIXt(center.time)) {
    pstart = center.time - dminutes(zoom)
    pend = center.time + dminutes(zoom)
  } else {
    pstart = ymd_hms(center.time, tz=tz) - dminutes(zoom)
    pend   = ymd_hms(center.time, tz=tz) + dminutes(zoom)
  }
  pstart = strftime(pstart, "%F %T")
  pend = strftime(pend, "%F %T")
  
  # now plot data
  print(plot_kWGPM(s=siteID, DT_info=DT_site_info, DT_data=DT_siteID, t1=pstart, t2=pend))
  
  
  # display data
  DT_siteID[date_time>=ymd_hms(pstart,tz=tz) & date_time<=ymd_hms(pend,tz=tz),][]
  
}


plot_kWGPM <- function (s=siteID, DT_info=DT_site_info, DT_data=DT_siteID, t1, t2, save.charts=FALSE) {
  # function to plot power and water flow for one siteID
  # s = siteID              - character 
  # DT_info=DT_site_info    - information about sites
  # DT=DT_siteID            - merged data from monitoring at siteID
  # t1                      - string ofYYYY-MM-DD hh:mm:ss for start of chart
  # t2                      - string ofYYYY-MM-DD hh:mm:ss for end of chart
  # save.charts             - logical to save charts
  # these need to be global for this function to work
  # wd_charts = work directory for charts
    
  # str(DT_info)
  
  # get timezone of siteID
  tz=as.character(DT_info[siteID==s,]$tz)
  
  # get posix times from unit_date
  start = ymd_hms(t1, tz=tz)
  end   = ymd_hms(t2, tz=tz)
  
  # configure breaks and labels
  span = as.numeric(as.duration(new_interval(start, end)))/60 # minutes
  # breaks = date_breaks("2 hours"), labels = date_format("%H:%M")
  # looking for approx 8 - 12 breaks across span
  if(span>0)             {dbreaks = "1 min";         dlabels = "%H:%M" ; xlabel="time"}
  if(span>10)            {dbreaks = "2 mins";        dlabels = "%H:%M" }
  if(span>30)            {dbreaks = "5 mins";        dlabels = "%H:%M" }
  if(span>60)            {dbreaks = "20 mins";       dlabels = "%H:%M" }
  if(span>180)           {dbreaks = "30 mins";       dlabels = "%H:%M" }
  if(span>360)           {dbreaks = "60 mins";       dlabels = "%H:%M" }
  if(span>720)           {dbreaks = "2 hours";       dlabels = "%H:%M" }
  if(span>(24*60))       {dbreaks = "3 hours";       dlabels = "%H:%M" }    # 1 day
  if(span>(3*24*60))     {dbreaks = "12 hours";       dlabels = "%e %Hh"; xlabel="date" } # 3 days
  if(span>(7*24*60))     {dbreaks = "1 day";      dlabels = "%e"  } # 1 week
  if(span>(14*24*60))    {dbreaks = "1 day";         dlabels = "%b-%d" }    # 2 weeks
  if(span>(30*24*60))    {dbreaks = "3 days";        dlabels = "%b-%d" }    # 1 month
  if(span>(90*24*60))    {dbreaks = "1 week";        dlabels = "%b-%d" }    # 3 months
  if(span>(120*24*60))   {dbreaks = "2 weeks";       dlabels = "%b-%d" }    # 6 months
  if(span>(365*24*60))   {dbreaks = "2 months";      dlabels = "%b" }       # 1 year
  if(span>(2*365*24*60)) {dbreaks = "4 months";      dlabels = "%b %y" }    # 2 years
  
  # get the kW and GPM data for the desired times
  DT_subset_data <- DT_data[date_time>=start & date_time<=end, list(kW=hpwhW/1000,GPM=Flow),by="date_time"]  
  
  # make a data.table of a set of minutes with 0 as value.
  DT_set.of.minutes <- data.table(date_time=seq(from=start, to=end, by=dminutes(1) ), zero=0 )
  # str(DT_set.of.minutes )
  setkey(DT_set.of.minutes,date_time)
  
  # merge sample with day of minutes
  # to deal with missing records, if any
  DT_set.of.data <- merge(DT_set.of.minutes,DT_subset_data,all.x=TRUE)
  
  # str(DT_set.of.data)
  
  # turn NA GPM & kW to zero
  summary(DT_set.of.data$GPM)
  DT_set.of.data$GPM[is.na(DT_set.of.data$GPM)] <- 0
  summary(DT_set.of.data$GPM)
  
  summary(DT_set.of.data$kW)
  DT_set.of.data$kW[is.na(DT_set.of.data$kW)] <- 0
  summary(DT_set.of.data$kW)
  
  # set max and min for kW rectangles
  DT_set.of.data[,kW.max:=kW]
  DT_set.of.data[,kW.min:=0]
  
  # set max and min for GPM rectangles
  DT_set.of.data[,GPM.max:=GPM]
  DT_set.of.data[,GPM.min:=0]
  
  # set up overlap rectangles
  DT_set.of.data[,overlap.max:=pmin(kW.max,GPM.max)]
  DT_set.of.data[,overlap.min:=0]
  
  # handle GPM when overlap
  # GPM > overlap.max, reset GPM.min 
  DT_set.of.data[GPM > overlap.max, GPM.min:=overlap.max]
  
  # GPM <= overlap.max, reset GPM.max 
  DT_set.of.data[GPM <= overlap.max, GPM.max:=0]
  
  # handle kW when overlap
  # kW > overlap.max, reset kW.min 
  DT_set.of.data[kW > overlap.max, kW.min:=overlap.max]
  
  # kW <= overlap.max, reset kW.max 
  DT_set.of.data[kW <= overlap.max, kW.max:=0]
  

  # make blank plot
  p2 <- ggplot(data=DT_set.of.data ) 
  
  # set axis labels for hours
  p2 <- p2 + scale_x_datetime(limits = c(start, end), breaks = date_breaks(width=dbreaks), labels = date_format(dlabels))
  
  # set limits for y-scale
  #p2 <- p2 + scale_y_continuous(limits=c(0,5))
  p2 <- p2 + coord_cartesian(ylim = c(0.01, 5)) 
  
  # plot kW using pink rectangles
  p2 <- p2 + geom_rect(aes(xmin = date_time, xmax = date_time + dminutes(1), ymin = kW.min, ymax = kW.max), color="deeppink", fill="deeppink") 
  
  # plot GPM using blue rectangles
  p2 <- p2 + geom_rect(aes(xmin = date_time, xmax = date_time + dminutes(1), ymin = GPM.min, ymax = GPM.max), color="deepskyblue", fill="deepskyblue") 
  
  # plot overlap using purple rectangles
  p2 <- p2 + geom_rect(aes(xmin = date_time, xmax = date_time + dminutes(1), ymin = overlap.min, ymax = overlap.max), color="purple", fill="purple") 
  
  # labels
  p2 <- p2 + xlab(xlabel) + ylab("GPM(blue) / kW(pink)") + ggtitle(paste0("Water flow and power for Unit ",s))
  
  # titles and subtitles
  plot.title = paste0("Water flow and power for Unit ",s)
  size = DT_info[siteID==s,]$size
  brand = DT_info[siteID==s,]$brand
  pdate = strftime(start, "%F(%a)")
  plot.subtitle = paste0('date = ', pdate, ' HPWH: size = ',size, ', model = ', brand )
  p2 <- p2 + ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 
  
  p2
  
  if(save.charts) {
    # save to (giant) png file
    ggsave(p2,path=wd_charts,file=paste0(s,'_',pdate,".png"),width=10,height=7)
    # save to (giant) pdf file
    ggsave(p2,path=wd_charts,file=paste0(s,'_',pdate,".pdf"),width=20,height=14)
    # the PDF format shows the short interval draws.
  }
  
  return(p2)
}


get_hpwh_Wh <- function (fn, DT_site_info, DT_checking_pass) {
  # function to read _hpwhW.Rdata file and calculate Wh per TDV.hour of year
  # fn is full path to _hphwW.Rdata file
  # DT_site_info is data.table with information about research sites
  # DT_checking_pass is a data.table with dates that pass checking criteria
  
  # load the hpwhW data.tables 
  load(fn) # DT_1var
  
  names(DT_1var)
  # [1] "date_time"   "readTime"    "hpwhW_99094"
  
  # extract the siteID from 3rd variable name
  ID = str_extract(names(DT_1var)[3],"[0-9]{5}")
  
  # change 3rd variable name to hpwhW
  setnames(DT_1var,3,"hpwhW")
  
  # get time zone for that siteID
  tz <- as.character(DT_site_info[siteID==ID,]$tz)
  
  # add siteID to DT_1var
  DT_1var[,siteID:=ID]
  
  # str(DT_1var)
  # 922615
  
  # sort the data.table
  setkey(DT_1var,date_time)
  
  # get chr_date from date_time
  DT_1var[,chr_date:=strftime(date_time,format="%Y-%m-%d",tz=tz)]
  
  # keep data only from good days
  DT_hpwhW <- merge(DT_1var,DT_checking_pass[siteID==ID,list(siteID,chr_date)],by=c("siteID","chr_date"))
  # str(DT_hpwhW)
  # 796320 obs. of  5 variables:
  
  # add day of year from date_time
  DT_hpwhW[,doy:=as.numeric(strftime(date_time,format="%j",tz=tz))]
  
  # skip any leap days
  DT_hpwhW[ leap_year(date_time) & chr_date>"2012-02-28" ,doy:=doy-1] # this only works for 2012!
  
  # add hour of day
  DT_hpwhW[,hod:=as.numeric(strftime(date_time,format="%H",tz=tz))]
  
  # calculate TDV.hoy (hour of year based for TDV calculations)
  DT_hpwhW[,TDV.hoy:=(doy-1)*24+hod]
  
  # remove any records missing hpwhW
  DT_hpwhW <- DT_hpwhW[!(is.na(hpwhW)),]
  
  # sum hpwhH for chr_date and TDV.hoy
  DT_Wh <- DT_hpwhW[,list(siteID=unique(siteID),
                          Wm=sum(hpwhW,na.rm=TRUE), # missing values not included
                          nmins=length(hpwhW),
                          hod=unique(hod)
  ),by=list(chr_date,TDV.hoy)
  ] [,Wh:=Wm/nmins] #calculate watt-hours weighted by number of minutes
  
  # clean up and rearrange the order of the columns
  DT_Wh <- DT_Wh[,list(siteID,chr_date,hod,TDV.hoy,Wh,nmins)]
  
  # return the data.table
  return(DT_Wh)
}


find_gaps <- function (fn=fn) {
  # function to find time gaps that aren't 1 minute in the original Ecotope *.csv files
  # fn = fully qualified path to data
  # prints data.table of the strange gaps plus the last record
  
  # make the gaps file name
  fn_gaps <- str_extract(fn,'[A-Za-z]+_1minute_[A-Za-z]+.csv')
  fn_gaps <- str_replace(fn_gaps,'.csv','')
  
  # read file as data.table
  system.time(DT_csv <- data.table(read.csv(fn)))
  # user  system elapsed 
  # 30.840   0.372  31.283 
  
  
  
  # make date_time using UTC so don't have to worry about time zones
  date_time <- parse_date_time(DT_csv$readTime,"%d%b%Y %H:%M:%S",tz="UTC")
  DT_csv[,date_time:=date_time]
  
  # confirm timezones not used
  # DT_csv[1:10,list(date_time,readTime)]
  
  # add line numbers from original file to data.table
  numlines=nrow(DT_csv)
  DT_csv[, linenum:=seq_len(numlines)]
  
  # date_time from the previous record
  prev_date_time <- c(ymd('2000-01-01',tz='UTC'),DT_csv[c(1:(nrow(DT_csv)-1)),]$date_time)
  # prev_date_time[1:5]
  # with_tz(prev_date_time[1:5], tzone="UTC")
  
  prev_readTime <- c('0',as.character(DT_csv[c(1:(nrow(DT_csv)-1)),]$readTime))
  # prev_readTime[1:5]
  
  # add to DT_csv
  DT_csv[,prev_date_time:=with_tz(prev_date_time, tzone="UTC")]
  DT_csv[,prev_readTime:=prev_readTime]
  
  # calculate the time (duration) since the previous record
  DT_csv[,gap:=date_time-prev_date_time]
  
  # make the gaps data.table
  DT_gaps <- DT_csv[gap>=dminutes(1)|gap<=dminutes(-1),list(file=fn_gaps, date_time,prev_date_time,gap,readTime,prev_readTime,linenum)]
  
  # add the last row
  DT_gaps <- rbind(DT_gaps,DT_csv[nrow(DT_csv),list(file=fn_gaps, date_time,prev_date_time,gap,readTime,prev_readTime,linenum)])
  
  return(DT_gaps)
}


plot_cdf_hod <- function (ID=siteID, DT_info=DT_working) {
  # function to plot cumulative fraction of daily hot water by hour of day for one siteID, 
  # good dates only, colored by weekday/weekend. 
  # ID=siteID =             character string of site ID
  # DT_info=DT_working =    data.table of information about all sites
  
  # this needs to be global for this function to work
  # wd_charts = working directory for charts
  # wd_data   = working directory for data
  
  # get DT_siteID minute data
  # from XXXXX_merged.Rdata file for siteID
  fn_siteID_merged <- paste0(wd_data,ID,"_merged.Rdata")
  load(file=fn_siteID_merged) 
  
  # now modify the DT_siteID data
  
  # good days for that site from DT_info
  good_days <- DT_info[siteID==ID,]$fdate
  
  # keep only the good days in DT_siteID
  DT_siteID <- DT_siteID[fdate %in% good_days,]
  
  # add daily_vol as sum of Flow by day
  DT_siteID <- merge(DT_siteID,DT_siteID[,list(daily_vol=sum(Flow)),by=fdate],by="fdate",all=TRUE)
  
  # calculate cumulative fraction of daily volume
  DT_cdf_vol <- DT_siteID[,list(cdf_vol=cumsum(Flow)/daily_vol,
                                date_time=date_time)
                          ,by=fdate
                          ][,fdate:=NULL]
  
  # add DT_cdf_vol to DT_siteID
  DT_siteID <- merge(DT_siteID,DT_cdf_vol,by="date_time")
  
  # change NaN to 0
  DT_siteID[is.na(cdf_vol),cdf_vol:=0]
  
  # add hour of day on the first day of POSIXct, UTC
  DT_siteID[,hod:=ymd_hm(paste0("1970-01-01 ",hour(date_time),":",minute(date_time)))]
  
  # add wknd from DT_info to DT_siteID
  names(DT_siteID)
  names(DT_info)
  DT_siteID <- merge(DT_siteID,DT_info[siteID==ID,list(wknd=wknd),by=fdate],by="fdate")
  
  # str(DT_siteID)
  
  # is the time zone set right?
  tz(DT_siteID$date_time)
  # seems to be
  
  # make blank plot
  p2 <- ggplot(data=DT_siteID ) 
  
  # set axis labels for hours
  p2 <- p2 + scale_x_datetime(breaks = date_breaks("2 hours"), labels = date_format("%H:%M"))
  
  # plot cdf_vol using blue trace for weekends
  p2 <- p2 + geom_line(aes(x = hod, y = cdf_vol, group=fdate, color=wknd) ) 
  
  # labels
  p2 <- p2 + xlab("hour of day") + ylab("fraction of total daily hot water") 
  
  # titles and subtitles
  plot.title = paste0("Daily Hot Water for site ",ID)
  plot.subtitle = with(DT_info[siteID==ID], paste0("study:",study,"   city:",geo,"   occupants:",total_occ))
  p2 <- p2 + ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 
  
  # change title for fill legend
  p2 <- p2 + scale_color_discrete(name  ="day",
                                  breaks=c(FALSE, TRUE),
                                  labels=c("weekday", "weekend"))
  
  p2
  
  # save to (giant) png file
  ggsave(plot=p2,file=paste0(wd_charts,"/",ID,"_cdf_hod",".png"),width=20,height=14)
  # save to (giant) pdf file
  ggsave(plot=p2,file=paste0(wd_charts,"/",ID,"_cdf_hod",".pdf"),width=20,height=14)
  
  return(p2)
  
}


plot_sd_volkW <- function (ID=siteID, cdate=chr_date, DT_data=DT_siteID, DT_info=DT_site_info_plus) {
  # function to plot water flow and power for one siteID, one date
  # ID=siteID =                   character string of site ID
  # cdate=chr_date =              character string of date as YYYY-MM-DD
  # DT_data=DT_siteID =           data.table of _merged data for that siteID
  # DT_info=DT_site_info_plus =  data.table of information about all sites
  
  # this needs to be global for this function to work
  # wd_charts = work directory for charts
  
  # get date from cdate
  siteday <- ymd(cdate, tz=as.character(DT_info[siteID==ID,]$tz))
  # str(siteday)
  #  POSIXct[1:1], format: "2013-02-10"
  
  # make a data.table of all minutes in siteday with 0 as value.
  DT_day.of.minutes <- data.table(date_time=minutes(0:1439) + siteday, zero=0)
  # str(DT_day.of.minutes)
  # str(DT_data)
  
  # list of records selected by date
  l_records <- DT_data$date_time %in% DT_day.of.minutes$date_time
  
  # get the site data for that date
  DT_data_date <- DT_data[l_records,list(
                                          OAT=OAT,
                                          Flow=Flow,
                                          hpwhW=hpwhW,
                                          Tin=Tin,
                                          Tintake=Tintake,
                                          Tout=Tout),
                              by=date_time
                              ]
  
  # prepare to merge
  setkey(DT_day.of.minutes,date_time)
  setkey(DT_data_date,date_time)
  
  # merge sample with day of minutes
  DT_day.of.data <- merge(DT_day.of.minutes,DT_data_date,all.x=TRUE)
  # str(DT_day.of.data)
  
  
  #   # check out Flow & hpwhW 
  #   summary(DT_day.of.data$Flow)
  #   summary(DT_day.of.data$hpwhW)
  #   DT_day.of.data$hpwhW # What is 25.002 W evergy 3 or 4 minutes?
  #   
  #   # quick look at the distribution of Flow & hpwhW 
  #   qplot(Flow, data=DT_day.of.data, geom="histogram")
  #   qplot(hpwhW, data=DT_day.of.data, geom="histogram")
  #   
  #   # quick look at hpwhW < 1000
  #   qplot(hpwhW, data=DT_day.of.data[hpwhW<100,], geom="histogram")
  #   
  
  
  # make blank plot
  p2 <- ggplot(data=DT_day.of.data ) 
  
  # set axis labels for hours
  p2 <- p2 + scale_x_datetime(breaks = date_breaks("2 hours"), labels = date_format("%H:%M"))
  
  # set limits for y-scale, 
  p2 <- p2 + coord_cartesian(ylim = c(0, 6)) 
  
  # plot hpwhW using pink rectangles
  p2 <- p2 + geom_rect(aes(xmin = date_time, xmax = date_time + dminutes(1), ymin = 0, ymax = hpwhW/1000), color="deeppink") 
  
  # plot Flow using blue rectangles
  p2 <- p2 + geom_rect(aes(xmin = date_time, xmax = date_time + dminutes(1), ymin = 0, ymax = Flow), color="deepskyblue") 
  
  # labels
  p2 <- p2 + xlab("hour") + ylab("GPM(blue) / KW(pink)") 
  
  
  # titles and subtitles
  dowk=lubridate::wday(ymd(cdate),label=TRUE,abbr=TRUE)
  plot.title = paste0("Water flow and power for site ",ID," on ",dowk," ",cdate)
  plot.subtitle = with(DT_info[siteID==ID], paste0("study:",study,"   brand:",brand,"   size:",size))
  p2 <- p2 + ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 
  
  
  p2
  
  # save to (giant) png file
  ggsave(plot=p2,file=paste0(wd_charts,"/",ID,"_VkW_",cdate,".png"),width=20,height=14)
  # save to (giant) pdf file
  ggsave(plot=p2,file=paste0(wd_charts,"/",ID,"_VkW_",cdate,".pdf"),width=20,height=14)
  
  return(p2)
  
}


plot25_scatter_VQ <- function (DT=DT_checking, cntr="" ) {
  # function to generate a scatter plots of daily volume of hot water and daily kWh for 25 siteIDs 
  # from DT_checking data.table
  # for 25 siteIDs in a 5X5 grid
  
  # make a blank plot
  p <- ggplot(data=DT, aes(x=sumvol, y=kWh)) # kWh/day as function of sumvol/day  #, color=wknd
  
  # scatter plot of daily kWh by daily hot water volume
  p <- p + geom_point() # (position = "jitter")
  
  # title
  p <- p + ggtitle("daily kWh by daily hot water volume")
  
  # labels
  p <- p + xlab("gallons per day") + 
    ylab("kWh per day")
  
  # limits to x-axis
  # p <- p + coord_cartesian(xlim=c(0, 200))
  
  # 5X5 facet
  p <- p + facet_wrap( ~ siteIDfacettitle, ncol=5)
  
  # see what's there
  p
  
  # save chart as png & pdf files
  # save to (giant) png file
  ggsave(plot=p,file=paste0(wd_charts,"/E25_scatter_VQ_",cntr,".png"),width=20,height=14)
  # save to (giant) pdf file
  ggsave(plot=p,file=paste0(wd_charts,"/E25_scatter_VQ_",cntr,".pdf"),width=20,height=14)
  
  
  return(p)
  
}


plot25_daily_sumvol <- function (DT=DT_checking, cntr="" ) {
  # function to generate a summary look at daily sumvol from DT_checking data.table
  # for 25 siteIDs in a 5X5 grid
  
  # make a blank plot
  p <- ggplot(data=DT, aes(x=as.Date(fdate), y=sumvol)) # use as.Date for date_breaks
  
  # histogram of count of chr_date
  p <- p + geom_bar(stat="identity")
  
  # labels
  p <- p + xlab("date") + 
    ylab("gallons per day")
  
  # title
  p <- p + ggtitle("cumulative daily volume")
  
  # limits to y-axis
  p <- p + coord_cartesian(ylim=c(0, 200))
  
  # labels for X-axis
  p <- p + scale_x_date(labels = date_format("%b %y"), breaks = date_breaks("3 month"))
  
  # 5X5 facet
  p <- p + facet_wrap( ~ siteIDfacettitle, ncol=5)
  
  # see what's there
  p
  
  # save chart as png & pdf files
  # save to (giant) png file
  ggsave(plot=p,filename=paste0(wd_charts,"/E25_daily_sumvol_",cntr,".png"),width=20,height=15)
  # save to (giant) pdf file
  ggsave(plot=p,filename=paste0(wd_charts,"/E25_daily_sumvol_",cntr,".pdf"),width=20,height=15)
  
  
  return(p)
  
}


check_days_siteID <- function (fn_siteID_merged_data2=fn_siteID_merged_data, DT_site_info_tz2=DT_site_info_tz) {
  # function to check days from siteID
  # fn_siteID_merged_data2 = full path name to file with merged 1 minute data, 2 to avoid recursion
  # DT_site_info_tz2 = data.table which contains information about each siteID, including timezone, 2 to avoid recursion
  # makes and returns data.table DT_checking with summary data for each day
  # adds checking criteria fields , passes = TRUE
  
  # get siteID
  siteID <- str_extract(fn_siteID_merged_data2, "[0-9]{5}")
  
  # get the timezone for this siteID
  tz <- as.character(DT_site_info_tz2[siteid==siteID,]$tz)
  
  # load data.table from siteID_merged.Rdata file
  load(file=fn_siteID_merged_data2) 
  
  # add gap, minutes since last data record to DT_siteID
  # find lagged date_time (of the previous record)
  date_time_lag <- DT_siteID$date_time[1:length(DT_siteID$date_time)-1]
  date_time_lag <- c(DT_siteID$date_time[1]-minutes(1),date_time_lag) # assume record prior to the first one was a minute prior
  
  # add lagged date_time to DT_siteID
  DT_siteID <- DT_siteID[,date_time_lag:=date_time_lag]
  
  # calculate gap (time since previous record) in minutes
  DT_siteID[,gap:=as.numeric(difftime(date_time,date_time_lag,units="mins"))]
  
  # check gap values
  # DT_siteID[,list(n=length(OAT)), by=gap][order(gap,n),]
  # looks reasonable
  
  # add date as character, not factor
  DT_siteID[,chr_date:=as.character(fdate)]
  
  # now implement checking tests
  # completeness:
  #   missing data <= 30 minutes/day
  #   no continuous gap > 10 minutes/day
  #   drop time change days
  # data quality:
  #   Flow >=0  & < 20 GPM
  #   Tin, Tout > 0C & < 100C
  #   Tin_max < Tout_max each day
  #   hpwhW >= 0 & < 6kW
  
  
  # checking criteria by fdate, factor of date
  DT_checking <- DT_siteID[,list(
                                  siteID=siteID,
                                  chr_date=unique(chr_date),
                                  num_recs=length(OAT), # number of records
                                  sumgap=sum(gap)-length(gap==1), # the default gap is 1 minute, exclude these
                                  mingap=min(gap), # some gaps were negative?
                                  maxgap=max(gap),
                                  minflow=min(Flow),
                                  maxflow=max(Flow),
                                  sumvol=sum(Flow),
                                  minTin=min(Tin),
                                  maxTin=max(Tin),
                                  minTout=min(Tout),
                                  maxTout=max(Tout),
                                  minW=min(hpwhW),
                                  maxW=max(hpwhW),
                                  kWh=sum(hpwhW)/60/1000 # convert W by min to kWh 
                                ),
                                by=fdate
                           ]
  
  
  # summary look at daily sumvol
  # p <- plot_daily_sumvol(); p
  
  # test function test_days
  DT_checking <- test_days(DT_checking)
  
  return(DT_checking)
}


test_days <- function (DT_checking=DT_checking) {
  # function to add checking tests for days 
  # passes criteria = TRUE
  
  # check not a time change day
  DT_checking[,timechangeOK:=TRUE]
  DT_checking[chr_date %in% time_changes,timechangeOK:=FALSE]
  
  # enough data per day?
  DT_checking[,enoughOK:=(num_recs>=(1440-30)),]
  
  # gaps small enough?
  DT_checking[,gapOK:=(maxgap<=10),]
  
  # no negative gaps?
  DT_checking[,gapPOS:=(mingap>0),]
  
  # flow rates?
  DT_checking[,flowOK:=(minflow>=0 & maxflow<20)]
  
  # Tin?
  DT_checking[,TinOK:=(minTin>0 & maxTin<100)] # liquid water
  
  # Tout?
  DT_checking[,ToutOK:=(minTout>0 & maxTout<100)] # liquid water
  
  # power?
  DT_checking[,POK:=(minW>=0 & maxW<6000)] # reasonable power levels, (W)
  
  # water heater really heats water? no inverted temperatures with daily sumvol > 1
  DT_checking[,WH_OK:=((maxTout>maxTin) | (sumvol<=1)),] 
  
  # does it pass all criteria?
  DT_checking[,pass:=(timechangeOK & enoughOK & gapOK & gapPOS & flowOK & TinOK & ToutOK & POK & WH_OK )]
  
  return(DT_checking) 
  
}


plot_daily_sumvol <- function (DT=DT_checking, site=siteID) {
  # function to generate a summary look at daily sumvol from DT_checking data.table
  
  # make a blank plot
  p <- ggplot(data=DT, aes(x=ymd(fdate), y=sumvol)) 
  
  # histogram of chr_date
  p <- p + geom_bar(stat="identity")
  
  # labels
  p <- p + xlab("date") + 
    ylab("gallons per day") + 
    ggtitle(paste0("sumvol for ",site))
  
  # see what's there
  p
  
  return(p)
  
}


vars_1siteID <- function(siteID, DT_sv=DT_sv) {
  # function to indicate existence of variable by siteID
  # given siteID and
  # data.table of siteIDs and vars, DT_sv
  # "Thu Sep 18 09:14:57 2014"    changed order of vars so OAT is first.
  
  vs <- DT_sv[siteID,]$vars
  
  DT_vars_1siteID <- data.table( siteID = siteID,
                                 OAT     = "OAT"     %in% vs,
                                 Flow    = "Flow"    %in% vs,
                                 hpwhW   = "hpwhW"   %in% vs,
                                 Tin     = "Tin"     %in% vs,
                                 Tintake = "Tintake" %in% vs,
                                 Tout    = "Tout"    %in% vs
  )
  
  return(DT_vars_1siteID)
}


merge_1siteID <- function (siteID,DT_site_info_tz2=DT_site_info_tz) {
  # read and merge all the *.Rdata files for one siteID 
  # pass through DT_site_info_tz2 to get time zone for siteID 
  #   the 2 is to avoid recursion
  # save to siteID_merged.Rdata file
  # there's probably a more clever way to do this
  #  "Thu Sep 18 08:52:14 2014"   rearrange so read OAT first, then all of existing vars
  
  # read OAT
  DT_OAT <- load_1var(siteID,"OAT")
  
  # read Flow
  DT_Flow <- load_1var(siteID,"Flow")
  
  # read hpwhW
  DT_hpwhW <- load_1var(siteID,"hpwhW")
  
  # read Tin
  DT_Tin <- load_1var(siteID,"Tin")
  
  # read Tintake
  DT_Tintake <- load_1var(siteID,"Tintake")
  
  # read Tout
  DT_Tout <- load_1var(siteID,"Tout")

  # start building DT_siteID from OAT. OAT exists for every siteID
  DT_siteID <- DT_OAT
  
  # merge the other vars if they exist
  # merge Flow
  if(is.data.table(DT_Flow))     {DT_siteID <- merge(DT_siteID,DT_Flow,by=c("date_time","readTime"))}
  
  # merge hpwhW
  if(is.data.table(DT_hpwhW))   {DT_siteID <- merge(DT_siteID,DT_hpwhW,by=c("date_time","readTime"))}
  
  # merge Tin
  if(is.data.table(DT_Tin))     {DT_siteID <- merge(DT_siteID,DT_Tin,by=c("date_time","readTime"))}
  
  # merge Tintake
  if(is.data.table(DT_Tintake)) {DT_siteID <- merge(DT_siteID,DT_Tintake,by=c("date_time","readTime"))}
  
  # merge Tout  
  if(is.data.table(DT_Tout))    {DT_siteID <- merge(DT_siteID,DT_Tout,by=c("date_time","readTime"))}
    
  # use correct time zone
  tz <- as.character(DT_site_info_tz2[siteid==siteID,]$tz)
    
  # add fdate for date as factor
  DT_siteID[,fdate:=as.factor(strftime(date_time,format="%F",tz=tz))]
  
  # set key to date_time
  setkey(DT_siteID,date_time)
  
  # save to siteID_merged.Rdata
  fn_siteID_merged <- paste0(wd_data,siteID,"_merged.Rdata")
  save(DT_siteID, file=fn_siteID_merged)
  
  # to see what happened
  return(DT_siteID)
  
}


load_1var <- function (siteID, v) {
  # function to read one 1var file
  # given siteID and variable name v
  # "Thu Sep 18 09:30:22 2014"  change name of data field to v
  
  # make file name
  fn <- paste0(wd_data, siteID, "_", v, ".Rdata")
  
  # check if file exists
  if(!file.exists(fn)) {
    cat("\nwarning: ", fn," does not exist\n")
    return(0)
  }
    
    
  # load the DT_1var data.table
  load(file=fn)
  
  # change the name of the data (3rd) column to v
  setnames(DT_1var, 3, v)
  
  # remove rows with missing data in 3rd column, v
  DT_1var <- DT_1var[!is.na(DT_1var[[3]]),,]
  
  # set the key to date_time
  setkey(DT_1var,date_time)
  
  # return the data.table
  return(DT_1var)
  
}


read_1file <- function(fn, DT_site_info2=DT_site_info) {
  # function to read one Ecotope *.csv file and save every variable to an individual .Rdata file
  # fn = full path of file name
  # DT_site_info is data.table of siteID info, including time zone
  
  # read file as data.table
  DT_csv <- data.table(read.csv(fn))
  
  # now get the variable names, 
  vars <- names(DT_csv)
  # and drop readTime from list of variable names
  vars <- vars[2:length(vars)]
  
  # for testing save1var()
  var = vars[1]
  save_1var(var, DT_csv2=DT_csv, DT_site_info3=DT_site_info2 )
  
  # apply save_1var function repeatedly for every variable in the file
  l_ply(.data=vars, .fun=save_1var, DT_csv2=DT_csv, DT_site_info3=DT_site_info2, .progress="text")
}


save_1var <- function(var=var, DT_csv2=DT_csv, DT_site_info3=DT_site_info2 ) {
  # function to extract one variable name from data.table and save to a siteID_var.Rdata file
  # var = variable =name in data.table
  # DT_csv2 = data.table of data from one siteID, 2 is to avoid recursion problems
  # DT_site_info3 = data.table of siteID info, 3 is to avoid recursion problems
  
  # get measurement type and siteID from variable name
  meas <- str_extract(var,perl("([A-Za-z]+)"))
  siteID <- str_extract(var,perl("[0-9]{5}"))
  
  # if site is from Fluid mess with time
  if(siteID>"99300") {
    # convert as UTC, cause UTC doesn't have any time changes
    date_timeFluid <- parse_date_time(DT_csv2$readTime,"%d%b%Y %H:%M:%S",tz="UTC")
    
    # add 8 hours, so it's the actual time
    date_timeUTC <- date_timeFluid + hours(8)
    
    # what time is it in Pacific time zone
    date_timePacific <- with_tz(date_timeUTC, tzone="America/Los_Angeles")
    
    # make a data.table with just date_time, readTime and var
    DT_readTime_var <- DT_csv2[,c("readTime",var),with=FALSE]
    DT_1var <- data.table(date_time=date_timePacific, DT_readTime_var )
    
    }  else { 
      
    # not Fluid study
  
    # get site timezone
    tz <- DT_site_info3[siteid==siteID,]$tz
  
    # convert readTime to POSIXct date-time object named date_time
    date_time <- parse_date_time(DT_csv2$readTime,"%d%b%Y %H:%M:%S",tz=tz)
    
    # make a data.table with just date_time, readTime and var
    DT_readTime_var <- DT_csv2[,c("readTime",var),with=FALSE]
    DT_1var <- data.table(date_time=date_time, DT_readTime_var )
  
    }

  # make new file name from siteID and measure name
  fn_DT_1var = paste0(wd_data,siteID,"_",meas,".Rdata")
  save(DT_1var,file=fn_DT_1var)
  
}


plot_histvol <- function (unit,dtp=dt_pass) {
  # histogram of sumvol by day 
  # unit = 3
  # dtp=dt_pass is default data.table to use
  
  # check that unit exists in dt
  if(nrow(dtp[Unit==unit,])==0) { return(cat(paste0("error: Unit ", unit," does not exist"))) } else {
    
    
    # make a blank plot
    p <- ggplot(data=dtp[Unit==unit,], aes(x=ymd(chr_date), y=sumvol)) 
    
    # histogram of chr_date
    p <- p + geom_bar(stat="identity")
    
    # labels
    p <- p + xlab("date") + 
      ylab("gallons per day") + 
      ggtitle(paste0("sumvol for Unit ",unit))
    
    # see what's there
    p
    
  }
  
  return(p)
  
}


plot_sud <- function (sud, dtp=dt_Tt_min_data) {
  # function to plot water flow and power for one sample unit_date
  # sud = unit_date, string of UU_YYYY-MM-DD
  # these need to be global for this function to work
  # dtp = data.table of TIAX translated minute data
  # wd_charts = work directory for charts
  
  # get date from unit_date
  sday <- str_match(sud,perl("([0-9]{4}-[0-9]{2}-[0-9]{2})"))[1]
  sday <- ymd(sday, tz="America/Los_Angeles")
  
  # make a data.table of all minutes in sday with 0 as value.
  dt_day.of.minutes <- data.table(date_time=minutes(0:1440) + sday, zero=0)
  # str(dt_day.of.minutes)
  
  # check that unit_date exists
  if(nrow(dtp[unit_date==sud,])==0) { return(cat(paste0("error: ", sud," does not exist"))) } else {
  
  # get the sample data
  dt_sud_min_data <- dtp[unit_date==sud,list(
    Unit,
    P,
    Tin,
    Tout,
    vol
  ),
  by=date_time
  ]
  
  
  # prepare to merge
  setkey(dt_day.of.minutes,date_time)
  setkey(dt_sud_min_data,date_time)
  
  # merge sample with day of minutes
  dt_day.of.data <- merge(dt_day.of.minutes,dt_sud_min_data,all.x=TRUE)
  # str(dt_day.of.data)
  
  # turn NA vol & P to zero
  dt_day.of.data$vol[is.na(dt_day.of.data$vol)] <- 0
  summary(dt_day.of.data$vol)
  dt_day.of.data$P[is.na(dt_day.of.data$P)] <- 0
  summary(dt_day.of.data$P)
  
  # make blank plot
  p2 <- ggplot(data=dt_day.of.data ) 
  
  # set axis labels for hours
  p2 <- p2 + scale_x_datetime(breaks = date_breaks("2 hours"), labels = date_format("%H:%M"))
  
  # set limits for y-scale
  #p2 <- p2 + scale_y_continuous(limits=c(0,5))
  p2 <- p2 + coord_cartesian(ylim = c(0.01, 5)) 
  
  # plot P using pink rectangles
  p2 <- p2 + geom_rect(aes(xmin = date_time, xmax = date_time + dminutes(1), ymin = 0, ymax = P), color="deeppink") 
  
  # plot vol using blue rectangles
  p2 <- p2 + geom_rect(aes(xmin = date_time, xmax = date_time + dminutes(1), ymin = 0, ymax = vol), color="deepskyblue") 
  
  # labels
  p2 <- p2 + xlab("hour") + ylab("GPM(blue) / KW(pink)") + ggtitle(paste0("Water flow and power for Unit ",sud))
  
  
  p2
  
  # save to (giant) png file
  ggsave(p2,path=wd_charts,file=paste(sud,".png",sep=""),width=20,height=14)
  # save to (giant) pdf file
  ggsave(p2,path=wd_charts,file=paste(sud,".pdf",sep=""),width=20,height=14)
  # the PDF format shows the short interval draws.
  
  return(p2)
  
  } # end of else sud does exist
}


extract_cleaned_data <- function(fn){
  # function to extract data from a cleaned TIAX data file
  # and make unit_date field to merge with Tt_data
  
  # get Unit from fn
  Unit <- as.integer(str_match(string=fn,"Unit-(..*)_00")[1,2])
  
  # read the file as data.table
  dt_cleaned_data <- data.table(read.csv(file=fn)) 
  # example data
  #   date,time,vol(gallon),Tin(F),Tout(F)
  #   03/26/2002,0:34:00,0,118.4,136.8
  #   03/26/2002,0:35:00,0,120.7,137
  
  # convert date, time to POSIXct date-time
  date_time <- as.POSIXct(strptime(paste(dt_cleaned_data$date,dt_cleaned_data$time, sep=" "), format="%m/%d/%Y %H:%M:%S",tz="America/Los_Angeles"))
  # str(date_time) # to see that date_time is working

  
  # make unit_date field
  dt_cleaned_data[,unit_date:=paste0(sprintf("%02d",Unit),"_",strftime(date_time, format = "%F", tz = "America/Los_Angeles"))]
  
  
  # make a new data.table with just the desired fields
  # Unit, POSIXct date-time, unit_date, Tin, Tout, volume, fn
  dt_Tc_min_data <- dt_cleaned_data[,list(Unit=Unit,
                                         date_time=date_time,
                                         unit_date=unit_date,
                                         Tin=Tin.F.,
                                         Tout=Tout.F., # see ,2.0 Weekly Data Analysis
                                         # Zogg, Robert A., and Mwilliam J. Murphy. California Field-Test Data and Analysis. TIAX LLC for California Energy Commission, April 2004. http://www.energy.ca.gov/reports/500-04-018/2004-05-21_500-04-018_A2.PDF.
                                         vol=vol.gallon.,
                                         fn=fn
                                        )
                            ]
  
  return(dt_Tc_min_data)
  
  
  
  
}


extract_Date.Time <- function (fn) {
  # function to extract data from a translated TIAX 'Date.Time' format type data file 
  # fn is full path filename
  
  # get Unit from fn
  Unit <- as.integer(str_match(string=fn,"Unit(..)_200.")[1,2])
  
  # get year from fn
  yr <- str_extract(string=fn,perl("200."))
  
  # read the file as data.table
  dt_Date.Time <- data.table(read.csv(file=fn)) 
  
  # convert yr, Date/Time to POSIXct date-time
  # 10/13 7:30 AM
  date_time <- as.POSIXct(strptime(paste(yr,dt_Date.Time$Date.Time, sep=" "), format="%Y %m/%d %I:%M %p",tz="America/Los_Angeles"))
  # str(date_time)
  
  # add date_time to dt_Date.Time
  dt_Date.Time <- data.table(cbind(dt_Date.Time,date_time))
  
  # lagged date_time (of the previous record)
  date_time_lag <- dt_Date.Time$date_time[1:length(dt_Date.Time$date_time)-1]
  date_time_lag <- c(dt_Date.Time$date_time[1]-minutes(1),date_time_lag) # assume record prior to first was a minute prior
  
  # add lagged date_time to dt_Date.Time
  dt_Date.Time <- cbind(dt_Date.Time,date_time_lag)
  
  # calculate gap (time since previous record) in minutes
  gap <- as.numeric(difftime(date_time,date_time_lag,units="mins"))
  
  # add gap to dt_Date.Time
  dt_Date.Time <- cbind(dt_Date.Time,gap)
  
  # calculate energy (kWh)
  # assume was constant entire time since previous record
  dt_Date.Time[,Qin := HPWH.Power * as.integer(date_time - date_time_lag)/60 ] 
  
  # make unit_date field
  dt_Date.Time[,unit_date:=paste0(sprintf("%02d",Unit),"_",strftime(date_time, format = "%F", tz = "America/Los_Angeles"))]
  
  # make a new data.table with just the desired fields
  # Unit, POSIXct date-time, kWh, Tin, Tout, volume
  dt_mdata <- dt_Date.Time[,list(Unit=Unit,
                                 date_time=date_time,
                                 unit_date=unit_date,
                                 gap=gap,
                                 Qin=Qin, # in kWh
                                 P=HPWH.Power, # in kW
                                 Tin=Inlet.Water,
                                 Tout=HPWH.Upper.Tank, # see ,2.0 Weekly Data Analysis
                                 # Zogg, Robert A., and Mwilliam J. Murphy. California Field-Test Data and Analysis. TIAX LLC for California Energy Commission, April 2004. http://www.energy.ca.gov/reports/500-04-018/2004-05-21_500-04-018_A2.PDF.
                                 vol=Gallons.Delivered,
                                 fn=fn
                                )
                          ]
  
  return(dt_mdata)
}


extract_Unit <- function (fn) {
  # function to extract data from a translated TIAX 'Unit' format type data file 
  # fn is full path filename
  
  # get Unit from fn
  Unit <- as.integer(str_match(string=fn,"Unit(..)_200.")[1,2])
  
  # get year from fn
  yr <- str_extract(string=fn,perl("200."))
  
  # read the file as data.table
  dt_Unit <- data.table(read.csv(file=fn)) 
  
  # convert Minute to %H %M format. Not sure what happens with time changes
  H_M <- paste( dt_Unit$Minute %/% 60 , dt_Unit$Minute%%60, sep=" ")
  
  # convert yr, Date, Minute to POSIXct date-time
  date_time <- as.POSIXct(strptime(paste(yr,dt_Unit$Date, H_M, sep=" "), format="%Y %j %H %M",tz="America/Los_Angeles"))
  # str(date_time)
  
  # add date_time to dt_Unit
  dt_Unit <- data.table(cbind(dt_Unit,date_time))
  
  # lagged date_time (of the previous record)
  date_time_lag <- dt_Unit$date_time[1:length(dt_Unit$date_time)-1]
  date_time_lag <- c(dt_Unit$date_time[1]-minutes(1),date_time_lag) # assume record prior to first was a minute prior
  
  # add lagged date_time to dt_Unit
  dt_Unit <- cbind(dt_Unit,date_time_lag)
  
  # calculate gap (time since previous record) in minutes
  gap <- as.numeric(difftime(date_time,date_time_lag,units="mins"))
  
  # add gap to dt_Date.Time
  dt_Date.Time <- cbind(dt_Unit,gap)
  
  # calculate energy 
  # assume was constant entire time since previous record
  dt_Unit[,Qin := HPWH.Power * as.integer(date_time - date_time_lag)/60 ] 
  
  # make unit_date field
  dt_Unit[,unit_date:=paste0(sprintf("%02d",Unit),"_",strftime(date_time, format = "%F", tz = "America/Los_Angeles"))]
  
  # make a new data.table with just the desired fields
  # Unit, POSIXct date-time, kWh, Tin, Tout, volume
  dt_mdata <- dt_Unit[,list(Unit=Unit,
                            date_time=date_time,
                            unit_date=unit_date,
                            gap=gap,
                            Qin=Qin, # in kWh
                            P=HPWH.Power, # in kW
                            Tin=Inlet.Water,
                            Tout=HPWH.Upper.Tank, # see ,2.0 Weekly Data Analysis
                            # Zogg, Robert A., and Mwilliam J. Murphy. California Field-Test Data and Analysis. TIAX LLC for California Energy Commission, April 2004. http://www.energy.ca.gov/reports/500-04-018/2004-05-21_500-04-018_A2.PDF.
                            vol=Gallons.Delivered,
                            fn=fn              
                            )
                    ]
  
  return(dt_mdata)
}


get_names <- function (fn) {
  # function to get field names from the header row  in .csv file fn
  fnames <- t(names(read.csv(fn)))
  df_names <- data.frame(cbind(fn,fnames))
  # for use in ldply be sure function returns a data.frame!
  return(df_names)
}


dt_cross <- function (dt_A, dt_B) {
    # CJ in J {data.table} already does this.
    # function to "cross merge" data.tables.
    # where every row in dt_B is concatenated to every row in dt_A
    # final result is a data.table with n * m rows
    
    # number of rows in each data table
    m <- nrow(dt_A)
    n <- nrow(dt_B)
    
    # multiply each data.table by the number of rows in the other data table
    
    # start with 2 copies of dt_A 
    dt_A1 <- rbind(dt_A,dt_A)
    
    # now duplicate up to the nrows in dt_B
    for (i in 3:n) {
        dt_A1 <- rbind(dt_A1,dt_A)
    }
    
    # start with 2 copies of dt_B 
    dt_B1 <- rbind(dt_B,dt_B)
    
    # now duplicate up to the nrows in dt_A
    for (i in 3:m) {
        dt_B1 <- rbind(dt_B1,dt_B)
    }
    
    # add merge keys to each data.table
    dt_A1[,mrgky := seq(1:nrow(dt_A1))]
    dt_B1[,mrgky := seq(1:nrow(dt_B1))]
    
    # set keys
    setkey(dt_A1,mrgky)
    setkey(dt_B1,mrgky)
    
    # merge both data.tables
    dt_C1 <- merge(dt_A1, dt_B1)
    
    # return the cross merged data.table
    return(dt_C1)
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Multiple plot function
  # from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
  # accessed 2014-10-23
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}