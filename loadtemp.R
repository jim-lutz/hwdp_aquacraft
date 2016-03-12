fn_script = "loadtemp.R"
# loads and cleans Aquacraft data to determine mixed use temperatures
# faucets, showers, clotheswashers, and baths

# Jim Lutz "Wed Mar  9 19:39:12 2016"
# "Thu Mar 10 04:25:17 2016"    check events with missing identifiers
# "Thu Mar 10 14:21:28 2016"    clean up Keycodes
# "Thu Mar 10 20:11:29 2016"    drop missing categories, email from Bill DeOreo, Thu, 10 Mar 2016 16:07:48 -0700
# "Fri Mar 11 08:25:40 2016"    rename to loadtemp.R, load, clean, summarize & store data
# "Fri Mar 11 12:39:55 2016"    check why more hots than mains
# "Fri Mar 11 16:26:36 2016"    drop REUWS2 hots w/ no mains

# make sure all packages loaded and start logging
source("setup.R")

# set the working directory names 
source("setup_wd.R")

# load useful functions (probably don't need this.)
source("functions.R")

# load Aquacraft data files
# path to directory
wd_Aquacraft  <- "/home/jiml/HotWaterResearch/projects/CECHWT24/hot water calcs/draw patterns/Aquacraft/" 

# get the mains events
fn_Mains_for_Hots <- paste0(wd_Aquacraft,"tblMains_for_Hots.h.csv")
DT_Mains_for_Hots <- fread(fn_Mains_for_Hots)
# Read 472022 rows and 9 (of 9) columns from 0.037 GB file in 00:00:03

str(DT_Mains_for_Hots)
# Classes ‘data.table’ and 'data.frame':	472022 obs. of  9 variables:
# $ Keycode  : chr  "12S1003" "12S1003" "12S1003" "12S1003" ...
# $ SumAs    : chr  "Clotheswasher" "Clotheswasher" "Clotheswasher" "Clotheswasher" ...
# $ CountAs  : chr  "" "" "" "" ...
# $ StartTime: chr  "Sun Oct 28 11:03:14 PDT 2012" "Sun Oct 28 11:07:34 PDT 2012" "Sun Oct 28 11:11:24 PDT 2012" "Sun Oct 28 11:13:54 PDT 2012" ...
# $ Duration : chr  "50.0" "40.0" "150.0" "150.0" ...
# $ Peak     : chr  "4.27" "4.27" "5.62" "7.36" ...
# $ Volume   : chr  "2.98" "2.123" "5.776" "13.141" ...
# $ Mode     : chr  "4.22" "4.27" "2.88" "6.06" ...
# $ ID       : chr  "1" "2" "3" "4" ...
# - attr(*, ".internal.selfref")=<externalptr> 

# add meterID to identify that this came from the mains meter
DT_Mains_for_Hots[,meterID:="mains"]

# drop ID
DT_Mains_for_Hots[,ID:=NULL]

# get the hot events 
fn_HotTraces_REUWS2 <- paste0(wd_Aquacraft,"tblHotTraces_REUWS2.h.csv")
DT_HotTraces_REUWS2 <- fread(fn_HotTraces_REUWS2)

str(DT_HotTraces_REUWS2)
# Classes ‘data.table’ and 'data.frame':	144242 obs. of  8 variables:
# $ Keycode  : chr  "12S112" "12S112" "12S112" "12S112" ...
# $ SumAs    : chr  "Leak" "Faucet" "Leak" "Faucet" ...
# $ CountAs  : chr  "Leak" "Faucet" "Leak" "Faucet" ...
# $ StartTime: chr  "Tue Feb 21 05:18:15 PST 2012" "Tue Feb 21 05:18:25 PST 2012" "Tue Feb 21 05:18:35 PST 2012" "Tue Feb 21 05:19:55 PST 2012" ...
# $ Duration : chr  "10.0" "10.0" "80.0" "40.0" ...
# $ Peak     : chr  "0.06" "1.31" "0.06" "2.28" ...
# $ Volume   : chr  "0.01" "0.218" "0.038" "0.933" ...
# $ Mode     : chr  "0.06" "1.31" "0.02" "2.28" ...
# - attr(*, ".internal.selfref")=<externalptr> 
  
# add meterID to identify that this came from the meter at the water heater
DT_HotTraces_REUWS2[,meterID:="hot"]

# concatenate the data.tables
DT_REUWS2_hots <- rbind(DT_Mains_for_Hots,DT_HotTraces_REUWS2)

# remove original large data.tables
rm(DT_Mains_for_Hots,DT_HotTraces_REUWS2)

names(DT_REUWS2_hots)
#[1] "Keycode"   "SumAs"     "CountAs"   "StartTime" "Duration"  "Peak"      "Volume"    "Mode"      "meterID"  

# change fields to match type in
# /home/jiml/HotWaterResearch/projects/CECHWT24/hot water calcs/draw patterns/Aquacraft/tblMains_for_Hots.csv.ftype
# TEXT,TEXT,TEXT,SHORT_DATE_TIME,DOUBLE,DOUBLE,DOUBLE,DOUBLE,LONG

# change Duration, Peak, Volume, Mode to numbers
DT_REUWS2_hots[, Duration := as.numeric(Duration)]
DT_REUWS2_hots[, Peak := as.numeric(Peak)]
DT_REUWS2_hots[, Volume := as.numeric(Volume)]
DT_REUWS2_hots[, Mode := as.numeric(Mode)]

# get timezone in StartTime
DT_REUWS2_hots[,StartTime.tz:= str_sub(StartTime, start=-8, end = -6)]

unique(DT_REUWS2_hots$StartTime.tz)
# [1] "PDT" "PST"

# so don't need to worry about time zones
DT_REUWS2_hots[,StartTime.tz:= NULL]

# change StartTime to a POSIXct date-time object
DT_REUWS2_hots[,start.time := parse_date_time(StartTime,"%a %b %d %H:%M:%S  %Y", tz="America/Los Angeles")]

# upper case Keycode
DT_REUWS2_hots[, Keycode := toupper(Keycode)]

str(DT_REUWS2_hots)

# compare by meterID
DT_REUWS2_hots[,list(nhouses=length(unique(Keycode))),by=meterID]
#    meterID nhouses
# 1:   mains      97
# 2:     hot     103

# look at Keycode
sort(unique(DT_REUWS2_hots$Keycode))

# see how the events match up
with(DT_REUWS2_hots,table(SumAs, CountAs, useNA = "ifany"))
#                CountAs
# SumAs                  Bathtub Clotheswasher Dishwasher Faucet Humidifier Irrigation   Leak  Other   Pool Shower Toilet
#                 289728       0             0          0      0          0          0      0      0      0      0      0
#   Bathtub            0     479             0          0      0          0          0      0      0      0      0      0
#   Clotheswasher   5428       0          2096          0      0          0          0      0      0      0      0      0
#   Dishwasher      3219       0             0       1230      0          0          0      0      0      0      0      0
#   Faucet             0       0             0          0 116374          0          0      0      0      0      0      0
#   Humidifier         0       0             0          0      0        390          0      0      0      0      0      0
#   Irrigation         0       0             0          0      0          0        284      0      0      0      0      0
#   Leak               0       0             0          0      0          0          0 169824      0      0      0      0
#   Other              0       0             0          0      0          0          0      0   6711      0      0      0
#   Pool               0       0             0          0      0          0          0      0      0      6      0      0
#   Shower             0       0             0          0      0          0          0      0      0      0   4426      0
#   Toilet             0       0             0          0      0          0          0      0      0      0      0  16069

# why so many blanks?
DT_REUWS2_hots[SumAs==''& CountAs=='', list(n=length(start.time)), by=Keycode][order(n)]
# no obvious pattern

# drop when both categories are missing
# "there are many events in the two databases that have no category assigned to them.  This should be ignored."
# email Bill DeOreo, 03/10/2016 03:07 PM
DT_events <- DT_REUWS2_hots[!(SumAs==''& CountAs==''), ]
rm(DT_REUWS2_hots)

sort(unique(DT_events$Keycode))
# 103 Keycodes

# confirm same Keycodes hot & mains
DT_events[,list(N_Keycodes = length(unique(Keycode))),by=meterID]
#    meterID N_Keycodes
# 1:   mains         97
# 2:     hot        103

Keycode.hot <- sort(unique(DT_events[meterID=="hot",]$Keycode)) 
Keycode.mains <- sort(unique(DT_events[meterID=="mains",]$Keycode))

l_mains.missing <- setdiff(Keycode.hot, Keycode.mains)
# [1] "12S1319" "12S233"  "12S606"  "12S858"  "13S192"  "13S214"  <- these Keycodes in hot, but not mains events databases
setdiff(Keycode.mains, Keycode.hot)
# character(0)

# drop missing Keycode
DT_events <- DT_events[!(Keycode %in% l_mains.missing),]



# get temperatures by house from Audit forms
fn_Combined_Hot_Water_Audit_Forms <- paste0(wd_Aquacraft,"Combined Hot Water Audit Forms.csv")
DT_Combined_Hot_Water_Audit_Forms <- fread(fn_Combined_Hot_Water_Audit_Forms)

# Keep Keycode, location, demographics, & temps
names(DT_Combined_Hot_Water_Audit_Forms)
DT_temps <- DT_Combined_Hot_Water_Audit_Forms[,c(1, 3, 4, 33, 34, 35, 36, 43, 44), with=FALSE]

# shorten names
names(DT_temps)
setnames(DT_temps, 4:9, c("N_adults", "N_teens", "N_children", "N_infants", "T_cold","T_hot"))
# [1] "Keycode"    "City"       "State"      "N_adults"   "N_teens"    "N_children" "N_infants"  "T_cold"     "T_hot"     

# make sure Keycodes upper case
DT_temps[,Keycode:=toupper(Keycode)]

# drop trailing H and H2
DT_temps[, Keycode2:=str_match(Keycode,"[0-9S]*")]

DT_temps[, list(unique(Keycode2)), by=Keycode]

# drop Keycode 12STBD
DT_temps <- DT_temps[Keycode!='12STBD',]
DT_temps[, Keycode:=Keycode2]
DT_temps[, Keycode2:=NULL]

sort(unique(DT_temps$Keycode))

# convert N_adults field to numbers
DT_temps[,N_adults:=as.numeric(str_extract(N_adults,"[1-9]") )] # missing is NA

# if N_adults is NA so are others
DT_temps[is.na(N_adults), `:=` (N_teens=NA, N_children=NA, N_infants=NA)]

# convert blanks to zeros
DT_temps[N_teens=='',N_teens:='0']
DT_temps[N_children=='',N_children:='0']
DT_temps[N_infants=='',N_infants:='0']

# convert to numeric
DT_temps[, c("N_teens", "N_children", "N_infants") := 
           list(as.numeric(N_teens), as.numeric(N_children), as.numeric(N_infants) )]

str(DT_temps)

# get number out of temperature field
DT_temps[,T_cold2:= str_extract(T_cold,"[0-9.]+")]
DT_temps[!is.na(T_cold2),list(T_cold,T_cold2)]
DT_temps[,T_hot2:= str_extract(T_hot,"[0-9.]+")]
DT_temps[!is.na(T_hot2),list(T_hot,T_hot2)]

# set temps with ~ to NA
DT_temps[str_detect(T_cold,"~"),T_cold2:=NA]
DT_temps[str_detect(T_hot,"~"),T_hot2:=NA]

# keep only when both temperatures were measured
DT_temps <- DT_temps[!is.na(T_cold2) & !is.na(T_hot2), list(Keycode,
                                                            T_cold=as.numeric(T_cold2), T_hot=as.numeric(T_hot2),
                                                            N_adults, N_teens, N_children, N_infants,
                                                            City, State) 
                     ]

str(DT_temps)
# Classes ‘data.table’ and 'data.frame':	81 obs. of  9 variables:
str(DT_events)
#Classes ‘data.table’ and 'data.frame':	326536 obs. of  10 variables:

# merge DT_temps & DT_events, only Keycode, keep all records to see which is missing from which
DT_Keycodes <- merge(DT_temps[,list(Keycode, temps=TRUE)],DT_events[,list(Keycode, events=TRUE)], by="Keycode", all=TRUE)

# summarize
DT_Keycodes[, list(N.Keycodes=length(unique(Keycode))),by=c("temps","events")]
#    temps events N.Keycodes
# 1:    NA   TRUE         26
# 2:  TRUE   TRUE         71
# 3:  TRUE     NA         10

# merge DT_temps & DT_events, keep only if in DT_temps
DT_temp_events <- merge(DT_temps, DT_events, by = "Keycode")

# check to make sure temps and events are both covered
DT_temp_events[is.na(T_cold), ]
DT_temp_events[is.na(meterID), ]
# Empty data.table (0 rows) of 18 cols: Keycode,T_cold,T_hot,N_adults,N_teens,N_children...

str(DT_temp_events)
# Classes ‘data.table’ and 'data.frame':	246806 obs. of  18 variables:

length(unique(DT_temp_events$Keycode))
# [1] 71

# save the DT_temp_events data.table for later processing
save(DT_temp_events,file=paste0(wd_data,"DT_temp_events.Rdata"))

# make summary table of draws and loads by type and meterID
# The Sum-as field identifies every cycle of every use.  
# The Count-as field if for counting the loads for dishwashers and clothes washers.
# email from Bill DeOreo, 03/09/2016 08:01 AM

DT_Ndraws.hot   <- DT_temp_events[meterID=="hot", 
                          list(Ndraws.hot=length(Keycode)),by=SumAs][,type:=SumAs][,SumAs:=NULL][order(type)]
DT_Nloads.hot   <- DT_temp_events[meterID=="hot", 
                          list(Nloads.hot=length(Keycode)),by=CountAs][,type:=CountAs][,CountAs:=NULL][order(type)]
DT_Ndraws.mains <- DT_temp_events[meterID=="mains", 
                          list(Ndraws.mains=length(Keycode)),by=SumAs][,type:=SumAs][,SumAs:=NULL][order(type)]
DT_Nloads.mains <- DT_temp_events[meterID=="mains", 
                          list(Nloads.mains=length(Keycode)),by=CountAs][,type:=CountAs][,CountAs:=NULL][order(type)]

# set keys
for (d in grep("^DT_N",ls(),value=TRUE)) {
  setkeyv(x=get(d),cols="type")
  str(DT_Ndraws.mains)
}

# make summary table
DT_event.summary <- merge(DT_Ndraws.mains, DT_Ndraws.hot)
DT_event.summary <- merge(DT_event.summary,DT_Nloads.mains)
DT_event.summary <- merge(DT_event.summary,DT_Nloads.hot)

DT_event.summary

# save event summary table
write.csv(DT_event.summary, file=paste0(wd_data,"event.summary.csv"),row.names = FALSE)
#             type Ndraws.mains Ndraws.hot Nloads.mains Nloads.hot
# 1:       Bathtub          127        135          127        135
# 2: Clotheswasher         3451       1307          939        573
# 3:    Dishwasher         1471       1502          452        460
# 4:        Faucet        41251      39753        41251      39753
# 5:          Leak        76199      60556        76199      60556
# 6:         Other         4381       1410         4381       1410
# 7:        Shower         1407       1510         1407       1510
# 8:        Toilet        11688          1        11688          1

# more hot events than mains events, Bathtub, Dishwasher, Shower?

# find earliest and latest dates
DT_end.hot <- DT_temp_events[meterID=="hot", list(earliest.hot=min(start.time), latest.hot=max(start.time)), by=Keycode]
DT_end.mains <- DT_temp_events[meterID=="mains", list(earliest.mains=min(start.time), latest.mains=max(start.time)), by=Keycode]

DT_ends <- merge(DT_end.hot, DT_end.mains)

DT_ends[, earliest.diff:=(earliest.mains-earliest.hot)][order(-earliest.diff)]

