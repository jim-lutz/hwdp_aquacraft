fn_script = "synchronize_events.R"
# synchronize events in Aquacraft hot and mains data set

# Jim Lutz  "Mon Mar 21 16:55:52 2016"

# make sure all packages loaded and start logging
source("setup.R")

# set the working directory names 
source("setup_wd.R")

# load useful functions (probably don't need this.)
source("functions.R")

# get DT_temp_events.Rdata
fn_DT_temp_events <- paste0(wd_data,"DT_temp_events.Rdata")
load(fn_DT_temp_events) 
str(DT_temp_events)
# Classes ‘data.table’ and 'data.frame':	86539 obs. of  20 variables:

names(DT_temp_events)
# [1] "Keycode"    "T_cold"     "T_hot"      "N_adults"   "N_teens"    "N_children" "N_infants"  "City"       "State"     
# [10] "SumAs"      "CountAs"    "StartTime"  "Duration"   "Peak"       "Volume"     "Mode"       "meterID"    "start.time"
# [19] "event.date" "drop"      

# look at just the dishwasher data
DT_DW_events <- DT_temp_events[SumAs=="Dishwasher", 
                               list(Keycode, meterID, SumAs, CountAs, start.time, event.date, Duration, Volume)]

#number of loads and number of draws by house
DT_loads <- DT_DW_events[,list(n.loads=length(Keycode[CountAs=="Dishwasher"]),
                           n.draws=length(Keycode[SumAs  =="Dishwasher"]))
                        ,by=Keycode]

# number of draws per load
DT_loads[,draw_load:=n.draws/n.loads][order(draw_load)]


qplot(data=DT_loads[order(draw_load)], x=draw_load)


# see about combining adjacent lines
# key by Keycode and meterID
setkeyv(DT_DW_events,c("Keycode","start.time"))
str(DT_DW_events)

# count meterIDs by Keycode
DT_hot_mains <- DT_DW_events[,list(n.hot  =length(Keycode[meterID=="hot"]   ),
                                   n.mains=length(Keycode[meterID=="mains"] ))
                             ,by=c("Keycode")]

# see which don't match
DT_hot_mains[, diff:= n.mains-n.hot][order(-diff)]
DT_hot_mains[abs(diff)>3,]

# keep only those keycodes where diff <= 3
DT_hot_mains <- DT_hot_mains[abs(diff)<=3,]
# 48

DT_hot_mains[][order(-diff)]

# look at one keycode that doesn't match
DT_DW_events[Keycode=="13S145"]
# can flag dubious records by hand then drop

# add record ID to DT_DW_events to make records easy to flag
DT_DW_events[, recordID:=seq.int(1,.N)]

DT_DW_events[Keycode=="13S145"]
# add 2294 to 2292, drop 2294

# add 2301 to 2299, drop 2301
DT_DW_events[recordID==2299]$start.time
# [1] "2013-01-23 17:03:58 America"
DT_DW_events[recordID==2299]$start.time+seconds(110)
# [1] "2013-01-23 17:05:48 America"
DT_DW_events[recordID==2301]$start.time
# [1] "2013-01-23 17:05:48 America"

2345 to 

# lag start.time Duration and Volume
DT_DW_events[, `:=` (start.time.hot = shift(start.time),
                     Duration.hot   = shift(Duration),
                     Volume.hot   = shift(Volume)), 
             by=Keycode]

DT_DW_events[Keycode=="12S598"]

# see how the events match up
with(DT_temp_events[],table(SumAs, CountAs, useNA = "ifany"))


DT_DW_events[Keycode=="12S112",][order(start.time)]


