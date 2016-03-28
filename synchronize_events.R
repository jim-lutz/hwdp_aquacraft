fn_script = "synchronize_events.R"
# synchronize events in Aquacraft hot and mains data set

# Jim Lutz  "Mon Mar 21 16:55:52 2016"
# "Fri Mar 25 08:26:17 2016"    repurpose earlier plotting functions for hot vs mains 
# "Mon Mar 28 10:41:31 2016"    still working on plotting functions

# make sure all packages loaded and start logging
source("setup.R")

# set the working directory names 
source("setup_wd.R")

# load useful functions 
source("functions.R") 
# source("plotfunctions.R") # add as needed

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
# 1 up to 11

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

# try plotting hot & mains draws as red & blue
# make each event a rectangle
rectangularize(DT_DW_events)
str(DT_DW_events)
DT_DW_events

# chop.merge() on test house
DT_bins <- chop.merge(DT_DW_events[Keycode=="13S145" & meterID=="hot",],
                      DT_DW_events[Keycode=="13S145" & meterID=="mains",])

tables()
DT_bins



