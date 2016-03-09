fn_script = "load_CA.R"
# first attempt at loading and analyzing Aquacraft data

# Jim Lutz "Mon Mar  7 18:19:03 2016"
# "Tue Mar  8 14:59:54 2016"    redid ../Aquacraft/*.sh & *.pl to exclude field types from headers

# make sure all packages loaded and start logging
source("setup.R")

# set the working directory names 
source("setup_wd.R")

# load useful functions (probably don't need this.)
source("functions.R")

# load Aquacraft data files
# path to directory
wd_Aquacraft  <- "/home/jiml/HotWaterResearch/projects/CECHWT24/hot water calcs/draw patterns/Aquacraft/"  
fn_AllCalEvents <- paste0(wd_Aquacraft,"AllCalEvents_2011.h.csv")
DT_AllCalEvents <- fread(fn_AllCalEvents)
# Read 2168699 rows and 9 (of 9) columns from 0.171 GB file in 00:00:11

str(DT_AllCalEvents)


names(DT_AllCalEvents)
# [1] "Keycode"   "SumAs"     "CountAs"   "StartTime" "Duration"  "Peak"      "Volume"    "Mode"      "ModeFreq" 

# change fields to match type in
# /home/jiml/HotWaterResearch/projects/CECHWT24/hot water calcs/draw patterns/Aquacraft/AllCalEvents_2011.csv.ftype
# INT,TEXT,TEXT,SHORT_DATE_TIME,LONG,FLOAT,FLOAT,FLOAT,LONG

# exam & convert Keycode
DT_AllCalEvents[, Keycode.i:=as.integer(Keycode)]
DT_AllCalEvents[,list(unique(Keycode),
                      is.integer(Keycode.i)),
                by=Keycode.i][,sort(Keycode.i)]
DT_AllCalEvents[, Keycode:=Keycode.i]
DT_AllCalEvents[, Keycode.i:=NULL]

# check SumAs and CountAs
DT_AllCalEvents[, testAs:= (SumAs==CountAs)]
DT_AllCalEvents[,length(Keycode),by=testAs]
#    testAs      V1
# 1:   TRUE 2132873
# 2:  FALSE   35825

# check the testAs FALSE
DT_AllCalEvents[testAs==FALSE,length(Keycode),by=CountAs]
#          CountAs    V1
# 1:               34858
# 2: Clotheswasher    16
# 3:    Dishwasher     5
# 4:  Clotheswashe   946
DT_AllCalEvents[testAs==FALSE,length(Keycode),by=SumAs]
#             SumAs    V1
# 1:  Clotheswasher 28704
# 2:     Dishwasher  7100
# 3: Clotheswasher@    16
# 4:    Dishwasher@     5
DT_AllCalEvents[testAs==FALSE,table(SumAs,CountAs)]
# CountAs
# SumAs                  Clotheswashe Clotheswasher Dishwasher
# Clotheswasher  27758          946             0          0
# Clotheswasher@     0            0            16          0
# Dishwasher      7100            0             0          0
# Dishwasher@        0            0             0          5
DT_AllCalEvents[,length(Keycode),by=SumAs]
DT_AllCalEvents[,length(Keycode),by=CountAs]

with(DT_AllCalEvents[SumAs %in% c('Clotheswasher','Dishwasher','','Clotheswasher@','Dishwasher@'),],
     table(SumAs, CountAs, useNA = "ifany"))
#               CountAs
# SumAs                  Clotheswashe Clotheswasher Dishwasher
#                 7551            0             0          0
# Clotheswasher  27758          946          7860          0
# Clotheswasher@     0            0            16          0
# Dishwasher      7100            0             0       1925
# Dishwasher@        0            0             0          5


# remove unwanted events
DT_HotCalEvents <- DT_AllCalEvents[!(SumAs=='Leak'|SumAs=='Toilet'|SumAs=='Irrigation'|SumAs=='Other'),]

# change StartTime to a POSIXct date-time object
DT_HotCalEvents[,start.time := parse_date_time(StartTime,"%a %b %d %H:%M:%S  %Y", tz="America/Los Angeles")]
# add separate fields for day of week and hour of day
DT_HotCalEvents[, dow := lubridate::wday(start.time, label=TRUE, abbr=TRUE)]
DT_HotCalEvents[, hrday := lubridate::hour(start.time)]

# change Duration, Peak, Volume, Mode, ModeFreq to numbers
DT_HotCalEvents[, Duration := as.numeric(Duration)]
DT_HotCalEvents[, Peak := as.numeric(Peak)]
DT_HotCalEvents[, Volume := as.numeric(Volume)]
DT_HotCalEvents[, Mode := as.numeric(Mode)]
DT_HotCalEvents[, ModeFreq := as.numeric(ModeFreq)]

str(DT_HotCalEvents)












====================

# weight by NBr
DT_NBr_weight <- DT_RASS_NBr[,list(NBr_weight = sum(sample_weight),NBr_sample=.N), by=NBr][order(NBr),]

# save as csv
write.csv(DT_NBr_weight, file=paste0(wd_data,"RASS_NBr_weights.csv"), row.names = FALSE)

# weight by combination of NBr and code
DT_code_weight <- DT_RASS_NBr[,list(code_weight = sum(sample_weight),
                                    code_sample = .N          # count of number of houses sampled with that combination
                                    ), by=c("NBr","code")][order(NBr,-code_weight)]

# save as csv
write.csv(DT_code_weight, file=paste0(wd_data,"RASS_code_weights.csv"), row.names = FALSE)

# merge tables to get weights by NBr in same table
DT_combin_weight <- merge(DT_code_weight,DT_NBr_weight,by="NBr", all=TRUE)[order(NBr,-code)]

# find fraction of houses by NBr with each occupancy combination code. By weight
DT_combin_weight[,f_NBr:= code_weight/NBr_weight]

# keep occupancy combination codes that are >5% for each NBr
DT_common_weight <- DT_combin_weight[f_NBr>0.05, ][order(NBr,-f_NBr)][, list(code, code_weight, code_sample, f_NBr, 
                                                        NBr_weight=sum(code_weight)),
                                                 by=NBr]

# add equivalent number of days
DT_common_weight[,code_days:=round(code_weight/NBr_weight * 365)][]


# save as csv
write.csv(DT_common_weight, file=paste0(wd_data,"RASS_common_weights.csv"), row.names = FALSE)

