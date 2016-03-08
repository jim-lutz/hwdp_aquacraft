fn_script = "load_CA.R"
# first attempt at loading and analyzing Aquacraft data

# Jim Lutz "Mon Mar  7 18:19:03 2016"

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

names(DT_AllCalEvents)
# [1] "Keycode"   "SumAs"     "CountAs"   "StartTime" "Duration"  "Peak"      "Volume"    "Mode"      "ModeFreq" 

str(DT_AllCalEvents)
DT_AllCalEvents[1]
#   Keycode SumAs CountAs       StartTime Duration  Peak Volume  Mode ModeFreq
#1:    #INT  TEXT    TEXT SHORT_DATE_TIME     LONG FLOAT  FLOAT FLOAT     LONG

# convert fields to correct format


# rename some columns
setnames(DT_RASS_NBr, c("People_.5", "People_6.18", "People_65.99", "People_Total"),
                      c("N_kids",    "N_youth",     "N_senior",     "N_total"))

# calc number of adults 
DT_RASS_NBr[,N_adult:=People_19.34 + People_35.54 + People_55.64]

# drop the unneeded columns
DT_RASS_NBr[,c("People_19.34", "People_35.54", "People_55.64"):= NULL]

# make a code for occupancy combination type
DT_RASS_NBr[,code:=paste(N_senior,N_adult,N_youth,N_kids,sep="_")]

# reorder columns for easier reading
setcolorder(DT_RASS_NBr,c("ID", "NBr", "sample_weight", "N_total", "N_senior", "N_adult", "N_youth", "N_kids", "code"))

# Houses with more than 10 people
DT_RASS_NBr[N_total>10,]
# 84 houses.

# houses with more than 5 bedrooms
DT_RASS_NBr[NBr>5,]
# 181 houses

# houses without any seniors or adults
DT_RASS_NBr[stri_startswith_fixed(code, "0_0_"),]
# 818 houses!

DT_RASS_NBr[N_youth>5,]
DT_RASS_NBr[ID== 25444]

# get rid of undesirable records
DT_RASS_NBr <- DT_RASS_NBr[N_total<=10 & NBr<=5 & !stri_startswith_fixed(code, "0_0_"),]

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

