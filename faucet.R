fn_script = "faucet.R"
# loads Aquacraft faucet data to determine distribution of fraction hot by events, volume, and GPM

# Jim Lutz "Tue Mar 15 15:58:21 2016"
# "Wed Mar 16 10:49:59 2016"    calculate median fraction hot volume for faucet events 

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
fn_Fau_use <- paste0(wd_Aquacraft,"tblFau_use_HOD_13 day.csv")
DT_Fau_use <- fread(fn_Fau_use)

str(DT_Fau_use)
# Classes ‘data.table’ and 'data.frame':	198 obs. of  10 variables:

setkeyv(DT_Fau_use, c("Keycode", "TraceType"))

# try calculating fraction hot
# lag the following columns
cols = c("TotalEvents", "TotalGal", "AvgFlowRate")
lag_cols = paste("lag", cols, sep="_")
DT_Fau_use[, (lag_cols):=shift(.SD, n=1, type="lag"), .SDcols=cols]

# calculat fraction hot
DT_Fau_use[, `:=` (Events_FH  =   lag_TotalEvents/TotalEvents,
                   Gal_FH     =   lag_TotalGal/TotalGal,
                   GPM_FH     =   lag_AvgFlowRate/AvgFlowRate)]

# now keep just the MAIN
DT_Fau_frac_hot <- DT_Fau_use[TraceType=="MAIN",list(Keycode, Fixture, Events_FH, Gal_FH, GPM_FH)][order(Events_FH)]

# save to csv file
write.csv(DT_Fau_frac_hot, file=paste0(wd_data,"faucet_fraction_hot.csv"),row.names=FALSE)


summary(DT_Fau_frac_hot$Events_FH)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0658  0.6414  0.8485  0.9585  1.0530  7.2170 

summary(DT_Fau_frac_hot$Gal_FH)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.08201 0.48810 0.59390 0.63160 0.72730 2.64600 

median(DT_Fau_frac_hot$Gal_FH)
# [1] 0.5939476

summary(DT_Fau_frac_hot$GPM_FH)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1809  0.5492  0.7631  0.7471  0.9062  1.8510 


P <- ggplot(DT_Fau_frac_hot,aes(x=Events_FH)) 
P <- P + geom_step(aes(y=..y..),stat="ecdf") + coord_flip()
P <- P + ylab("houses") + xlab("fraction of faucet draws that are hot")
P <- P + ggtitle("cumulative distribution of fraction of faucet draws that are hot")
P

