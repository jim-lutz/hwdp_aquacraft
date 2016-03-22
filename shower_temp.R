fn_script = "shower_temp.R"
# calculate shower temperatures from Aquacraft hot and mains data 

# Jim Lutz    "Mon Mar 21 16:47:29 2016"

# make sure all packages loaded and start logging
source("setup.R")

# set the working directory names 
source("setup_wd.R")

# load useful functions (probably don't need this.)
source("functions.R")

# load Aquacraft data files
# path to directory
wd_Aquacraft  <- "/home/jiml/HotWaterResearch/projects/CECHWT24/hot water calcs/draw patterns/Aquacraft/" 

# get DT_temp_events.Rdata
fn_DT_temp_events <- paste0(wd_data,"DT_temp_events.Rdata")
load(fn_DT_temp_events) 
str(DT_temp_events)
# Classes ‘data.table’ and 'data.frame':	86539 obs. of  20 variables:

names(DT_temp_events)

# look at just the shower data


