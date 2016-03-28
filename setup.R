# setup.R
# make sure any needed packages are loaded
# Jim Lutz  Mon Jan  6 10:20:41 CST 2014
# "Mon Jan  6 22:32:35 2014"
# had to do these steps individually instead of sourcing this file

# http://cran.cnr.berkeley.edu/bin/linux/ubuntu/
# http://www.r-bloggers.com/r-3-0-0-is-released-whats-new-and-how-to-upgrade/
# installing/loading the package:
# if(!require(installr)) { 
#     install.packages("installr"); require(installr)} #load / install+load installr
# Warning in install.packages :
#   package ‘installr’ is not available (for R version 3.0.2)
# HA it's for Windows machines, not linux



# had to change permissions for library
# /usr/lib/R$ sudo -R chmod 777 library

# added & changed permissions for
# /home/jiml/R/packages 
# see: http://www.r-bloggers.com/installing-r-packages/

# clean up leftovers before starting
# clear all the objects except fn_script
l_obj=ls(all=TRUE)
l_obj = c(l_obj, "l_obj") # be sure to include l_obj
rm(list = l_obj[l_obj != "fn_script"])
# clear the plots
if(!is.null(dev.list())){
  dev.off(dev.list()["RStudioGD"])
}
# clear history
cat("", file = "nohistory")
loadhistory("nohistory")
# clear the console
cat("\014")

# only works if have internet access
update.packages(checkBuilt=TRUE)

# work with logging
if(!require(logging)){install.packages("logging")}
library(logging)

# work with dates & times
# need this for logging.R
if(!require(lubridate)){install.packages("lubridate")}
library(lubridate)


# set up logging file
source("logging.R")
fn_log_file <- paste(fn_script,".log",sep="")
log_file(writeToFile, fn_log_file) 

# track time 
start_time <- now()
logwarn('start time')

# work with rJava
#if(!require(rJava)){install.packages("rJava")}
#library(rJava)
#logwarn('rJava loaded')

# work with xlsx files
#if(!require(xlsx)){install.packages("xlsx")}
#library(xlsx)
#logwarn('xlsx loaded')

# work with zoo
if(!require(zoo)){install.packages("zoo")}
library(zoo)
logwarn('zoo loaded')

library(lubridate)
logwarn('reloaded lubridate')


sessionInfo() 
# 2015-01-28 15:53:30 
# R version 3.1.2 (2014-10-31)
# Platform: i686-pc-linux-gnu (32-bit)

# work with data.tables
if(!require(data.table)){install.packages("data.table")}
library(data.table)
logwarn('data.table loaded')

# work with reshape
# change shape of data
# http://had.co.nz/reshape/
# Wickham, Hadley. ???Reshaping Data with the Reshape Package.??? Journal of Statistical Software 21, no. 12 (November 2007): 1???20.
if(!require(reshape)){install.packages("reshape")}
library(reshape)
logwarn('reshape loaded')

if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
logwarn('reshape2 loaded')

# work with plyr
if(!require(plyr)){install.packages("plyr")}
library(plyr)
logwarn('plyr loaded')


# work with stringr
if(!require(stringr)){install.packages("stringr")}
library(stringr)
logwarn('stringr loaded')

# work with stringi
if(!require(stringi)){install.packages('stringi')}
library(stringi)
logwarn('stringi loaded')


# work with ggplot2
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
logwarn('ggplot2 loaded')

# change the default background for ggplot2 to white, not gray
theme_set( theme_bw() )

# generic plot scaling methods
if(!require(scales)){install.packages("scales")}
library(scales)
logwarn('scales loaded')

# work with  GGally
if(!require( GGally)){install.packages("GGally")}
library( GGally)
logwarn(' GGally loaded')

# work with  readr
# http://blog.rstudio.org/2015/04/09/readr-0-1-0/
if(!require( readr)){install.packages("readr")}
library( readr)
logwarn(' readr loaded')

# work with  tidyr
if(!require( tidyr)){install.packages("tidyr")}
library( tidyr)

logwarn(' tidyr loaded')

# # work with  xlsx
# if(!require( xlsx)){install.packages("xlsx")}
# library( xlsx)
# 
# logwarn(' xlsx loaded')

