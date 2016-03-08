# setup_wd.R
# Jim Lutz
# "Thu Sep 19 15:34:56 2013"

# setup  working directories
# be sure to start in the scripts directory
wd <- sub("/scripts","",getwd())
logwarn('working directory %s',wd)

wd_scripts <-paste(wd,"/scripts/",sep="")    # use this for scripts
wd_data    <- paste(wd,"/data/",sep="")      # use this for interim data files
wd_results <- paste(wd,"/results/",sep="")   # use this for intermediate results 
wd_charts  <-paste(wd,"/charts",sep="")     # use this for charts, ggsave puts in /
setwd(wd_scripts) # go back the the scripts subdirectory
logwarn('working directories assigned')

