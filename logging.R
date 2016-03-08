# logging.R
# source file to set up convenient logging

# add logger
if(!require(logging)){install.packages("logging")}
library(logging)

# set up loggers and handlers
basicConfig(level='FINEST')
setLevel(30, getHandler('basic.stdout')) # warnings to screen


log_file <- function (writeToFile, fn_log_file) {
# function to set up a new logging file
    # var is full path name of log file
  addHandler(writeToFile, file=fn_log_file, level='DEBUG') # debug to log file
  
  if ( file.exists(fn_log_file) ) { # start w/ fresh log file
      file.remove(fn_log_file)
  }
}

summ_warn <- function (name_of_numeric_vector, numeric_vector) {
# function to log some summary data
    # first var is name of numeric vector, second var is numeric vector
    logwarn('summary stats of %s',name_of_numeric_vector)
    
    stats_sum <- summary(numeric_vector)
    
    stats_names <- paste(names(stats_sum))
    logwarn(':    %s',stats_names)
    stats_values <- paste(stats_sum)
    logwarn(':    %s',stats_values)
    
    rm(stats_sum,stats_names,stats_values)
}
