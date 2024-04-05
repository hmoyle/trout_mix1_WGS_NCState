

ngsadmix_dir <- "data/ngsadmix/maf_0.05" # set NGSadmix outputs directory
N_K <- 10    # set number of K run
N_reps <- 4  # set number of reps run

## choose K

# pull all log files
log_files <- list.files(ngsadmix_dir, pattern = ".log", full.names = T, recursive=T)

# read in all logs
all_logs <- lapply(1:length(log_files), FUN = function(i) readLines(log_files[i]))

# make list of the line that starts with "best like=" from all logs, just target 'b'
library(stringr)
bestlikes_str_list <- sapply(1:length(log_files), FUN= function(x) all_logs[[x]][which(str_sub(all_logs[[x]], 1, 1) == 'b')])

# make dataframe with 1:N_K and N_reps to add likelihood values
loglikes <- data.frame(K = rep(2:N_K, each=N_reps))

# add the log likelihood (first number in the string)
loglikes$loglike<-as.vector(as.numeric( sub("\\D*(\\d+).*", "\\1", bestlikes_str_list) ))

# calculate delta K and probability
# choose the K with the highest value
# ex.
#   1         2         3         4         5 
# Inf       Inf  41.50002  29.32132 394.18113 
# in this case, it's 5 - will need to run more K so that we make sure best value is not >5
# ex. run 2
# 1          2          3          4          5          6          7          8          9         10 
# Inf  1980.5446        Inf   195.7116        Inf  1525.4743  2816.0378 87997.0985        Inf  1784.6857 
# K=8 should be chosen
tapply(loglikes$loglike, loglikes$K, FUN= function(x) mean(abs(x))/sd(abs(x)))

# K=3 should be chosen
