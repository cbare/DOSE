# .libPaths( c('/home/ubuntu/R/library', .libPaths()) )
# source("DOSE/dose_code_for_Chris_0.R") 

## record version info
session.info <- sessionInfo()

## get seeds, alphas and X from redis


## start taking jobs from the queue
require(’doRedis’)
redisWorker(’jobs’)

