require(rredis)
require(doRedis)

.libPaths( c('/home/ubuntu/R/library', .libPaths()) )
source("DOSE/dose_code_for_Chris_0.R") 

## record version info
session.info <- sessionInfo()

my.ip <- system("hostname -i", intern=TRUE)
log.filename <<- sprintf("dose.large.node.%s.tsv", my.ip)
log.colnames <- c("i", "mse.R", "mse.L", "mse.E", "lambda.R", "lambda.L", 
                "alpha.E", "lambda.E", "n", "p", "phi", "eta", "rho",
                "random.seed",
                "user", "system", "elapsed")
write(log.colnames, ncolumns=length(log.colnames), file=log.filename, sep="\t")

redisConnect(host='10.6.173.133')

## get seeds, alphas and X from redis
myseeds <- redisGet('myseeds')
alphas <- redisGet('alphas')
X <- redisGet('X')

## start taking jobs from the queue
redisWorker('jobs', host='10.6.173.133')

