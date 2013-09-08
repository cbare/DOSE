## install packages
source('http://depot.sagebase.org/CRAN.R')
pkgInstall(c("synapseClient"))
install.packages('rredis')
install.packages('doRedis')

# load libraries
require(rredis)
require(doRedis)
require(synapseClient)

## set up simulation data
set.seed(123456789)
myseeds <- sample(10000:100000, 10000, replace = FALSE)
alphas <- c(10^(-3:-1), seq(0.2, 0.9, by = 0.1))

load("DOSE/pilot_design.RData")
#load("large_sim_design.RData")
nSim <- nrow(X)

redisConnect()
redisSet('myseeds', myseeds)
redisSet('alphas', alphas)
redisSet('X', X)

## calculate remaining simulations
checkpoint.entity <- synGet('syn2195791')
checkpoint <- read.table(getFileLocation(checkpoint.entity), header=T)
sims <- setdiff(1:nSim, checkpoint$i)

## order simulations by (roughly) how long they'll take
sims <- sims[order(X[sims,1]*X[sims,2], decreasing=TRUE)]

# for (i in sims) {
#   redisRPush('jobs', i)
# }

registerDoRedis('jobs')
ans <- foreach(i=sims, .verbose=TRUE) %dopar% {
  try({
    t <- system.time(
      ans <- try({
        SimDataAndFitModels(seed = myseeds[i], 
                            sim.pars = X[i,], 
                            alpha.grid = alphas)
      })
    )
    output <- append(append(list(i=i), ans), summary(t))
    write.table(output, file=log.filename, append=TRUE,
                row.names=FALSE, col.names=FALSE, sep="\t")
    output
  })
}

removeQueue('jobs')
