source('http://depot.sagebase.org/CRAN.R')
pkgInstall(c("synapseClient"))
install.packages('rredis')

require(rredis)
require(synapseClient)


set.seed(123456789)
myseeds <- sample(10000:100000, 10000, replace = FALSE)
alphas <- c(10^(-3:-1), seq(0.2, 0.9, by = 0.1))

redisSet('myseeds', myseeds)
redisSet('alphas', alphas)

load("pilot_design.RData")
#load("large_sim_design.RData")
nSim <- nrow(X)

registerDoRedis('jobs')

redisSet('X', X)

## calculate remaining simulations
checkpoint.entity <- synGet('syn2195791')
checkpoint <- read.table(getFileLocation(checkpoint.entity), header=T)
sims <- setdiff(1:nSim, checkpoint$i)

## order simulations by how long they'll take
sims <- sims[order(X[sims,1]*X[sims,2], decreasing=TRUE)]

# for (i in sims) {
#   redisRPush('jobs', i)
# }

registerDoRedis('jobs')
ans <- foreach(i in sims) %dopar% {
  try({
    #cat("sim = ", i, "\n")
    t <- system.time(
      ans <- try({
        SimDataAndFitModels(seed = myseeds[i], 
                            sim.pars = X[i,], 
                            alpha.grid = alphas)
      })
    )
    output <- append(append(c(i=i), ans), summary(t))
    write.table(output, file=log.filename, append=TRUE,
                row.names=FALSE, col.names=FALSE, sep="\t")
    output
  })
}

removeQueue('jobs')
