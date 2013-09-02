library(parallel)

# get IP addresses of workers
lines <- readLines("/usr/local/Rmpi/hostfile.plain")
workers <- do.call(c, lapply(strsplit(lines, " "), function(host) { rep(host[1], as.integer(host[2])) }))

# Get code out of github
# install necessary packages
# stuff that needs to happen once per machine
hosts <- unique(workers)
cl <- makePSOCKcluster(hosts, master=system("hostname -i", intern=TRUE))
clusterEvalQ(cl, { system("git clone https://github.com/cbare/DOSE.git") })
clusterEvalQ(cl, { .libPaths( c('/home/ubuntu/R/library', .libPaths()) ) })
clusterEvalQ(cl, { options(repos=structure(c(CRAN="http://cran.fhcrc.org/")))  })
clusterEvalQ(cl, { install.packages('glmnet') })
clusterEvalQ(cl, { install.packages('lhs') })
clusterEvalQ(cl, { install.packages('DiceDesign') })
clusterEvalQ(cl, { system('curl -O http://cran.r-project.org/src/contrib/Archive/MASS/MASS_7.3-22.tar.gz ') })
clusterEvalQ(cl, { install.packages("./MASS_7.3-22.tar.gz", repos=NULL, type='source') })
stopCluster(cl)

cl <- makePSOCKcluster(workers, master=system("hostname -i", intern=TRUE))
clusterEvalQ(cl, { .libPaths( c('/home/ubuntu/R/library', .libPaths()) ) })

clusterEvalQ(cl, { source("DOSE/dose_code_for_Chris_0.R") })

set.seed(123456789)
myseeds <- sample(10000:100000, 10000, replace = FALSE)
alphas <- c(10^(-3:-1), seq(0.2, 0.9, by = 0.1))
clusterExport(cl, c("myseeds", "alphas"))

load("pilot_design.RData")
nSim <- nrow(X)
clusterExport(cl, c("X"))

## Chris,
## Basically, we just need to paralellize the "for loop" below.

output <- matrix(NA, nSim, 13)
colnames(output) <- c("mse.R", "mse.L", "mse.E", "lambda.R", "lambda.L", 
                      "alpha.E", "lambda.E", "n", "p", "phi", "eta", "rho",
                      "random.seed")

##for (i in 1:nSim) {
running.time <- system.time(
  ans <- parLapplyLB(cl,
    1:nSim,
    function(i) {
      try({
        cat("sim = ", i, "\n")
        SimDataAndFitModels(seed = myseeds[i], 
                            sim.pars = X[i,], 
                            alpha.grid = alphas)
      })
    }))

stopCluster(cl)
