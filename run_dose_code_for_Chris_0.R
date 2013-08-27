source("dose_code_for_Chris_0.R")

set.seed(123456789)
myseeds <- sample(10000:100000, 10000, replace = FALSE)
alphas <- c(10^(-3:-1), seq(0.2, 0.9, by = 0.1))

load("pilot_design.RData")
nSim <- nrow(X)

## Chris,
## Basically, we just need to paralellize the "for loop" below.

output <- matrix(NA, nSim, 13)
colnames(output) <- c("mse.R", "mse.L", "mse.E", "lambda.R", "lambda.L", 
                      "alpha.E", "lambda.E", "n", "p", "phi", "eta", "rho",
                      "random.seed")
  
for (i in 1:nSim) {
  cat("sim = ", i, "\n")
  output[i,] <- SimDataAndFitModels(seed = myseeds[i], 
                                    sim.pars = X[i,], 
                                    alpha.grid = alphas)
}









