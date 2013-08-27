
source("dose_code_for_Chris_0.R")

nSim <- 1000
set.seed(123)
X <- maximinLHS(nSim, 5)
par.ranges <- list(c(100, 500), c(501, 1000), c(0.1, 0.9), c(0.1, 10), c(0.1, 0.9))
is.discrete <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
lin.trans <- c(TRUE, TRUE, TRUE, FALSE, TRUE)
X <- TransformDesign(X, par.ranges, is.discrete, lin.trans)

#save(X, file = "pilot_design.RData", compress = TRUE)


