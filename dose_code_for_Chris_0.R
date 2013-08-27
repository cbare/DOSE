
library(lhs)
library(DiceDesign)
library(MASS)
library(glmnet)


TransformDesign <- function(x, par.ranges, is.discrete, lin.trans) {
  X <- x
  for (i in 1:ncol(x)) {
    par.range <- par.ranges[[i]]
    par.min <- min(par.range)
    par.max <- max(par.range)
    if (lin.trans[i]) {
      aux <- x[, i] * (par.max - par.min) + par.min
    }
    else {
      par.mid <- 1
      ind1 <- x[, i] <= 0.5
      ind2 <- x[, i] > 0.5
      aux <- rep(NA, length(x[, i]))
      aux[ind1] <- 2 * x[ind1, i] * (par.mid - par.min) + par.min
      aux[ind2] <- 
        2 * x[ind2, i] * (par.max - par.mid) + 2 * par.mid - par.max 
    }
    if (is.discrete[[i]]) {
      X[, i] <- round(aux)
    }
    else {
      X[, i] <- aux
    }  
  }
  X
}




GetRidgeGrid <- function(d, U, V, y, beta.max = 0.001, epsilon = 1e-6, 
                         K = 100, upper.bound = TRUE) {
  r <- length(d)
  aux <- V %*% t(U) %*% y
  if (upper.bound) {
    lambda.max <- max(d * abs(aux[1:r,1]))/beta.max
  }
  else {
    lambda.max <- max(d * abs(aux[1:r,1]))/beta.max - max(d^2)
  }
  exp(seq(log(lambda.max), log(epsilon * lambda.max), length.out = K))
}




FitRidge <- function(X, Y, lambda = NULL, beta.max = 0.001, 
                     epsilon = 1e-6, K = 100) {
  n <- nrow(X)
  p <- ncol(X)
  Xs <- svd(X)
  rhs <- t(Xs$u) %*% Y
  d <- Xs$d
  if (is.null(lambda)) {
    lambda <- GetRidgeGrid(d, Xs$u, Xs$v, Y, beta.max,
                           epsilon, K)
  }
  k <- length(lambda)
  dx <- length(d)
  div <- d^2 + rep(lambda, rep(dx, k))
  a <- drop(d * rhs)/div
  dim(a) <- c(dx, k)
  Xs$v %*% a  
}




OptimizeRidge <- function(Xtrain, Ytrain, Xtest, nfolds, lambda.grid = NULL,
                          beta.max = 0.001, epsilon = 1e-6, K = 100) {
  p <- ncol(Xtrain)
  n <- nrow(Xtrain)
  if (is.null(lambda.grid)) {
    Xs <- svd(Xtrain)
    lambda.grid <- GetRidgeGrid(Xs$d, Xs$u, Xs$v, Ytrain, beta.max, 
                                epsilon, K)
  }
  foldsize <- round(n/nfolds)
  nlambda <- length(lambda.grid)
  MSEs <- matrix(NA, nfolds, nlambda)
  for (i in 1:nfolds) {
    index <- sample(1:n, foldsize, replace = FALSE)
    Betas <- FitRidge(Xtrain[-index,], Ytrain[-index], lambda.grid)
    MSEs[i, ] <- apply((Ytrain[index] - Xtrain[index,] %*% Betas)^2, 2, mean)
  }
  best <- which.min(apply(MSEs, 2, mean))
  list(pred = Xtest %*% Betas[, best], best.lambda = lambda.grid[best])
}




OptimizeLasso <- function(Xtrain, Ytrain, Xtest, nfolds, lambda.grid) {
  cv.fit <- cv.glmnet(Xtrain, Ytrain, alpha = 1, lambda = lambda.grid, 
                      nfolds = nfolds)
  list(pred = predict(cv.fit, Xtest, s = "lambda.min"),
       best.lambda = cv.fit$lambda.min)
}




OptimizeEnet <- function(Xtrain, Ytrain, Xval, nfolds, alpha.grid, 
                         lambda.grid) {
  nalpha <- length(alpha.grid)
  MSE <- best.lambda <- rep(NA, nalpha)
  for (i in 1:nalpha) {
    cv.fit <- cv.glmnet(Xtrain, Ytrain, alpha = alpha.grid[i], 
                        lambda = lambda.grid, nfolds = nfolds)
    index <- which.min(cv.fit$lambda)
    best.lambda[i] <- cv.fit$lambda.min
    MSE[i] <- cv.fit$cvm[index]
  }
  best <- which.min(MSE)
  best.fit <- glmnet(Xtrain, Ytrain, alpha = alpha.grid[best])
  list(pred = predict(best.fit, Xval, s = best.lambda[best]), 
       best.alpha = alpha.grid[best],
       best.lambda = best.lambda[best])
}




CreateSigma <- function(rho, p) {
  aux1 <- matrix(rep(1:p, p), p, p)
  aux2 <- matrix(rep(1:p, each = p), p, p) 
  rho^abs(aux1 - aux2)
}






GetBlockSizes <- function(p, max.block, min.block) {
  max.block <- min(c(p, max.block))
  block.sizes <- sample(min.block:max.block, ceiling(p/min.block), 
                        replace = TRUE)
  cum.sizes <- cumsum(block.sizes)
  block.sizes <- block.sizes[which(cum.sizes <= p)]
  aux <- which.min(block.sizes)
  block.sizes[aux] <- block.sizes[aux] + p - sum(block.sizes)
  block.sizes 
}





SimulateData <- function(n, beta, sig, rho, max.block, min.block) {
  p <- length(beta)
  block.sizes <- GetBlockSizes(p, max.block, min.block)
  nblocks <- length(block.sizes)
  Sigma <- CreateSigma(rho, block.sizes[1])
  X <- mvrnorm(n, rep(0, block.sizes[1]), Sigma)
  if (nblocks > 1) {
    for (i in 2:nblocks) {
      Sigma <- CreateSigma(rho, block.sizes[i])
      X <- cbind(X, mvrnorm(n, rep(0, block.sizes[i]), Sigma))  
    }
  }
  y <- X %*% beta + sig * rnorm(n)
  list(y = y, X = X)
}



SimDataAndFitModels <- function(seed,
                                sim.pars, 
                                nfolds = 10,
                                alpha.grid,
                                lambda.grid = NULL,
                                scale.Y = TRUE,
                                max.block = 300,
                                min.block = 20) {
  out <- rep(NA, 13)
  names(out) <- c("mse.R", "mse.L", "mse.E", "lambda.R", "lambda.L", 
                  "alpha.E", "lambda.E", "n", "p", "phi", "eta", "rho",
                  "random.seed")
  
  ## sample parameter values
  n <- sim.pars[1]
  p <- sim.pars[2]
  phi <- sim.pars[3]
  eta <- sim.pars[4]
  rho <- sim.pars[5]
  
  set.seed(seed)
  beta.star <- rnorm(p)
  ind.beta <- rbinom(p, 1, phi)
  if (sum(ind.beta) != 0) {
    beta <- (eta * beta.star * ind.beta * sum(ind.beta))/sum(ind.beta * 
                                                               abs(beta.star))
  }
  
  ## simulate train and test data
  set.seed(seed)
  dat <- SimulateData(2*n, beta, sig = 1, rho, max.block, min.block)
  if(scale.Y) {
    Ytrain <- scale(dat[[1]][1:n,])
    Ytest <- scale(dat[[1]][(n+1):(2*n),])
  }
  else {
    Ytrain <- dat[[1]][1:n,] - mean(dat[[1]][1:n,])
    Ytest <- dat[[1]][(n+1):(2*n),] - mean(dat[[1]][(n+1):(2*n),])
  }
  Xtrain <- scale(dat[[2]][1:n,])
  Xtest <- scale(dat[[2]][(n+1):(2*n),])
  p <- length(beta)
  
  ## fit ridge regression 
  set.seed(seed)
  aux1 <- OptimizeRidge(Xtrain, Ytrain, Xtest, nfolds, lambda.grid)
  out[4] <- aux1$best.lambda
  out[1] <- mean((Ytest - aux1$pred)^2)
  
  ## fit the lasso 
  set.seed(seed)
  aux2 <- OptimizeLasso(Xtrain, Ytrain, Xtest, nfolds, lambda.grid)
  out[5] <- aux2$best.lambda
  out[2] <- mean((Ytest - aux2$pred)^2)
  
  ## fit the elastic net 
  set.seed(seed)
  aux3 <- OptimizeEnet(Xtrain, Ytrain, Xtest, nfolds, alpha.grid, lambda.grid)
  out[6] <- aux3$best.alpha
  out[7] <- aux3$best.lambda
  out[3] <- mean((Ytest - aux3$pred)^2)
  
  out[8] <- n
  out[9] <- p
  out[10] <- phi
  out[11] <- eta
  out[12] <- rho
  out[13] <- seed
  
  out
}



## for checking out par ratios
##
SimDataAndFitModels6pars <- function(seed,
                                     sim.pars, 
                                     nfolds = 10,
                                     alpha.grid,
                                     lambda.grid = NULL,
                                     scale.Y = TRUE,
                                     max.block = 300,
                                     min.block = 20) {
  out <- rep(NA, 14)
  names(out) <- c("mse.R", "mse.L", "mse.E", "lambda.R", "lambda.L", 
                  "alpha.E", "lambda.E", "n", "p", "phi", "eta", "sig", "rho",
                  "random.seed")
  
  ## sample parameter values
  n <- sim.pars[1]
  p <- sim.pars[2]
  phi <- sim.pars[3]
  eta <- sim.pars[4]
  sig <- sim.pars[5]
  rho <- sim.pars[6]
  
  set.seed(seed)
  beta.star <- rnorm(p)
  ind.beta <- rbinom(p, 1, phi)
  if (sum(ind.beta) != 0) {
    beta <- (eta * beta.star * ind.beta * sum(ind.beta))/sum(ind.beta * 
                                                               abs(beta.star))
  }
  
  ## simulate train and test data
  set.seed(seed)
  dat <- SimulateData(2*n, beta, sig, rho, max.block, min.block)
  if(scale.Y) {
    Ytrain <- scale(dat[[1]][1:n,])
    Ytest <- scale(dat[[1]][(n+1):(2*n),])
  }
  else {
    Ytrain <- dat[[1]][1:n,] - mean(dat[[1]][1:n,])
    Ytest <- dat[[1]][(n+1):(2*n),] - mean(dat[[1]][(n+1):(2*n),])
  }
  Xtrain <- scale(dat[[2]][1:n,])
  Xtest <- scale(dat[[2]][(n+1):(2*n),])
  p <- length(beta)
  
  ## fit ridge regression 
  set.seed(seed)
  aux1 <- OptimizeRidge(Xtrain, Ytrain, Xtest, nfolds, lambda.grid)
  out[4] <- aux1$best.lambda
  out[1] <- mean((Ytest - aux1$pred)^2)
  
  ## fit the lasso 
  set.seed(seed)
  aux2 <- OptimizeLasso(Xtrain, Ytrain, Xtest, nfolds, lambda.grid)
  out[5] <- aux2$best.lambda
  out[2] <- mean((Ytest - aux2$pred)^2)
  
  ## fit the elastic net 
  set.seed(seed)
  aux3 <- OptimizeEnet(Xtrain, Ytrain, Xtest, nfolds, alpha.grid, lambda.grid)
  out[6] <- aux3$best.alpha
  out[7] <- aux3$best.lambda
  out[3] <- mean((Ytest - aux3$pred)^2)
  
  out[8] <- n
  out[9] <- p
  out[10] <- phi
  out[11] <- eta
  out[12] <- sig
  out[13] <- rho
  out[14] <- seed
  
  out
}





