ans.original <- read.table('junk/checkpoint9.tsv', header=T)

temp.env = new.env()
load('pilot_design.RData', envir=temp.env)
X.pilot <- temp.env$X
load('large_sim_design.RData', envir=temp.env)
X.large <- temp.env$X


close.enough <- function(a,b) {
  sum(abs(a-b)) < 1e-10
}

classify <- function(ans, X.large, X.pilot) {
  apply(ans, 1, function(row){
    i <- row['i']
    params <- row[c('n','p','phi','eta','rho')]
    if (close.enough(params, X.large[i,])) {
      return('large')
    } else if (close.enough(params, X.pilot[i,])) {
      return('pilot')
    } else {
      return('???')
    }
  })
}

## remove pilot study entries from answer
classify.ans <- classify(ans.original, X.large, X.pilot)
ans <- ans.original[classify.ans=='large',]

## remove duplicates from answer
ans <- ans[!duplicated(ans$i),]

## missing rows
missing.rows <- setdiff(1:10000, ans$i)


## get checkpoint run with parallel package
entity.checkpoint.169 <- synGet('syn2203316')
checkpoint169 <- read.table(getFileLocation(entity.checkpoint.169), header=T)

common.i <- intersect(checkpoint169$i, ans.10k$i)


## compare current answers with previous partial run
all(sapply(common.i, function(i) {
  all(checkpoint169[checkpoint169$i==i, 1:14] == ans.10k[ans.10k$i==i, 1:14])
}))

## TRUE! (it better be!)

## or considering round-off error are they close enough?
## most results were written to txt log files and read in from there
## which introduces round off error in the smallest digit
all(sapply(common.i, function(i) {
  all(close.enough(checkpoint169[checkpoint169$i==i, 1:14], ans.10k[ans.10k$i==i, 1:14]))
}))


## model running time
library(glmnet)

train <- sample.int(nrow(ans), 0.85 * nrow(ans))
test <- setdiff(1:nrow(ans), train)

data <- ans[, c('n', 'p', 'phi','eta','rho')]
data$n_times_p <- ans$n * ans$p

fit <- cv.glmnet(x=as.matrix(data[train,]),
                 y=as.matrix(ans[train, c('elapsed'), drop=FALSE]),
                 alpha=0.8)

predicted <- predict(fit, as.matrix(data[test,]))
actual <- as.matrix(ans[test, c('elapsed'), drop=FALSE])

corr <- cor(predicted, actual)
order.by.predicted <- order(predicted)

##  create a plot of predicted vs actual
plot(actual[order.by.predicted],
     pch=21, col="#aaaaaaaa", bg="#cc000030",
     ylab="running time in seconds", xlab="simulation")

title(main="predicted vs. actual",
      col.main="#666666")

lines(predicted[order.by.predicted],
      col='blue', lwd=2)

legend("topleft", pch=c(NA, 21), lwd=c(2,NA), 
       col=c("blue", "#aaaaaa"),
       pt.bg=c(NA,"#cc000030"),
       legend=c('predicted','actual'))

mtext('simulation running time', padj=-0.5)

legend("bottomright", legend=sprintf('corr=%0.3f', corr))


## plot running time vs matrix size
matrix_size <- ans$n*ans$p / 1e6
running_time <- ans$elapsed / 60
plot(matrix_size, running_time,
     pch=21, col="#aaaaaaaa", bg="#cc000030",
     ylab="running time (minutes)", xlab="matrix size (millions)",
     col.lab="#666666")

title(main="Simulation running time vs. matrix size",
      col.main="#666666")

mtext('10,000 simulations running on m3.xlarge EC2 instances', padj=-0.5,
      col="#999999")



