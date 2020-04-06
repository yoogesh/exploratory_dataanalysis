#
# Numerical tests
#
numericalTest <- function(data,
                          nReps = 2000,
                          discrepancyFn,
                          generateFn,
                          returnStats = FALSE){

  dObserved <- discrepancyFn(data)
  discrepancies <- sapply(1:nReps,
                          FUN = function(i){
                            newData <- generateFn(data)
                            discrepancyFn(newData)
                          }
  )
  result <-  mean(discrepancies >= dObserved)
  if (returnStats){
    result <- list(p = result, dobs = dObserved, d = discrepancies)}
  result
}


numericalTestXY <- function(x, y = NULL,
                            nReps = 2000,
                            discrepancyFn = discrepancyMeans,
                            generateXY = generateXYcommonMean,
                            returnStats = FALSE){
  data <-  if(is.null(y)) {
    list(x = x[[1]], y = x[[2]])
  } else {
    list(x = x, y = y)}
  numericalTest(data = data,
                nReps = nReps,
                discrepancyFn = discrepancyFn,
                generateFn = generateXY,
                returnStats = returnStats)
}


discrepancyMeans <- function(data) {
  # assume data is a list of named components x and y
  x <- data$x
  y <- data$y
  abs(mean(x) - mean(y))
}

discrepancyAbsAve <- function(data) {abs(mean(data))}

ks.discrep <- function(data) {
  ks.test(data$x,data$y)$statistic
}

discrepancyMedians <- function(data){abs(median(data$x) - median(data$y))}

t.stat <- function(data){abs(t.test(x = data$x, y = data$y)$statistic)}

paired.t.stat <- function(data){
  t.test(x = data$x, y = data$y, paired = TRUE)$statistic
}


correlationDiscrepancy <- function(data) {
  # Note that data needs to have named x and a y components
  x <- data$x
  y <- data$y
  abs(cor(x,y))
}


slopeDiscrepancy <- function(data) {
  # Note that data needs to have named x and a y components
  fit <- lm(y ~ x, data = data)
  abs(fit$coef[2])
}

loessDiscrepancy <- function(data){
  fit <- loess(y ~ x, data = data,   # default span
               family = "symmetric") # robust family
  sd(fit$fitted)/sd(data$y) # proportion of sd of ys explained by mu
}
