#
#  Generating data under the null hypothesis.
#
generateFromMeanShiftData <- function(data,
                                      size = length(data),
                                      mu = 0) {
  dataWithMeanMu <- data - mean(data) + mu
  quantile(dataWithMeanMu, probs = runif(size))
}
# First we need a function to generate new x and y at once
generateXYcommonMean <- function(data){
  # assume data is a list containing x and y
  x <- data$x
  y <- data$y
  # return new data as a list of x and y
  newx <- generateFromMeanShiftData(x)
  newy <- generateFromMeanShiftData(y)
  list(x = newx, y = newy)
}

mixRandomly <- function(data){
  # assume data is a list containing x and y
  x <- data$x
  y <- data$y
  m <- length(x)
  n <- length(y)
  z <- c(x,y)
  xIndices <- sample(1:(m+n), m, replace = FALSE)
  newx <- z[xIndices]
  newy <- z[-xIndices] # the remainder
  list(x = newx, y = newy)
}

mixCoords <- function(data) {
  # Note that data needs to have named x and a y components
  x <- data$x
  y <- data$y
  n <- length(x)
  stopifnot(n == length(y))
  new_y <- sample(y, n, replace=FALSE)
  list(x=x, y=new_y)
}