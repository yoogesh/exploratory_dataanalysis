#
# Lineup tests
#
lineup <- function(data,
                   showSubject=NULL, generateSubject=NULL,
                   trueLoc=NULL, layout =c(5,4)) {
  # Get the number of subjects in total
  nSubjects <- layout[1] * layout[2]
  if (is.null(trueLoc))
  {trueLoc <- sample(1:nSubjects, 1)}
  if (is.null(showSubject))
  {stop("need a plot function for the subject")}
  if (is.null(generateSubject))
  {stop("need a function to generate subject")}

  # Need to decide which subject to present
  presentSubject <- function(subjectNo) {
    if(subjectNo != trueLoc) {data <- generateSubject(data)}
    showSubject(data, subjectNo) }

  # This does the plotting
  savePar <- par(mfrow=layout,
                 mar=c(2.5, 0.1, 3, 0.1), oma=rep(0,4))
  sapply(1:nSubjects, FUN = presentSubject)
  par(savePar)
  # hide the true location but return information to reconstruct it.
  hideLocation(trueLoc, nSubjects)
}

hideLocation <- function(trueLoc, nSubjects){
  possibleBaseVals <- 3:min(2*nSubjects, 50)
  # remove easy base values
  possibleBaseVals <- possibleBaseVals[possibleBaseVals != 10 &
                                         possibleBaseVals != 5]
  base <- sample(possibleBaseVals, 1)
  offset <- sample(5:min(5*nSubjects, 125),1)
  # return location information (trueLoc hidden)
  list(trueLoc = paste0("log(",
                        base^(trueLoc + offset),
                        ", base=", base,
                        ") - ", offset))
}

revealLocation <- function(hideLocation){
  eval(parse(text = hideLocation$trueLoc)) # parse and evaluate the text
}


compareBoxplots <- function(data, subjectNo) {
  y <- data$y  # assume data is a list of x and y
  x <- data$x
  group <- factor(c(rep("x", length(x)), rep("y", length(y))))
  # create data for boxplots
  bp_data <- data.frame(vals = c(x,y), group = group)
  boxplot(vals ~ group, data = bp_data,
          main = paste(subjectNo), cex.main = 4,
          xlab = "", ylab = "",
          col = adjustcolor(c("firebrick", "steelblue"), 0.5),
          xaxt = "n", yaxt = "n" # turn off axes
  )
}

compareQuantiles <- function(data, subjectNo) {
  y <- sort(data$y)  # assume data is a list of x and y
  x <- sort(data$x)
  ylim <- extendrange(c(x,y))
  n <- length(y)
  m <- length(x)
  py <- ppoints(n)
  px <- ppoints(m)
  plot(px, x, ylim = ylim, xlim = c(0,1),
       main = paste(subjectNo), cex.main = 4,
       xlab = "", ylab = "",
       col = adjustcolor("firebrick", 0.75),
       pch = 17, cex = 4,
       xaxt = "n", yaxt = "n" # turn off axes
  )
  points(py, y,
         col = adjustcolor("steelblue", 0.75),
         pch = 19, cex = 4)
}

showScatter <- function(data, subjectNo) {
  plot(y ~ x, data = data,
       main = paste(subjectNo), cex.main = 4,
       xlab = "", ylab = "", pch = 19, cex = 2,
       col = adjustcolor("steelblue", 0.75),
       xaxt = "n", yaxt = "n" # turn off axes
  )
}

library(MASS)
showDensityContours <- function(data, subjectNo){
  den <- kde2d(x = data$x, y = data$y, n=100)
  zlim <- range(den$z)
  contour(den$x, den$y, den$z,
          levels = pretty(zlim, 7), lwd=1,
          drawlabels = FALSE,
          main = paste(subjectNo), cex.main = 4,
          xlab = "", ylab = "",
          col = "steelblue",
          xaxt = "n", yaxt = "n" # turn off axes
  )
}
