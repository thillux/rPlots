#!/usr/bin/Rscript

source("plot.r")

doConfidence <- function() {
    numPoints <- 10

    x <- rnorm(numPoints)
    y <- rnorm(numPoints)

    plotWithConfidence(x, y, abs(y/10.0), pdfFile="conf.pdf")
}

doConfidenceCategorial <- function() {
    x <- c("Bier", "Mate", "water", "Weizen")
    y <- rnorm(length(x))

    plotWithConfidence(x, y, abs(y/10.0), pdfFile="confCategorial.pdf", xDataIsCategorial=TRUE)
}

doConfidenceCategorialFromFile <- function(name) {
    a <- read.table(name)
    plotWithConfidence(t(a[1]), t(a[2]), t(a[3]), pdfFile="confCategorial2.pdf", xDataIsCategorial=TRUE)
}

doBoxPlot <- function() {
    numPoints <- 10000

    x <- rnorm(numPoints)
    y <- rnorm(numPoints)

    plotBoxes(x, y, pdfFile="box.pdf")
}

doConfidenceContinous <- function() {
    x1 <- seq(0,10,0.01)
    x <- x1
    for (i in 1:31) {
        x <- c(x,x1)
    }

    numPoints <- length(x)

    y <- x*x + rnorm(numPoints)

    outMean <- tapply(y, x, mean)
    outSD <- tapply(y, x, sd)
    out <- data.frame(x = names(outMean), mean = outMean, sd = outSD, row.names = NULL)

    confLevel <- 0.99
    numReplications <- 32

    plotWithConfidenceContinous(x1, out$mean, 10.0 * qnorm(1.0 - (1.0 - confLevel)/2.0) * out$sd/sqrt(numReplications), pdfFile = "confContinous.pdf")
}

doBoxPlot()
doConfidence()
doConfidenceCategorial()
doConfidenceContinous()
doConfidenceCategorialFromFile("result")
