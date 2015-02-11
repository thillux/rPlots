#!/usr/bin/Rscript

source("plot.r")

doConfidence <- function() {
    numPoints <- 10

    x <- rnorm(numPoints)
    y <- rnorm(numPoints)

    plotWithConfidence(x, y, abs(y/10.0), pdfFile="conf.pdf")
}

doBoxPlot <- function() {
    numPoints <- 10000

    x <- rnorm(numPoints)
    y <- rnorm(numPoints)

    plotBox(x, y, pdfFile="box.pdf")
}

doConfidence()
doBoxPlot()
