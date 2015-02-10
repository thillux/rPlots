#!/usr/bin/Rscript

source("plot.r")

numPoints <- 10

x <- rnorm(numPoints)
y <- rnorm(numPoints)

plotWithConfidence(x, y, abs(y/10.0))
