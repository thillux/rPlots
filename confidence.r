#!/usr/bin/Rscript
# arg1 = input, arg2 = output, arg3 = xTitle, arg4 = yTitle
# input format:
# name average confidence

source("plot.r")
args <- commandArgs(TRUE)

doConfidenceCategorialFromFile <- function(name) {
    a <- read.table(name)
    plotWithConfidence(t(a[1]), t(a[2]), t(a[3]), pdfFile=args[2], xDataIsCategorial=TRUE, xTitle=args[3], yTitle=args[4])
}
doConfidenceCategorialFromFile(args[1])
