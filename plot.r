source("colors.r")

#
# OUTLINE
#

# every plot function in this file makes IMHO nice formatted plots in four steps

# 1. Plot an empty plot, to inform R about ranges of data (grid needs this)
# 2. Plot background and grid
# 3. Plot data
# 4. Plot axes, box and titles

################################################################################

# Helper function for leaving out unused margins
doMargins <- function(mainTitle, xTitle, yTitle) {
  margins <- par()$mar
    if(is.null(mainTitle)) {
        margins[3] <- 1.0
    }
    if(is.null(xTitle)) {
        margins[1] <- 2.5
    }
    if(is.null(yTitle)) {
        margins[2] <- 2.5
    }
    margins[4] <- 1.0
    par(mar=margins)
}

################################################################################

# Helper function for plotting axes and title on top of plot
doBoxTitleAndAxes <- function(mainTitle, xTitle, yTitle) {
  box(col = thillux_grey[1], bty="l")
  title(main=mainTitle, col=thillux_grey[1], xlab=xTitle, ylab=yTitle)
  axis(1, col="#00000000", col.axis = thillux_grey[1], col.ticks = thillux_grey[1])
  axis(2, col="#00000000", col.axis = thillux_grey[1], col.ticks = thillux_grey[1])
}

################################################################################

# Helper function for creating PDF devices
doOpenPDF <- function(pdfFile, pdfTitle) {
  pdfFilePath = "out.pdf"
  if(!is.null(pdfFile)) {
    pdfFilePath = pdfFile
  }
  pdf(pdfFilePath, pointsize=10, width=7, height=5, title = pdfTitle)
}

################################################################################

# Helper function for plotting background and grid, before plotting on top of it
doPlotBackgroundAndGrid <- function() {
  u <- par("usr")
  rect(u[1], u[3], u[2], u[4], col = bgColor, border = FALSE)
  par(col.lab=thillux_grey[1])
  grid(col=thillux_grey[1], lty=3, lwd=0.5)
}

################################################################################

plotHistogram <- function(dataArray, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot", breaks = 10) {
    doOpenPDF(pdfFile, pdfTitle)
    doMargins(mainTitle, xTitle, yTitle)

    h <- hist(dataArray, plot=FALSE, breaks=breaks)
    plot(h$mids, h$counts, ylim = c(0, max(h$counts)), xlim = c(min(h$mids) * 0.9, max(h$mids) * 1.1),
    type = 'n', bty = 'n', ann=FALSE, axes=FALSE)

    doPlotBackgroundAndGrid()

    hist(dataArray,
      add=TRUE,
      axes=FALSE,
      col=colorScheme[1,2],
      border=colorScheme[1,1],
      ylab='',
      xlab='',
      main='',
      breaks=breaks,
      lty=1,
      cex=1)

    doBoxTitleAndAxes(mainTitle, xTitle, yTitle)

    noOut <- dev.off()
}

########################################

plotHistogramNormal <- function(dataArray, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot", breaks = 10) {
    doOpenPDF(pdfFile, pdfTitle)
    doMargins(mainTitle, xTitle, yTitle)

    h <- hist(dataArray, plot=FALSE, breaks=breaks)
    h$counts <- h$counts/sum(h$counts)
    plot(h$mids, h$counts, ylim = c(0, max(h$counts)), xlim = c(min(h$mids) * 0.9, max(h$mids) * 1.1),
    type = 'n', bty = 'n', ann=FALSE, axes=FALSE)

    doPlotBackgroundAndGrid()

    hist(dataArray,
      add=TRUE,
      axes=FALSE,
      col=colorScheme[1,2],
      border=colorScheme[1,1],
      ylab='',
      xlab='',
      main='',
      breaks=breaks,
      freq=FALSE,
      lty=1,
      cex=1)

    x <- seq(min(-10.0, min(dataArray) * 0.5) , max(max(dataArray) * 1.5, 100), length=10000)
    y <- dnorm(x, mean=mean(dataArray), sd=sd(dataArray))

    par(new=TRUE)

    plot(x,y,
         type="l",
         lwd=3,
         axes=FALSE,
         col=colorScheme[2,2],
         xlim = c(min(h$mids) * 0.9, max(h$mids) * 1.1),
         ylab='',
         xlab='',
         main=''
    )

    polygon(x, y,
            col=colorScheme[2,2],
            border=colorScheme[2][1],
            ylab='',
            xlab='',
            main=''
    )

    doBoxTitleAndAxes(mainTitle, xTitle, yTitle)

    noOut <- dev.off()
}

########################################

plotQQNormal <- function(dataArray, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot") {
    doOpenPDF(pdfFile, pdfTitle)
    doMargins(mainTitle, xTitle, yTitle)

    plot(qqnorm(dataArray, plot.it=FALSE, ylab='',
      xlab=''), ann=FALSE, type="n", bty="n", axes=FALSE, ylab='',
      xlab='',
      main='');

    doPlotBackgroundAndGrid()

    qqline(sort(dataArray), ylab='',
      xlab='', col=colorScheme[2,2], lwd=2)

    par(new=TRUE)

    qq <- qqnorm(sort(dataArray), plot.it=FALSE,ylab='',
      xlab='')

    plot(qq$x, qq$y,
      type="b",
      axes=FALSE,
      col=colorScheme[1,2],
      bg=colorScheme[1,1],
      ylab='',
      xlab='',
      main=NULL,
      lty=1,
      pch=21,
      cex=1,
      lwd=1)

    doBoxTitleAndAxes(mainTitle, xTitle, yTitle)

    noOut <- dev.off()
}

########################################

plotQQ <- function(dataArray1, dataArray2, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot") {
    doOpenPDF(pdfFile, pdfTitle)
    doMargins(mainTitle, xTitle, yTitle)

    plot(qqplot(dataArray1, dataArray2, plot.it=FALSE, ylab='',
      xlab=''), ann=FALSE, type="n", bty="n", axes=FALSE, ylab='',
      xlab='',
      main='');

    doPlotBackgroundAndGrid()

    par(new=TRUE)

    qq <- qqplot(sort(dataArray1), sort(dataArray2), plot.it=FALSE,ylab='',
      xlab='')

    plot(qq$x, qq$y,
      type="b",
      axes=FALSE,
      col=colorScheme[1,2],
      bg=colorScheme[1,1],
      ylab='',
      xlab='',
      main=NULL,
      lty=1,
      pch=21,
      cex=1,
      lwd=1)

    doBoxTitleAndAxes(mainTitle, xTitle, yTitle)

    noOut <- dev.off()
}

########################################

plotPoints <- function(dataArray1, dataArray2, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot") {
    doOpenPDF(pdfFile, pdfTitle)
    doMargins(mainTitle, xTitle, yTitle)

    plot(dataArray1, dataArray2, ann=FALSE, type="n", bty="n", axes=FALSE, ylab='',
      xlab='',
      main='')

    doPlotBackgroundAndGrid()

    par(new=TRUE)

    plot(dataArray1, dataArray2,
      type="p",
      axes=FALSE,
      col=colorScheme[1,2],
      bg=colorScheme[1,1],
      ylab='',
      xlab='',
      main=NULL,
      lty=1,
      pch=21,
      cex=1,
      lwd=1)

    doBoxTitleAndAxes(mainTitle, xTitle, yTitle)

    noOut <- dev.off()
}

########################################

plotSmoothLine <- function(dataArray1, dataArray2, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot") {
    doOpenPDF(pdfFile, pdfTitle)
    doMargins(mainTitle, xTitle, yTitle)

    plot(dataArray1, dataArray2, ann=FALSE, type="n", bty="n", axes=FALSE, ylab='',
      xlab='',
      main='')

    doPlotBackgroundAndGrid()

    par(new=TRUE)

    plot(smooth.spline(dataArray1, dataArray2),
      type="l",
      axes=FALSE,
      col=colorScheme[1,2],
      bg=colorScheme[1,1],
      ylab='',
      xlab='',
      main=NULL,
      lty=1,
      pch=21,
      cex=1,
      lwd=1)

    doBoxTitleAndAxes(mainTitle, xTitle, yTitle)

    noOut <- dev.off()
}

################################################################################

plotWithConfidence <- function(xData, yData, e, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot", connectionLines=FALSE) {
    doOpenPDF(pdfFile, pdfTitle)
    doMargins(mainTitle, xTitle, yTitle)

    deltaX <- abs(diff(range(xData))) / 15.0
    deltaY <- abs(diff(range(c(yData+e,yData-e)))) / 15.0
    xLim <- range(xData) + c(-deltaX, deltaX)
    yLim <- range(c(yData+e,yData-e)) + c(-deltaY, deltaY)

    plot(xData, yData, ann=FALSE, type="n", bty="n", axes=FALSE, ylab='',
    xlab='',
    main='',
    xlim=xLim,
    ylim=yLim)

    doPlotBackgroundAndGrid()

    par(new=TRUE)

    arrowLength <- abs(diff(range(xData)))/25.0
    arrowTipLength <- abs(diff(range(yData)))/50.0

    rect(xData - arrowLength, yData - e, xData + arrowLength, yData + e, col=colorScheme[1,2], border=FALSE)

    segments(xData - arrowLength, yData - e, xData + arrowLength, yData - e, lend=1, col=colorScheme[1,1])
    segments(xData - arrowLength, yData + e, xData + arrowLength, yData + e, lend=1, col=colorScheme[1,1])

    segments(xData - arrowLength/2.0, yData, xData+arrowLength/2.0, yData, col=colorScheme[1,1], lwd = 1.0)

    if (connectionLines) {
      frame <- data.frame(xData,yData)
      frame <- frame[order(xData),]
      lines(frame$xData, frame$yData, col=colorScheme[1,2],lty=2)
    }

    # draw circle outlines
    rVert <- pmin(e/2.0, arrowTipLength)
    rHor <- arrowLength/5.0

    for (i in 1 : length(xData)) {
      mid1X <- xData[i] - arrowLength
      mid1Y <- yData[i] + e[i] - rVert[i]

      mid2X <- xData[i] - arrowLength
      mid2Y <- yData[i] - e[i] + rVert[i]

      mid3X <- xData[i] + arrowLength
      mid3Y <- yData[i] + e[i] - rVert[i]

      mid4X <- xData[i] + arrowLength
      mid4Y <- yData[i] - e[i] + rVert[i]

      deg <- 90 : 180
      xCircle1 <- mid1X + rHor * cos(deg/180.0 * pi)
      yCircle1 <- mid1Y + rVert[i] * sin(deg/180.0 * pi)
      lines(xCircle1, yCircle1, col=colorScheme[1,1])

      deg <- 180 : 270
      xCircle2 <- mid2X + rHor * cos(deg/180.0 * pi)
      yCircle2 <- mid2Y + rVert[i] * sin(deg/180.0 * pi)
      lines(xCircle2, yCircle2, col=colorScheme[1,1])

      polygon(c(xCircle1, xCircle2), c(yCircle1, yCircle2), col = colorScheme[1,2], border = FALSE)

      deg <- 0 : 90
      xCircle3 <- mid3X + rHor * cos(deg/180.0 * pi)
      yCircle3 <- mid3Y + rVert[i] * sin(deg/180.0 * pi)
      lines(xCircle3, yCircle3, col=colorScheme[1,1])

      deg <- 270 : 360
      xCircle4 <- mid4X + rHor * cos(deg/180.0 * pi)
      yCircle4 <- mid4Y + rVert[i] * sin(deg/180.0 * pi)
      lines(xCircle4, yCircle4, col=colorScheme[1,1])

      polygon(c(xCircle3, xCircle4), c(yCircle3, yCircle4), col = colorScheme[1,2], border = FALSE)
    }

    doBoxTitleAndAxes(mainTitle, xTitle, yTitle)

    noOut <- dev.off()
}

################################################################################

plotWithConfidenceContinous <- function(xData, yData, e, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot", connectionLines=FALSE) {
    doOpenPDF(pdfFile, pdfTitle)
    doMargins(mainTitle, xTitle, yTitle)

    deltaX <- abs(diff(range(xData))) / 15.0
    deltaY <- abs(diff(range(c(yData+e,yData-e)))) / 15.0
    xLim <- range(xData) + c(-deltaX, deltaX)
    yLim <- range(c(yData+e,yData-e)) + c(-deltaY, deltaY)

    plot(xData, yData, ann=FALSE, type="n", bty="n", axes=FALSE, ylab='',
    xlab='',
    main='',
    xlim=xLim,
    ylim=yLim)

    doPlotBackgroundAndGrid()

    par(new=TRUE)

    polygon(c(sort(xData),rev(sort(xData))),c(yData + e, rev(yData-e)), col=colorScheme[1,2], border=FALSE)

    lines(xData, yData + e, col=colorScheme[1,1])
    lines(xData, yData - e, col=colorScheme[1,1])

    lines(xData,yData,col=colorScheme[2,1])

    doBoxTitleAndAxes(mainTitle, xTitle, yTitle)

    noOut <- dev.off()
}

################################################################################

plotBox <- function(xData, yData, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot", breaks=10) {
    doOpenPDF(pdfFile, pdfTitle)
    doMargins(mainTitle, xTitle, yTitle)

    plot(cut(xData, breaks=breaks), yData, ann=FALSE, type="n", bty="n", axes=FALSE, ylab='',
      xlab='',
      col="#00000000",
      border="#00000000",
      bg="#00000000",
      main='')

    doPlotBackgroundAndGrid()

    par(new=TRUE)

    plot(cut(xData, breaks=breaks), yData,
      type="l",
      axes=FALSE,
      col=thillux_grey[2],
      bg=colorScheme[1,2],
      border=colorScheme[1,1],
      ylab='',
      xlab='',
      main=NULL,
      lty=1,
      pch=21,
      cex=1,
      lwd=1)

    doBoxTitleAndAxes(mainTitle, xTitle, yTitle)

    noOut <- dev.off()
}

################################################################################

plotDensity <- function(xData, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot", connectionLines=FALSE) {
    doOpenPDF(pdfFile, pdfTitle)
    doMargins(mainTitle, xTitle, yTitle)

    d <- density(xData)

    plot(d, ann=FALSE, type="n", bty="n", axes=FALSE, ylab='',
    xlab='',
    main='')

    doPlotBackgroundAndGrid()

    par(new=TRUE)

    plot(d$x, d$y, type="l", col=colorScheme[1,1])

    polygon(d$x, d$y, col=colorScheme[1,2], border=FALSE)

    doBoxTitleAndAxes(mainTitle, xTitle, yTitle)

    noOut <- dev.off()
}

################################################################################
