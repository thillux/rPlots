source("colors.r")

########################################

plotHistogram <- function(dataArray, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot", breaks = 10) {
    pdfFilePath <- "hist.pdf"
    if(!is.null(pdfFile)) {
        pdfFilePath <- pdfFile
    }
    pdf(pdfFilePath, pointsize=10, width=7, height=5, title = pdfTitle)

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

    h <- hist(dataArray, plot=FALSE, breaks=breaks)
    plot(h$mids, h$counts, ylim = c(0, max(h$counts)), xlim = c(min(h$mids) * 0.9, max(h$mids) * 1.1),
    type = 'n', bty = 'n', ann=FALSE, axes=FALSE)

    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col = bgColor, border = FALSE)

    par(col.lab=thillux_grey)

    grid(col=thillux_grey, lty=3, lwd=0.5)

    hist(dataArray,
      add=TRUE,
      axes=FALSE,
      col=color,
      border=borderColor,
      ylab='',
      xlab='',
      main='',
      breaks=breaks,
      lty=1,
      cex=1)
    box(col = thillux_grey, bty="l")
    title(main=mainTitle, col=thillux_grey, xlab=xTitle, ylab=yTitle)
    axis(1, col="#00000000", col.axis = thillux_grey, col.ticks = thillux_grey)
    axis(2, col="#00000000", col.axis = thillux_grey, col.ticks = thillux_grey)

    noOut <- dev.off()
}

########################################

plotHistogramNormal <- function(dataArray, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot", breaks = 10) {
    pdfFilePath <- "hist.pdf"
    if(!is.null(pdfFile)) {
        pdfFilePath <- pdfFile
    }
    pdf(pdfFilePath, pointsize=10, width=7, height=5, title = pdfTitle)

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

    h <- hist(dataArray, plot=FALSE, breaks=breaks)
    h$counts <- h$counts/sum(h$counts)
    plot(h$mids, h$counts, ylim = c(0, max(h$counts)), xlim = c(min(h$mids) * 0.9, max(h$mids) * 1.1),
    type = 'n', bty = 'n', ann=FALSE, axes=FALSE)

    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col = bgColor, border = FALSE)

    par(col.lab=thillux_grey)

    grid(col=thillux_grey, lty=3, lwd=0.5)

    hist(dataArray,
      add=TRUE,
      axes=FALSE,
      col=color,
      border=borderColor,
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
         col=thillux_green,
         xlim = c(min(h$mids) * 0.9, max(h$mids) * 1.1),
         ylab='',
         xlab='',
         main=''
    )

    polygon(x, y,
            col=thillux_green,
            border="#4ACAA8",
            ylab='',
            xlab='',
            main=''
    )

    box(col = thillux_grey, bty="l")
    title(main=mainTitle, col=thillux_grey, xlab=xTitle, ylab=yTitle)
    axis(1, col="#00000000", col.axis = thillux_grey, col.ticks = thillux_grey)
    axis(2, col="#00000000", col.axis = thillux_grey, col.ticks = thillux_grey)

    noOut <- dev.off()
}

########################################

plotQQNormal <- function(dataArray, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot") {
    pdfFilePath = "hist.pdf"
    if(!is.null(pdfFile)) {
        pdfFilePath = pdfFile
    }
    pdf(pdfFilePath, pointsize=10, width=7, height=5, title = pdfTitle)

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

    plot(qqnorm(dataArray, plot.it=FALSE, ylab='',
      xlab=''), ann=FALSE, type="n", bty="n", axes=FALSE, ylab='',
      xlab='',
      main='');

    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col = bgColor, border = FALSE)

    par(col.lab=thillux_grey)

    grid(col=thillux_grey, lty=3, lwd=0.5)

    qqline(sort(dataArray), ylab='',
      xlab='', col=thillux_green, lwd=2)

    par(new=TRUE)

    qq <- qqnorm(sort(dataArray), plot.it=FALSE,ylab='',
      xlab='')

    plot(qq$x, qq$y,
      type="b",
      axes=FALSE,
      col=borderColor,
      bg=color,
      ylab='',
      xlab='',
      main=NULL,
      lty=1,
      pch=21,
      cex=1,
      lwd=1)

    box(col = thillux_grey, bty="l")
    title(main=mainTitle, col=thillux_grey, xlab=xTitle, ylab=yTitle)
    axis(1, col="#00000000", col.axis = thillux_grey, col.ticks = thillux_grey)
    axis(2, col="#00000000", col.axis = thillux_grey, col.ticks = thillux_grey)

    noOut <- dev.off()
}

########################################

plotQQ <- function(dataArray1, dataArray2, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot") {
    pdfFilePath = "hist.pdf"
    if(!is.null(pdfFile)) {
        pdfFilePath = pdfFile
    }
    pdf(pdfFilePath, pointsize=10, width=7, height=5, title = pdfTitle)

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

    plot(qqplot(dataArray1, dataArray2, plot.it=FALSE, ylab='',
      xlab=''), ann=FALSE, type="n", bty="n", axes=FALSE, ylab='',
      xlab='',
      main='');

    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col = bgColor, border = FALSE)

    par(col.lab=thillux_grey)

    grid(col=thillux_grey, lty=3, lwd=0.5)

    par(new=TRUE)

    qq <- qqplot(sort(dataArray1), sort(dataArray2), plot.it=FALSE,ylab='',
      xlab='')

    plot(qq$x, qq$y,
      type="b",
      axes=FALSE,
      col=borderColor,
      bg=color,
      ylab='',
      xlab='',
      main=NULL,
      lty=1,
      pch=21,
      cex=1,
      lwd=1)

    box(col = thillux_grey, bty="l")
    title(main=mainTitle, col=thillux_grey, xlab=xTitle, ylab=yTitle)
    axis(1, col="#00000000", col.axis = thillux_grey, col.ticks = thillux_grey)
    axis(2, col="#00000000", col.axis = thillux_grey, col.ticks = thillux_grey)

    noOut <- dev.off()
}

########################################

plotPoints <- function(dataArray1, dataArray2, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot") {
    pdfFilePath = "hist.pdf"
    if(!is.null(pdfFile)) {
        pdfFilePath = pdfFile
    }
    pdf(pdfFilePath, pointsize=10, width=7, height=5, title = pdfTitle)

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

    plot(dataArray1, dataArray2, ann=FALSE, type="n", bty="n", axes=FALSE, ylab='',
      xlab='',
      main='')

    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col = bgColor, border = FALSE)

    par(col.lab=thillux_grey)

    grid(col=thillux_grey, lty=3, lwd=0.5)

    par(new=TRUE)

    plot(dataArray1, dataArray2,
      type="p",
      axes=FALSE,
      col=borderColor,
      bg=color,
      ylab='',
      xlab='',
      main=NULL,
      lty=1,
      pch=21,
      cex=1,
      lwd=1)

    box(col = thillux_grey, bty="l")
    title(main=mainTitle, col=thillux_grey, xlab=xTitle, ylab=yTitle)
    axis(1, col="#00000000", col.axis = thillux_grey, col.ticks = thillux_grey)
    axis(2, col="#00000000", col.axis = thillux_grey, col.ticks = thillux_grey)

    noOut <- dev.off()
}

########################################

plotSmoothLine <- function(dataArray1, dataArray2, mainTitle=NULL, xTitle=NULL, yTitle=NULL, pdfFile=NULL, pdfTitle="thillux plot") {
    pdfFilePath = "hist.pdf"
    if(!is.null(pdfFile)) {
        pdfFilePath = pdfFile
    }
    pdf(pdfFilePath, pointsize=10, width=7, height=5, title = pdfTitle)

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

    plot(dataArray1, dataArray2, ann=FALSE, type="n", bty="n", axes=FALSE, ylab='',
      xlab='',
      main='')

    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col = bgColor, border = FALSE)

    par(col.lab=thillux_grey)

    grid(col=thillux_grey, lty=3, lwd=0.5)

    par(new=TRUE)

    plot(smooth.spline(dataArray1, dataArray2),
      type="l",
      axes=FALSE,
      col=borderColor,
      bg=color,
      ylab='',
      xlab='',
      main=NULL,
      lty=1,
      pch=21,
      cex=1,
      lwd=1)

    box(col = thillux_grey, bty="l")
    title(main=mainTitle, col=thillux_grey, xlab=xTitle, ylab=yTitle)
    axis(1, col="#00000000", col.axis = thillux_grey, col.ticks = thillux_grey)
    axis(2, col="#00000000", col.axis = thillux_grey, col.ticks = thillux_grey)

    noOut <- dev.off()
}
