# datosPlots, comunas, comunas_poblacion, SII_sizes
plotSeries <- function(dataframe, xs, ys, logy=FALSE, principal=0, legend=F, legendx=0, ...) {
  if (is.null(ys)){
    ys <- names(dataframe)[which(names(dataframe) != xs)]
  }
  n <- length(ys)
  ne <- n - (principal != 0)
  y <- as.matrix(dataframe[, ..ys])
  if (logy) y <- log10(y)
  x <- dataframe[[xs]]
  x2 <- as.numeric(x)
  
  ylim <- range(y, na.rm=T)
  xlim <- as.Date(range(x, na.rm=T))
  colores <- colorRampPalette(c("#b2182b", "#d9ef8b", "#2166ac"))(ne)
  yaxt <- switch(logy, "n", "s")
  
  if (principal==0){
    plot(NA, xlim=xlim, ylim=ylim, xaxt="n", yaxt=yaxt, bty="l", las=1, lwd=2, ...)
    ysn <- ys
  } else {
    plot(x2, y[, ys[principal]], xlim=xlim, ylim=ylim, xaxt="n", yaxt=yaxt, bty="l", las=1, lwd=2, ...)
    ysn <- ys[-which(ys == ys[principal])]
  }
  
  xwhere <- as.numeric(strftime(x, "%m")) %in% 1
  xlabel <- strftime(x[xwhere], "%Y")
  abline(v=x2[xwhere], col="gray80")
  
  for (i in 1:ne) {
    lines(x2, y[, ysn[i]], col=colores[i], lwd=2)
  }
  axis(1, x2[xwhere], xlabel)
  if (logy) {
    axis(2, 0:max(y), 10^(0:max(y)), las=2)
  }
  if (legend) {
    legend('right', legend=ys, col=colores, lwd=2, bty="n", 
           cex=.8, horiz=F, xpd=T, inset=c(legendx, 0))
  }
}