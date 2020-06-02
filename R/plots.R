#' @encoding UTF-8
#' @title Gráfico de la serie
#'
#' @param dat objeto retornado por \link{computar_reclamos}.
#' @param t_observado periodo observado
#' @param pos posición del ranking a observar
#' @param ascendente se muestren de menor a mayor?
#' @param clase nombre de la clase que se desa visualziar. Debe corresponder al nombre asignado a `byClase` en \link{computar_reclamos}.
#' @param metrics métricas a visualizar. Si no se pasa argumento, se usarán todas.
#'
#' @importFrom graphics par
#' @export
#'
#' @examples 1+1
plot.reclamos <- function(dat, t_observado='min', pos=1, ascendente=NULL, clase="industria", metrics=NULL, ...){
  db <- dat$reporte
  if (!"reclamos" %in% class(dat)) stop("Objeto db debe ser de la clase reclamos")
  if (!dat$estado$reporte) stop("Primero debe extraer la serie con reportar")

  clasei <- clase
  if (is.null(ascendente)) {
    up_ <-  c(TRUE, FALSE)
  } else {
    up_ <- ascendente
  }

  mdat <- db$serie[posicion==pos & up %in% up_ & clase==clasei]
  if (t_observado %in% c("min", "max")) {
    mt <- sort(unique(mdat$t_observado))
    mt <- switch(t_observado,
                 "min"=min(mt),
                 "max"=max(mt))
  } else {
    mt <- as.Date(t_observado)
  }
  mdat <- mdat[t_observado==mt]
  # outt$reporte$serie[posicion==1 & up==T & clase=="industria" & t_observado == as.Date("2019-12-15")]

  if (is.null(metrics)) {
    metricsi <- unique(mdat$metrics)
  } else {
    metricsi <- metrics
  }
  mdat <- mdat[metrics %in% metricsi]
  cols <- names(mdat)[1:which(names(mdat) == "variable")]

  n2plot <- nrow(unique(mdat[, c("metrics", "up")]))
  dev.new(noRStudioGD = T)
  pcol <- ifelse(n2plot > 6, 2, 1)
  prow <- ifelse(n2plot > 6, ceiling(n2plot/2), n2plot)
  par(mfcol=c(prow, pcol))
  cter <- 0
  for (i in 1:length(metricsi)) {
    temp <- mdat[metrics == metricsi[i]]
    if (nrow(temp) > 0) {
      for (ups in up_) {
        temp2 <- temp[up %in% ups]
        if (nrow(temp2) > 0) {
          cter <- cter + 1
          par(mar = c(2, 4.1, 2, 2)); xlabt = ""
          if (cter %in% c(prow, n2plot)) {par(mar = c(5, 4.1, 2, 2)); xlabt <- "Tiempo"}
          last_line_time <- c(nrow(temp2)-1, nrow(temp2))
          with(temp2, plot(values~t, type="l", ylab="Valor", xlab=xlabt, las=1, lwd=2, bty="l", col="#878787"))
          with(temp2[last_line_time, ], lines(values~t, col=ifelse(up, "#d73027", "#4575b4"), lwd=3))
          mtext(paste(metricsi[i], unique(temp2$variable), unique(temp2$variable_valor), sep=" | "), side=3, line=-1, las=1, cex=.8)
          abline(with(temp2, lm(values~t)), col="#006837", lty=3, lwd=2)
        }
      }
    } else {
      warning(paste0("No hay datos disponibles para la metrica '", i))
    }
  }
  return(mdat[, list(N=.N), cols])
}
