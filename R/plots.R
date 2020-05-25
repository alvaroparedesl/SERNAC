#' @title Gráfico de la serie
#'
#' @param db objeto retornado por \link{computar_reclamos}.
#' @param t_observado periodo observado
#' @param pos posición del ranking a observar
#' @param ascendente se muestren de menor a mayor?
#' @param clase nombre de la clase que se desa visualziar. Debe corresponder al nombre asignado a `byClase` en \link{computar_reclamos}.
#' @param metrics métricas a visualizar. Si no se pasa argumento, se usarán todas.
#'
#' @return
#' @export
#'
#' @examples 1+1
plotme <- function(db, t_observado='min', pos=1, ascendente=TRUE, clase="industria", metrics=NULL){

  clasei <- clase
  mdat <- db$serie[posicion==pos & up==ascendente & clase==clasei]
  if (t_observado %in% c("min", "max")) {
    mt <- sort(unique(mdat$t_observado))
    mt <- switch(t_observado,
                 "min"=min(mt),
                 "max"=max(mt))
  } else {
    mt <- as.Date(t_observado)
  }
  mdat <- mdat[t_observado==mt]

  if (is.null(metrics)) {
    metricsi <- unique(mdat$metrics)
  } else {
    metricsi <- metrics
  }
  mdat <- mdat[metrics %in% metricsi]
  cols <- names(mdat)[1:which(names(mdat) == "variable")]

  par(mfrow=c(length(metricsi),1))
  for (i in metricsi) {
    temp <- mdat[metrics == i]
    if (nrow(temp) > 0) {
      with(temp, plot(values~t, type="l", ylab="Valor", xlab="Tiempo", las=1,
                      main=i))
      # main=paste0(i, " | ", paste0(mainn, collapse="-"))))
    } else {
      warning(paste0("No hay datos disponibles para la metrica '", i))
    }
  }
  return(unique(mdat[, ..cols]))
}
