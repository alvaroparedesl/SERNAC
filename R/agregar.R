#' @title Agregar/resumir por variable(s)
#'
#' @description Agrupa reclamos por fecha, de forma mensual o semanal. Retorno los tiempos de respuesta entre reclamos, y el número de reclamos
#' contabilizados para esa agrupación temporal, con diferencias lagueadas definidas por el usuario. También agrupa por variable, dependiendo de
#' lo que defina el usuario.
#'
#' @param df data.table data.frame
#' @param bycols columnas que agruparán
#' @param timep tiempo para hacer la agrupación; mensual o semanal
#' @param prep entrega una tabla lista para ser apilada con otras, sin perder información.
#' Sólo si `setdiff(bycols, prepIgnore)` tiene largo igual a 1.
#' @param prepIgnore columna de agrupación que ignorar al preparar los datos
#' @param fillN cambia NAs por 0 (sólo en N, no en diff).
#' @param usar_tiempo si es TRUE, se usa la diferencia de tiempo como una de las variables para buscar valores atípicos. Por defecto es TRUE.
#'
#' @importFrom data.table setnames setcolorder
#' @return un data.frame de data.table ordenado por tiempo, para llegar y aplicar una serie temporal (filtrando previamente).
#'
#' @examples 1+1
agg_by <- function(df, bycols=c("proveedor_rut", "proveedor_mercado_nombre"),
                   timep="mensual", prep=FALSE, prepIgnore=NULL, fillN=TRUE, usar_tiempo=TRUE) {
  if (nrow(df) < 1){
    stop("Datos vacíos??")
  }
  tType <- list(semanal=c("%Y-%U", "-4", "%Y-%U-%u"),
                mensual=c("%Y-%m", "-15", "%Y-%m-%d"))[[timep]]
  tiempo <- list(mensual="months", semanal="weeks")
  df[, t:=strftime(caso_creacion_fecha, tType[1])]
  if (usar_tiempo) {
    df[, temp:=as.numeric(difftime(caso_cierre_fecha, caso_creacion_fecha, units="days"))] # TODO: definir estas columnas fuera
    tesub <- df[,
                list(# tmin=min(temp, na.rm=T),
                  # tmax=max(temp, na.rm=T),
                  tmean=mean(temp, na.rm=T),
                  # tmedian=median(temp, na.rm=T),
                  tsd=sd(temp, na.rm=T),
                  N=.N,
                  prop_acogidos = sum(reclamo_acogido, na.rm=T)/sum(!is.na(reclamo_acogido))
                  # log10N=log10(.N)
                ),
                by=c("t", bycols)]

    df[, c("t", "temp"):=NULL]
  } else {
    df[, temp:=as.numeric(difftime(caso_cierre_fecha, caso_creacion_fecha, units="days"))] # TODO: definir estas columnas fuera
    tesub <- df[,list(N=.N, prop_acogidos = sum(reclamo_acogido, na.rm=T)/sum(!is.na(reclamo_acogido))),
                by=c("t", bycols)]

    df[, c("t"):=NULL]
  }

  tesub[, t:=as.Date(paste0(t, tType[2]), tType[3])]
  #--------- rellenando fechas faltantes:
  minmax <- range(tesub$t, na.rm=T)
  uniq <- unique(tesub[, ..bycols])[,.(t=seq(minmax[1], minmax[2], tiempo[[timep]])), by=bycols]
  tesub <- merge(uniq, tesub, all.x=T, sort=F)
  if (fillN) tesub[is.na(N), N:=0]
  #-------------

  tesub <- tesub[order(t)] # NO MOVER ESTO
  # ojo que no estoy considerando la serie continua en su fecha
  # tesub[, c("diff1N", "diff1log10N") := list(c(0, diff(N)), c(0, diff(log10N))),  by=bycols]
  if (prep){
    # Prepara los datos para ser agrupados
    coln <- c(names(tesub))
    bycolsR <- bycols
    if (!is.null(prepIgnore)) {
      bycolsR <- setdiff(bycols, prepIgnore)
    }
    if (length(bycolsR) == 1) {
      tesub[, variable:=bycolsR]
      setnames(tesub, bycolsR, "variable_valor")
      pos <- which(coln == bycolsR)
      if (pos == 1) {
        setcolorder(tesub, c("variable", "variable_valor", coln[(pos+1):length(coln)]))
      } else {
        setcolorder(tesub, c(coln[1:(pos-1)], "variable", "variable_valor", coln[(pos+1):length(coln)]))
      }
    }
  }
  return(tesub)
}
