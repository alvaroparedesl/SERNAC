#' @title Calcular métricas
#'
#' @param x vector númerico.
#' @param cuantiles vector númerico con los cuantiles deseados (valor entre 0 y 1).
#' @param IQR vector con el rango intercuantil de la forma c(min, max). Ambos valores deben estar en `cuantiles`.
#' @param coef coeficiente de dispersión (de castigo), usualmente 1.5.
#'
#' @return
#' @export
#'
calcular_metricas <- function(x, cuantiles, IQR, coef) {
  # calcular_metricas <- function(x, cuantiles=c(0, .25, .5, .75, 1), IQR=c(.25, .75), coef=1.5) {
  # de la función boxplot.stats: sin asumir normalidad, usando cuantiles 8 como es recomendado.
  if (!cuantiles %in% IQR) stop("Los valores de IQR deben estar contenidos en la variable cuantiles.")
  qs <- quantile(x, cuantiles, type=8, na.rm=T)
  ans <- c(qs, sd=sd(x, na.rm=T), mean=mean(x, na.rm=T), N=sum(!is.na(x)))
  qtls <- ans[which(cuantiles %in% IQR)]
  iqr <- diff(qtls)
  c(ans, qtls + c(-1, 1) * coef * iqr)
}


#' @title Marca outliers
#'
#' @param x vector númerico
#' @param t_min límite inferior
#' @param t_max límite superior
#' @param to_data_table si es TRUE, transforma el ooutput a una data.table dataframe. Caso contrario, retorna una lista. Por defecto es TRUE.
#'
#' @return
#' @export
#'
get_outliers <- function(x, t_min, t_max, to_data_table=TRUE) {
  out <- x < t_min | x > t_max
  if (to_data_table) {
    return(out)
  } else {
    y <- x[out & !is.na(x)]
    return(list(x=y, pos=which(x %in% y)))
  }
}


#' @title EXtrae las series
#'
#' @param x no lo sé
#' @param y no lo sé
#'
#' @return
#' @export
#'
extraer_series <- function(x, y) {
  nxy <- names(y)

  ans <- lapply(nxy, function(i) {
    y1 <- y[[i]]
    x1 <- x[[i]]$datos
    cols <- names(y1)[1:(which(names(y1) == "variable") + 1)]
    vars <- names(y1)[(which(names(y1) == "metric_cat") + 1): (length(names(y1)) - 2)]

    y2 <- melt(y1, id.vars=c(cols, "up", "metric_cat", "posicion", "N.y"),
               measure.vars = vars, variable.name = "metrics", value.name="cuantil")[!is.na(cuantil)]
    setnames(y2, "t", "t_observado")

    xy <- merge(x1, y2, by=c(cols[-length(cols)], "metrics"), sort=F)
    xy[t > t_observado, t:=1, by=c(cols, "up", "posicion", "metrics", "t_observado")]
    xy <- xy[t > 1]
    # mymat <- t(matrix(rep(vars, nrow(y1)), nrow=length(vars)))
    # mymat[is.na(y1[, ..vars])] <- NA
    # y1$variables <- apply(mymat, 1, function(x) paste0(na.omit(x), collapse="|"))
  })
  names(ans) <- nxy
  # xy[up==T & t_observado=="2019-11-15" & posicion == 1 & metrics=="d1N"]
  return(ans)
}


#' @title Reportar
#'
#' @param x objeto de salida de la función principal.
#'
#' @return
#' @export
#'
reportar <- function(x) {
  mit <- lapply(rev(names(x)), function(y) {
    y1 <- x[[y]]
    y1$clase <- y
    y1
  })
  todo <- rbindlist(mit, use.names=T, fill=T)
  setorder(todo, t_observado, clase, posicion, up, t)
  ids <- names(todo)[1:(which(names(todo) == "variable"))]
  tops_metric <- unique(todo[, c(ids, "t_observado", "clase", "up", "posicion", "metrics", "N.y"), with=F])
  tops <- tops_metric[, list(metrics=paste0(metrics, collapse=", ")), by=c(ids, "t_observado", "clase", "up", "posicion", "N.y")]
  return(list(tops=tops, tops_metrics=tops_metric, serie=todo, ids=ids))
}


#' @title Búsqueda por rut
#'
#' @param ruts vector con los ruts a verificar.
#' @param db objeto dataframe de data.table.
#' @param cols columnas donde mirar.
#'
#' @return
#' @export
#'
buscar_ruts <- function(ruts, db, cols=c("proveedor_rut", "proveedor_nombre_fantasia", "proveedor_mercado_nombre")) {
  x <- sort(unique(na.omit(ruts)))
  exp1 <- paste0( cols[1], " %in% c(", paste0(x, collapse=", "), ")")
  tp <- unique(db[eval(parse(text=exp1)), ..cols])
  setorder(tp, proveedor_rut)
  return(tp)
}
