#' @encoding UTF-8
#' @title Calcular métricas
#'
#' @param x vector númerico.
#' @param cuantiles vector númerico con los cuantiles deseados (valor entre 0 y 1).
#' @param IQR vector con el rango intercuantil de la forma c(min, max). Ambos valores deben estar en `cuantiles`.
#' @param coef coeficiente de dispersión (de castigo), usualmente 1.5.
#'
#' @importFrom stats quantile sd na.omit setNames formula
#' @importFrom data.table rbindlist setnames melt
#' @export
#'
calcular_limites <- function(x, cuantiles, IQR, coef) {
  # calcular_limites <- function(x, cuantiles=c(0, .25, .5, .75, 1), IQR=c(.25, .75), coef=1.5) {
  # de la función boxplot.stats: sin asumir normalidad, usando cuantiles 8 como es recomendado.
  if (!all(IQR %in% cuantiles)) stop("Los valores de IQR deben estar contenidos en la variable cuantiles.")
  qs <- quantile(x, cuantiles, type=8, na.rm=T)
  ans <- c(qs, sd=sd(x, na.rm=T), mean=mean(x, na.rm=T), N=sum(!is.na(x)))
  qtls <- ans[which(cuantiles %in% IQR)]
  iqr <- diff(qtls)
  return(c(ans, qtls + c(-1, 1) * coef * iqr))
}


#' @encoding UTF-8
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
obtener_outliers <- function(x, t_min, t_max, to_data_table=TRUE) {
  out <- x < t_min | x > t_max
  if (to_data_table) {
    return(out)
  } else {
    y <- x[out & !is.na(x)]
    return(list(x=y, pos=which(x %in% y)))
  }
}


#' @encoding UTF-8
#' @title EXtrae las series
#'
#' @param obj objeto de la clase `reclamos_seleccion`.
#'
#' @importFrom data.table rbindlist setnames melt merge.data.table dcast
#' @export
#'
extraer_series <- function(obj) {
  if (!"reclamos" %in% class(obj)) stop("Objeto debe ser de la clase reclamos")

  x <- obj$reclamos
  y <- obj$ranking$seleccion
  nxy <- names(y)

  ans <- lapply(nxy, function(i) {
    y1 <- y[[i]]
    x1 <- x[[i]]$datos
    cols <- names(y1)[1:(which(names(y1) == "variable") + 1)]
    vars <- names(y1)[(which(names(y1) == "metric_cat") + 1): (length(names(y1)) - 2)]

    y2 <- melt(y1, id.vars=c(cols, "up", "metric_cat", "posicion", "N_registros"),
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

  #-------- RECLAMOS EXPORTADOS: originales, con su id
  # el cruce sólo se hará desde proveedor rut hacia adelante, caso contrario, nada.
  if ("proveedor_rut" %in% obj$Clases) {
    nstart <- which(obj$Clases %in% "proveedor_rut")
    to_export <- names(y)[nstart:length(obj$Clases)]
    expr1 <- formula(paste0(paste(obj$Clases, collapse=" + "), " + t + up + posicion + clase + rank_id ~ variable"))
    temp <- lapply(rev(to_export), function(x) {
      tp <- y[[x]]
      tp[, clase:=x]
    })
    recl1 <- rbindlist(temp, fill=T)[!is.na("proveedor_rut"), c(obj$Clases, "variable", "variable_valor", "t", "up", "posicion", "clase"), with=F]
    recl1[, rank_id:=1:.N]
    recl2 <- dcast(recl1, expr1, value.var="variable_valor")
    extra_vars <- sort(unique(recl1$variable))
    min_date <- as.POSIXct(paste0(strftime(min(recl2$t), "%Y-%m"), "-01"))
    expr2 <- formula(paste0("proveedor_nombre_fantasia +", paste(obj$Clases[to_export], collapse=" + "), " + ",
                            paste(extra_vars, collapse= " + "),
                            " + t + up + posicion + caso_numero + reclamo_descripcion ~ clase"))
    fusion <- list()
    for (i in 1:length(to_export)){
      temp <- obj$original[caso_creacion_fecha >= min_date, ]
      temp[, t:=as.Date(paste0(strftime(temp$caso_creacion_fecha, "%Y-%m"), "-15"))]
      x1 <- recl2[clase %in% to_export[i], c(obj$Clases[to_export[1:i]], extra_vars, "t", "up", "posicion", "clase", "rank_id"), with=F]
      y1 <- temp[!is.na(proveedor_rut) & !is.na(caso_numero),
                 c("proveedor_nombre_fantasia", obj$Clases[to_export[1:i]], extra_vars, "t", "caso_numero", "reclamo_descripcion"),
                 with=F]
      x1[is.na(x1)] <- ''
      y1[is.na(y1)] <- ''
      temp <- lapply(extra_vars, function(v) {
        recl <- merge(x1, y1, by=c(obj$Clases[to_export[1:i]], v, "t"), suffixes=c("", ".borrar"))
      })
      fusion <- c(fusion, temp)
    }
    fusion <- rbindlist(rev(fusion), fill=T)
    fusion[is.na(fusion)] <- ''
    fusion[, rank_id:=NULL]
    fusion <- fusion[, -grep(".borrar", names(fusion)), with=F]  # aquí se ve que se producen duplicados: motivo desconocido
    fusion <- unique(fusion)  # y aquí "solucionamos" el problema anterior...
    recls <- dcast(fusion, expr2, value.var="caso_numero")
    recls <- merge(recls, obj$original[, c("caso_numero", "cierre_corto"), with=F], by="caso_numero", suffixes=c("", "_caso"))
    if ("cierre_corto_caso" %in% names(recls)){
      setnames(recls, "up", "reclamo_ascendente")
    } else {
      setnames(recls, c("up", "cierra_corto"), c("reclamo_ascendente", "cierre_corto_caso"))
    }
    setorder(recls, t, -reclamo_ascendente, posicion)
    setcolorder(recls, c("proveedor_nombre_fantasia", obj$Clases[to_export], extra_vars, "t",
                         "reclamo_ascendente", "posicion", "caso_numero", names(obj$Clases[to_export]),
                         "cierre_corto_caso", "reclamo_descripcion"))
    # caso_numero tiene NAs.... por la chucha
  } else {
    recls <- NULL
  }

  obj$series <- list(temporal=ans, reclamos=recls)
  obj$estado$series <- TRUE
  return(obj)
}


#' @encoding UTF-8
#' @title Reportar
#'
#' @param dat objeto de salida de la función principal.
#'
#' @return
#' @export
#'
reportar <- function(dat) {
  if (!"reclamos" %in% class(dat)) stop("Objeto debe ser de la clase reclamos")

  x <- dat$series$temporal
  mit <- lapply(rev(names(x)), function(y) {
    y1 <- x[[y]]
    y1$clase <- y
    y1
  })
  todo <- rbindlist(mit, use.names=T, fill=T)
  setorder(todo, t_observado, clase, posicion, up, t)
  ids <- names(todo)[1:(which(names(todo) == "variable"))]
  tops_metric <- unique(todo[, c(ids, "t_observado", "clase", "up", "posicion", "metrics", "N_registros"), with=F])
  tops <- tops_metric[, list(metrics=paste0(metrics, collapse=", ")), by=c(ids, "t_observado", "clase", "up", "posicion", "N_registros")]

  dat$reporte <- list(tops=tops, tops_metrics=tops_metric, serie=todo, ids=ids)
  dat$estado$reporte <- TRUE
  class(dat) <- "reclamos"
  return(dat)
}


#' @encoding UTF-8
#' @title Búsqueda por rut
#'
#' @param ruts vector con los ruts a verificar.
#' @param db objeto dataframe de data.table.
#' @param cols columnas donde mirar y que extraer. La primera se usa para comparar (debe contener el rut) y el resto trae más información
#'
#' @export
#'
buscar_ruts <- function(ruts, db, cols=c("proveedor_rut", "proveedor_nombre_fantasia", "proveedor_mercado_nombre")) {
  if (!"reclamos" %in% class(db)) stop("Objeto debe ser de la clase reclamos")

  df <- db$original
  x <- sort(unique(na.omit(ruts)))
  exp1 <- paste0( cols[1], " %in% c(", paste0(x, collapse=", "), ")")
  tp <- df[eval(parse(text=exp1)), list(N=.N), by=cols]
  setorder(tp, proveedor_rut)
  return(tp)
}


#' @encoding UTF-8
#' @title Exportar diferentes elementos de la clase reclamos a csv.
#'
#' @param obj objeto de la clase reclamos.
#' @param que vector de objetos a exportar. Opciones válidas son: `ranking`, `agrupados`.
#' @param donde ruta de salida. Si ninguna es específicada se usará el directorio actual de trabajo.
#' @param ... parámetros adicionales para \link[data.table]{fwrite}, como separador de columnas, marcador de decimales, verbose, etc.
#'
#' @details Por completar, con la descripción de las columnas de cada output.
#'
#' @importFrom data.table fwrite
#' @export
#'
exportar <- function(obj, que='ranking', donde=NULL, ...) {
  if (!"reclamos" %in% class(obj)) stop("Objeto debe ser de la clase reclamos")
  mpath <- ifelse(is.null(donde), getwd(), donde)
  ctime <- format(Sys.time(), "%Y-%m-%d_%H%M%S")

  fwrite(obj$series$reclamos, file.path(mpath, paste0(ctime, "_top_ranking_reclamos_caso_numero.csv")), ...)
  if ('ranking' %in% que) {
    fwrite(obj$reporte$tops, file.path(mpath, paste0(ctime, "_top_ranking_formato_ancho.csv")), ...)  # top metricas wide format colapsado
    fwrite(obj$reporte$tops_metrics, file.path(mpath, paste0(ctime, "_top_ranking_formato_largo.csv")), ...)  # top metricas long format
    fwrite(obj$reporte$serie, file.path(mpath, paste0(ctime, "_top_ranking_serie_formato_largo.csv")), ...) # serie completa de los top seleccionados, long format
    # tops_metrics, pero formato wide (iterar sobre la lista)
    temp <- lapply(names(obj$ranking$seleccion), function(x){
      fwrite(obj$ranking$seleccion[[x]], file.path(mpath, paste0(ctime, "_top_ranking_formato_ancho_", x, ".csv")), ...)
    })
  }
  if ('agrupados' %in% que) {
    temp <- lapply(names(obj$reclamos), function(x){
      fwrite(obj$reclamos[[x]]$datos, file.path(mpath, paste0(ctime, "_datos_agrupados_", x, ".csv")), ...)
      fwrite(obj$reclamos[[x]]$limites, file.path(mpath, paste0(ctime, "_datos_agrupados_limites_", x, ".csv")), ...)
    })
  }

}

####--------------------------------------------------------------------------
#' @encoding UTF-8
#' @title Obtener registro con mayoría
#'
#' @param db data.table dataframe
#' @param cols columna(s) de agrupación, por las que filtrarán los registros.
#' @param bycol columna(s) que contiene el registro que seerá usadao para buscar el mayor N.
#'
#' @return
#' @export
#'
#' @examples 1+1
obtener_mayoria <- function(db, cols, bycol) {
  colsd <- setdiff(cols, bycol)
  tabN <- db[, list(N=.N), by=cols]
  tabMaj <- tabN[, eval(parse(text=paste0("list(", bycol, " = ", bycol, "[which.max(N)])"))), by=colsd, ]
  return(tabMaj)
}

####--------------------------------------------------------------------------
#' @encoding UTF-8
#' @title Obtener registro más reciente
#'
#' @details Basado en `caso_creacion_fecha`.
#'
#' @param db data.table dataframe
#' @param cols columna(s) de agrupación, por las que filtrarán los registros.
#' @param bycol columna(s) que contiene el registro que seerá usadao para buscar el mayor N.
#'
#' @return
#' @export
#'
#' @examples 1+1
obtener_reciente <- function(db, cols, bycol) {
  colsd <- setdiff(cols, bycol)
  tabN <- db[, list(N=as.numeric(max(caso_creacion_fecha))), by=cols]
  tabMaj <- tabN[, eval(parse(text=paste0("list(", bycol, " = ", bycol, "[which.max(N)])"))), by=colsd, ]
  return(tabMaj)
}

#' @encoding UTF-8
#' @title Print method para la clase reclamos
#'
#' @param x objeto de la clae `reclamos`.
#' @param ... parámetros adicionales para ,\link{print}.
#'
#' @export
print.reclamos <- function(x, ...){
  cat("Las clases definidas son:\n")
  print(x$Clases)
  cat("Con las siguientes categorías:\n"  )
  cat(x$Categorias)
}
