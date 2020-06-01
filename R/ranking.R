#' @encoding UTF-8
#' @title Seleccionar por ranking
#'
#' @param dat objeto de la clase `reclamos`.
#' @param topn número de posiciones del ranking a incluir. Se pueden retornar más o menos valores,
#' dependiendo de si existen empates o el número final de registros.
#' @param nmin número mínimo de datos requeridos para estar presente en la selección.
#' @param reporte si es TRUE, imprime un reporte resumen.
#' @param ... argumentos extras pasados a \link{rankear}.
#'
#' @importFrom data.table setnames dcast setorder
#' @export
#'
#' @examples 1+1
seleccionar_ranking <- function(dat, topn, nmin, reporte=TRUE, ...) {
  if (!"reclamos" %in% class(dat)) stop("Objeto dat debe ser de la clarse reclamos")

  selec <- dat[['reclamos']]
  clases <- dat$Clases

  out_ranks <- lapply(1:length(clases), function(i) {
    ys <- clases[1:i]
    y <- selec[[names(clases)[i]]]$datos[outlier_i==T | outlier_e==T]
    ans <- rankear(y, ys, ...)
  })
  names(out_ranks) <- names(clases)

  condicion1 <- paste0("N.y > ", nmin)
  seleccion <- lapply(1:length(clases), function(i) {
    x <- names(clases)[i]
    ys <- c(clases[1:i], "variable_valor", "variable", "t")
    y1 <- out_ranks[[x]]

    # mrank <- y[, list(N=.N), by=c("up", "metric_cat", "outlier_ir")][order(outlier_ir)]
    # tope <- mrank[which(cumsum(mrank$N) >= topn)[1], outlier_ir]
    # y1 <- y[outlier_ir <= tope]
    # comparar contra:
    y2 <- merge(y1, selec[[x]]$datos, by=ys, all.x=T, order=F)
    exp1 <- formula(paste0(paste0(ys, collapse=" + "), "+ outlier_i.x + outlier_e.x + outlier_ir + outlier_r + up + metric_cat ~ metrics"))
    exp2 <- formula(paste0(paste0(ys, collapse=" + "), " ~ metrics"))
    y3 <- dcast(y2[outlier_i.y==T], exp1, value.var="p")
    mn <- dcast(selec[[x]]$datos[metrics=="N"], exp2, value.var="values")
    y4 <- merge(y3, mn, by=ys, all.x=T, order=F)
    setorder(y4, outlier_ir, outlier_r, -N.y)
    setnames(y4, "N.x", "N")
    # y4[eval(parse(text=condicion1))][1:topn]
    fin <- y4[y4[N.y > nmin, .I[1:topn], c("up", "t")]$V1][!is.na(N.y)]
    fin[, posicion:=1:.N, by=c("up", "t")]
  })
  names(seleccion) <- names(clases)
  # seleccion2 <- list(reclamos=dat, seleccion=seleccion, reporte=NULL, atributos=list(topn=topn, nmin=nmin))
  dat$ranking <- list(seleccion=seleccion)
  dat$ranking$reporte <- NULL
  dat$atributos$ranking <- list(topn=topn, nmin=nmin)
  dat$estado$ranking <- TRUE

  if (reporte){
    report <- lapply(dat$ranking$seleccion, function(x) {
      xt <- x[, list(N=.N), by=c("up", "t", "metric_cat")]
      setorder(xt, t, up)
      xt
    })
    print(report)
    dat$ranking$reporte <- reporte
  }

  class(dat) <- "reclamos"
  return(dat)
}

#' @encoding UTF-8
#' @title Generar ranking
#'
#' @param dt data.table.
#' @param col nombre de la columna a evaluar.
#' @param t_observado tiempo en el cual se evalua el ranking.
#'
#' @importFrom data.table frankv
#' @export
#'
#' @examples 1+1
rankear <- function(dt, col, t_observado='max') {
  # metrics_ <- as.character(unique(dt$metrics))
  # metricsL <- list(t=metrics_[grep("t", metrics_)], N=metrics_[grep("N", metrics_)])
  dt[, metric_cat:=ifelse(grepl("t", metrics), "tiempo", "conteo")]
  # Hay que decidir si en resumen, los outliers son hacia arriba o hacia baajo para las metricas, para un tiempo t.
  # La mayoría de las metricas calificadas como outliers decidirá si el registro está por arriba o por abajo, en caso de empate, se clasifica por abajo.
  dt[, up:=ifelse(sum(values >= 0) > .N/2, TRUE, FALSE), by=c(col, "variable_valor", "variable", "metric_cat", "t")]

  if (t_observado[1] == 'max') {
    my_outs <- dt[t %in% max(t), list(outlier_i=sum(outlier_i, na.rm=T), outlier_e=sum(outlier_e, na.rm=T)),
                  by=c(col, "variable_valor", "variable", "t", "up", "metric_cat")]
  } else {
    ts <- as.Date(t_observado)
    my_outs <- dt[t %in% ts, list(outlier_i=sum(outlier_i, na.rm=T), outlier_e=sum(outlier_e, na.rm=T)),
                  by=c(col, "variable_valor", "variable", "t", "up", "metric_cat")]
  }

  my_outs[, c("outlier_ir", "outlier_er", "outlier_r"):=list(frankv(-outlier_i, na.last="keep"),
                                                             frankv(-outlier_e, na.last="keep"),
                                                             frankv(-c(outlier_i + outlier_e), na.last="keep")),
          by=c("metric_cat", "up", "t")]
  setorder(my_outs, outlier_r)
  return(my_outs)
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

#' @encoding UTF-8
#' @title Ranking normalizado
#'
#' @param x un vector
#'
#' @return
#' @export
#' @importFrom data.table frank
#'
#' @examples norm_rank(1:10)
norm_rank <- function(x) {
  list(scale(x, center=T, scale=T)[, 1], (frank(x, na.last="keep")/sum(!is.na(x))) )
}
