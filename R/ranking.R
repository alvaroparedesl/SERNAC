#' @encoding UTF-8
#' @title Seleccionar por ranking
#'
#' @param dat objeto de la clase `reclamos`.
#' @param topn número de posiciones del ranking a incluir. Se pueden retornar más o menos valores,
#' dependiendo de si existen empates o el número final de registros.
#' @param nmin número mínimo de datos requeridos para estar presente en la selección.
#' @param reporte si es TRUE, imprime un reporte resumen.
#' @param tipo tipo de ranking a usarse. Las opciones son `N` (número), `puntaje` o `mixto`. Por defecto es `N`. Ver la sección `Details` para mayor información.
#' @param ... argumentos extras pasados a \link{rankear}.
#'
#' @details Todos los tipos de rankings se calculan en base a las métricas (el número de reclamos, la proporción de acogida,
#' los lags definidos y el tiempo de respuesta, en el caso de ser incorporado), ya sea por su número o por su percentil.\cr
#' Para `tipo` `N` (la opción que se ha utilizado desde el inicio), el orden del ranking se calcula en base al número de métricas que caen fuera del rango intercuantil definido,
#' sumando el número de ellas de manera interna (dentro de la agrupación Clase + Categoría) y de manera externa (usando datos escalados y centrados, usando como agrupación basal, sólo Categoría).
#' Debido al ruido que se produce al usar el ranking externo, el ranking final se obtiene ordenando por el ranking interno, luego por el ranking interno y externo, y finalmente por el número
#' de reclamos del periodo `t` (donde a mayor número, se favorece el ranking).
#' \cr
#' Para `tipo` `puntaje` se usará la suma de los percentiles para armar el ranking y no el número de métricas que caen fuera del rango intercuantil definido (se suman los percentiles internos y externos).
#' Se suman el valor del percentil de todas las métricas presentes (sean o no atípicas), pero se consideran en el ranking final sólo aquellas que fueron marcadas inicialmente
#' (caso contrario, toda observación tendría siempre, una posición en el ranking). Se hace la diferenciación entre métricas catalogadas de ascendentes/descendentes;
#' para las ascendentes, se hace la suma simple de los percentiles de las métricas que tienen un cambio positivo y para las descendentes, la suma de `(1- p)` de los percentiles de las métricas que tienen
#' un cambio negativo.\cr
#' Por último, `tipo` `mixto` usa una combinación de ambas formas para armar el ranking final. Cada ranking es construido de manera independiente, y posteriormente la posición de cada uno
#' es sumada y el ranking final es recalculado (de menor a mayor valor).
#'
#' @importFrom data.table setnames dcast setorder
#' @export
#'
#' @examples 1+1
seleccionar_ranking <- function(dat, topn, nmin, reporte=TRUE, tipo="N", ...) {
  if (!"reclamos" %in% class(dat)) stop("Objeto dat debe ser de la clarse reclamos")

  selec <- dat[['reclamos']]
  clases <- dat$Clases

  out_ranks <- lapply(1:length(clases), function(i) {
    ys <- clases[1:i]
    y <- selec[[names(clases)[i]]]$datos[is_outlier_internal==T | is_outlier_external==T]
    rankear(y, ys, ...)
  })
  names(out_ranks) <- names(clases)

  seleccion <- lapply(1:length(clases), function(i) {
    x <- names(clases)[i]
    ys <- c(clases[1:i], "variable_valor", "variable", "t")
    y1 <- out_ranks[[x]]

    # mrank <- y[, list(N=.N), by=c("up", "metric_cat", "outlier_internal_rank")][order(outlier_internal_rank)]
    # tope <- mrank[which(cumsum(mrank$N) >= topn)[1], outlier_internal_rank]
    # y1 <- y[outlier_internal_rank <= tope]
    # comparar contra:
    y2 <- merge(y1, selec[[x]]$datos, by=ys, all.x=T, order=F)
    exp1 <- formula(paste0(paste0(ys, collapse=" + "), "+ up + metric_cat + ", paste(grep('^outlier', names(y2), value=T, perl=T), collapse=" + "), " ~ metrics"))
    exp2 <- formula(paste0(paste0(ys, collapse=" + "), " ~ metrics"))
    y3 <- dcast(y2[is_outlier_internal==T], exp1, value.var="p")
    mn <- dcast(selec[[x]]$datos[metrics=="N"], exp2, value.var="values")
    y4 <- merge(y3, mn, by=ys, all.x=T, order=F)
    setnames(y4, c("N.x", "N.y"), c("N", "N_registros"))
    switch(tipo,
      N = setorder(y4, outlier_internal_rank, outlier_external_rank, -N_registros),
      puntaje = setorder(y4, outlier_rank_score, -N_registros),
      mixto = setorder(y4, outlier_rank_mixto, -N_registros)
    )
    fin <- y4[y4[N_registros > nmin, .I[1:topn], c("up", "t")]$V1][!is.na(N_registros)]
    fin[, posicion:=1:.N, by=c("up", "t")]
  })
  names(seleccion) <- names(clases)
  # seleccion2 <- list(reclamos=dat, seleccion=seleccion, reporte=NULL, atributos=list(topn=topn, nmin=nmin))
  dat$ranking <- list(seleccion=seleccion)
  dat$ranking$reporte <- NULL
  dat$atributos$ranking <- list(dates=sort(unique(seleccion[[1]]$t)), topn=topn, nmin=nmin, tipo=tipo, extra=list(...))
  dat$estado$ranking <- TRUE

  if (reporte){
    report <- lapply(dat$ranking$seleccion, function(x) {
      xt <- x[, list(N=.N), by=c("up", "t", "metric_cat")]
      setorder(xt, t, up)
      xt
    })
    print(report)
    dat$ranking$reporte <- report
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
  # La mayoría de las metricas calificadas como outliers decidirá si el registro está por arriba o por abajo, en caso de empate, se clasifica por arriba.
  dt[, up:=ifelse(sum(values >= 0) >= .N/2, TRUE, FALSE), by=c(col, "variable_valor", "variable", "metric_cat", "t")]

  if (t_observado[1] == 'max') {
    expr1 <- "t %in% max(t)"
  } else {
    ts <- as.Date(t_observado)
    expr1 <- "t %in% ts"
  }
  my_outs <- dt[eval(parse(text=expr1)), list(outlier_internal_N = sum(is_outlier_internal, na.rm=T),
                                              outlier_external_N = sum(is_outlier_external, na.rm=T),
                                              outlier_scorein = sum((1 - p)*(values < 0)*(!up),
                                                                    p*(values >= 0)*(up), na.rm=T),
                                              outlier_scoreex = sum((1 - p_basal)*(values < 0)*(!up),
                                                                    p_basal*(values >= 0)*(up), na.rm=T)),
                #---- debería usar sólo los valores que fueron marcados como outliers???? o sumarlos todos???
                                              # outlier_scorein = sum((1 - p)*(values < 0)*(!up)*is_outlier_internal,
                                              #                       p*(values >= 0)*(up)*is_outlier_internal, na.rm=T),
                                              # outlier_scoreex = sum((1 - p_basal)*(values < 0)*(!up)*is_outlier_external,
                                              #                       p_basal*(values >= 0)*(up)*is_outlier_external, na.rm=T)),
                by=c(col, "variable_valor", "variable", "t", "up", "metric_cat")]

  outliers_vars <- c("outlier_internal_rank", "outlier_external_rank", "outlier_rank",
                     "outlier_internal_rank_score", "outlier_external_rank_score", "outlier_rank_score")
  my_outs[, (outliers_vars):=list(frankv(-outlier_internal_N, na.last="keep"),
                                  frankv(-outlier_external_N, na.last="keep"),
                                  frankv(-c(outlier_internal_N + outlier_external_N), na.last="keep"),
                                  frankv(-outlier_scorein, na.last="keep"),
                                  frankv(-outlier_scoreex, na.last="keep"),
                                  frankv(-c(outlier_scorein + outlier_scoreex), na.last="keep")),
          by=c("metric_cat", "up", "t")]
  my_outs[, outlier_rank_mixto:=frankv(c(outlier_rank + outlier_rank_score), na.last="keep")] # el que te tiene menos, va primero
  # tp <- abs(my_outs[up==T & t==as.Date("2020-04-15"), outlier_rank - outlier_rank_score]); sum(tp)/length(tp)
  # tp <- abs(my_outs[up==F & t==as.Date("2020-04-15"), outlier_rank - outlier_rank_score]); sum(tp)/length(tp)
  # dt[t==as.Date("2020-04-15"), ]
  return(my_outs)
}


#' @encoding UTF-8
#' @title Ranking normalizado
#'
#' @param x un vector

#' @return una lista de valores centrados y escalados, del cuantil al que corresponde cada observación y del cuantil al cual pertenecen con respecto a los valores escalados.
#'
#' @export
#' @importFrom data.table frank
#'
#' @examples norm_rank(1:10)
norm_rank <- function(x) {
  list(scale(x, center=T, scale=T)[, 1],
       frank(x, na.last="keep")/sum(!is.na(x)) )  # percentil al que pertenece la observación.
}


#' @encoding UTF-8
#' @title Visualizar rankings
#'
#' @inheritParams plot.reclamos
#' @export
#'
ver_ranking <- function(x, t_observado=NULL, pos=1, ascendente=NULL, clase="industria"){
  # x <- outt; t_observado='2019-10-15'; pos=NULL; ascendente=NULL; clase="industria"
  if (!"reclamos" %in% class(x)) stop("Objeto x debe ser de la clarse reclamos")
  tp <- x$reporte$tops
  vars <- list(t_observado=t_observado, posicion=pos, up=ascendente, clase=clase)
  expr1 <- NULL
  for (i in 1:length(vars)){
    if (!is.null(vars[[i]])){
      if (names(vars[i]) == 't_observado') {
        temp <- paste0(names(vars[i]), " %in% as.Date(c('", paste(vars[[i]], collapse="', '"), "'))")
      } else if (names(vars[i]) == 'posicion') {
        temp <- paste0(names(vars[i]), " %in% c(", paste(vars[[i]], collapse=", "), ")")
      } else {
        temp <- paste0(names(vars[i]), " %in% c('", paste(vars[[i]], collapse="', '"), "')")
      }

      if (is.null(expr1)) {
        expr1 <- temp
      } else {
        expr1 <- paste(expr1, temp, sep=" & ")
      }
    }
  }

  tp[eval(parse(text=expr1))]
}
