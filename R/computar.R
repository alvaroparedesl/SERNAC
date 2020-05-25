#' @title Procesar reclamos
#'
#' @param mdatai data.frame con la base de datos.
#' @param byCategoria vector de strings con las variables consideradas como categorías.
#' @param byClase vector de strings con nombres (\link{setNames}), que incluya las variables de clase (categorización). El orden es importante.
#' @param lags vector númerico
#' @param n_min número mínimo de observaciones
#' @param rango_fechas vector de la forma `c(min, max)` que incluye las fechas inferior y superior a considerar. Puede ser en formato fecha o
#'     como string en formato YYYY-mm-dd.
#' @inheritParams calcular_metricas
#'
#' @return
#' @export
#'
computar_reclamos <- function(mdatai,
                             byCategoria=c("cierre_corto", "proveedor_mercado_nombre", "proveedor_mercado_categoria_nombre",
                                           "proveedor_rut", "cut_region"),
                             byClase=c(industria="proveedor_mercado_categoria_nombre", proveedor="proveedor_rut",
                                       producto="mercado_tipo_producto_nombre"),
                             lags=1:6,
                             cuantiles=c(0, .25, .5, .75, 1),
                             IQR=c(.25, .75),
                             coef=1.5,
                             n_min=50,
                             rango_fechas=c(NULL, NULL)) {
  start_time <- Sys.time()

  #------ 0:
  mdatai <- data.table(mdatai)
  rutNombreN <- mdatai[, list(N=.N), by="proveedor_rut"]
  mdata <- mdatai[proveedor_rut %in% rutNombreN[N > n_min]$proveedor_rut]
  cat(paste0(nrow(mdatai) - nrow(mdata), " registros con menos de 50 observaciones a lo largo del periodo total fueron descartados.\n"))

  if (!is.na(rango_fechas[1])){
    mdata <- mdata[caso_creacion_fecha >= as.POSIXct(rango_fechas[1])]
  }
  if (!is.na(rango_fechas[2])){
    mdata <- mdata[caso_creacion_fecha <= as.POSIXct(rango_fechas[2])]
  }

  #------ 1:
  metricsN <- c(paste0("q", sprintf("%03d", cuantiles*100)), "sd", "mean", "N", "lower_threshold", "upper_threshold")
  exprs1 <- sprintf("`:=`(%s)", paste0("d", lags, "N=c(rep(NA,", lags, "), diff(N, ", lags, "))", collapse=", "))

  bases <- rbindlist(lapply(byCategoria, function(x) agg_by(mdata, bycols=x, prep=T)))

  aggs <- lapply(1:length(byClase), function(i) {
    ys <- byClase[1:i]
    rbindlist(lapply(byCategoria[-which(byCategoria %in% ys)],
                     function(x) agg_by(mdata, bycols=c(ys, x), prep=T,
                                        prepIgnore=c(ys))))
  })
  # names(aggs) <- names(byClase)
  new <- c(list(base=bases), aggs)

  #----- 2:
  ans <- lapply(new, function(dt, cuantiles, IQR, coef) {
    #---- Definir variables
    svar <- intersect(names(dt), byClase)
    svarP <- c(svar, "variable_valor", "variable")

    #---- Ejecutar
    dt[, eval(parse(text=exprs1)), by=svarP]
    aggsM <- melt(dt, id.vars=c(svarP, "t"), variable.name="metrics", value.name="values")  # formato ancho a largo
    aggsM[, c("values_norm", "p") := norm_rank(values), by=svarP]
    # setkey(aggsM, proveedor_rut, variable, variable_valor, metrics)
    #----- ESTA vuelta con exp_1 y exp_2 es porque los argumentos directos no son reconocidos (problemas de scope)
    exp_1 <- paste0('calcular_metricas(values, cuantiles=c(', paste0(cuantiles, collapse=", "),
                    '), IQR=c(', paste0(IQR, collapse=", "),
                    '), coef=', coef, ')')
    exp_2 <- paste0('calcular_metricas(scale(values)[, 1], cuantiles=c(', paste0(cuantiles, collapse=", "),
                    '), IQR=c(', paste0(IQR, collapse=", "),
                    '), coef=', coef, ')')

    limites <- aggsM[, list(metric=metricsN,
                            # values=calcular_metricas(values, cuantiles=c(0, .25, .5, .75, 1), IQR=c(.25, .75), coef=1.5),
                            # values_norm=calcular_metricas(scale(values)[, 1])),
                            values=eval(parse(text=exp_1)),
                            values_norm=eval(parse(text=exp_2))),
                     by=c(svarP, "metrics")]

    form1 <- paste0(c(svarP, "metrics ~ metric"), collapse= " + ")
    mtable <- dcast(limites[metric %in% c("lower_threshold", "upper_threshold")], form1, value.var="values")
    merged <- merge(aggsM, mtable, by=c(svarP, "metrics"), all.x=T, sort=F)
    merged[, outlier_i:=get_outliers(values, lower_threshold, upper_threshold)]
    merged[, c("lower_threshold", "upper_threshold"):=NULL]
    return(list(datos=merged, limites=limites))
  }, cuantiles=cuantiles, IQR=IQR, coef=coef)

  limit <- dcast(ans$base$limites[metric %in% c("lower_threshold", "upper_threshold")],
                 variable_valor + variable + metrics ~ metric, value.var="values_norm")

  ans2 <- lapply(2:length(ans), function(i) {
    gt <- ans[[i]]
    temp <- merge(gt$datos, limit, by=c("variable_valor", "variable", "metrics"), all.x=T, sort=F)
    temp[, outlier_e:=get_outliers(values_norm, lower_threshold, upper_threshold)]
    temp[, c("lower_threshold", "upper_threshold"):=NULL]
    return(list(datos=temp, limites=gt$limites))
  })
  names(ans2) <- names(byClase)
  ans2 <- c(list(base=ans$base), ans2)

  end_time <- Sys.time()
  cat(end_time - start_time, "\n")

  return(ans2)
}
