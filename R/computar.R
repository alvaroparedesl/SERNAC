#' @encoding UTF-8
#' @title Procesar reclamos
#'
#' @param mdatai data.frame con la base de datos.
#' @param byCategoria vector de strings con las variables consideradas como categorías.
#' @param byClase vector de strings con nombres (\link{setNames}), que incluya las variables de clase (categorización). El orden es importante.
#' @param lags vector númerico
#' @param n_min número mínimo de observaciones
#' @param rango_fechas vector de la forma `c(min, max)` que incluye las fechas inferior y superior a considerar. Puede ser en formato fecha o
#'     como string en formato YYYY-mm-dd.
#' @param verbose si es TRUE, devuelve mensajes sobre el avance del proceso, de lo contrario, no entrega ningún mensaje. Por defecto es TRUE.
#' @param ... argumentos extras pasados a \link{agregar_datos}, como `usar_tiempo`.
#' @inheritParams calcular_limites
#'
#' @importFrom data.table rbindlist dcast
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
                             rango_fechas=c(NULL, NULL),
                             verbose=TRUE,
                             ...) {
  tiempos <- Sys.time()

  #------ 0:
  mdatai <- data.table(mdatai)

  # filtros de fecha
  if (!is.na(rango_fechas[1])){
    mdatai <- mdatai[caso_creacion_fecha >= as.POSIXct(rango_fechas[1])]
  }
  if (!is.na(rango_fechas[2])){
    mdatai <- mdatai[caso_creacion_fecha <= as.POSIXct(rango_fechas[2])]
  }

  rutNombreN <- mdatai[, list(N=.N), by="proveedor_rut"]
  mdata <- mdatai[proveedor_rut %in% rutNombreN[N > n_min]$proveedor_rut]  # TODO: agregar esto y el número de observaciones a un objeto reporte.
  if (verbose) cat("Se descartaron ", paste0(nrow(mdatai) - nrow(mdata), " registros con menos de 50 observaciones por proveedor a lo largo del periodo total.\n"))

  #------ 1:
  if (verbose) cat("Agregando datos... ")
  bases <- rbindlist(lapply(byCategoria, function(x) agregar_datos(mdata, bycols=x, prep=T, ...)))

  aggs <- lapply(1:length(byClase), function(i) {
    ys <- byClase[1:i]
    dupl <- which(byCategoria %in% ys) # evitar duplicar categorias y clases
    if (length(dupl) > 0 ){
      variables <- byCategoria[-dupl]
    } else {
      variables <- byCategoria
    }
    rbindlist(lapply(variables,
                     function(x) agregar_datos(mdata, bycols=c(ys, x), prep=T,
                                        prepIgnore=c(ys), ...)))
  })
  # names(aggs) <- names(byClase)
  new <- c(list(base=bases), aggs)
  tiempos <- c(tiempos, Sys.time()); if (verbose) cat(difftime(tiempos[length(tiempos)], tiempos[length(tiempos) - 1], units="secs"), "segundos.\n")
  #--------------------------------------------------------------------------------------------------------------------------------------------------
  #----- 2:
  if (verbose) cat("Calculando métricas... ")
  metricsN <- c(paste0("q", sprintf("%03d", cuantiles*100)), "sd", "mean", "N", "lower_threshold", "upper_threshold")
  exprs1 <- sprintf("`:=`(%s)", paste0("d", lags, "N=c(rep(NA,", lags, "), diff(N, ", lags, "))", collapse=", "))
  ans <- lapply(new, generar_metricas, cuantiles=cuantiles, IQR=IQR, coef=coef, byClase=byClase, exprs1=exprs1, metricsN=metricsN)

  tiempos <- c(tiempos, Sys.time()); if (verbose) cat(difftime(tiempos[length(tiempos)], tiempos[length(tiempos) - 1], units="secs"), "segundos.\n")
  #--------------------------------------------------------------------------------------------------------------------------------------------------
  #------ 3
  if (verbose) cat("Convirtiendo datos... ")
  limit <- dcast(ans$base$limites[metric %in% c("lower_threshold", "upper_threshold")],
                 variable_valor + variable + metrics ~ metric, value.var="values_norm")

  ans2 <- lapply(2:length(ans), function(i) {
    gt <- ans[[i]]
    temp <- merge(gt$datos, limit, by=c("variable_valor", "variable", "metrics"), all.x=T, sort=F)
    temp[, outlier_external:=obtener_outliers(values_norm, lower_threshold, upper_threshold)]
    temp[, c("lower_threshold", "upper_threshold"):=NULL]
    return(list(datos=temp, limites=gt$limites))
  })
  names(ans2) <- names(byClase)

  tiempos <- c(tiempos, Sys.time()); if (verbose) cat(difftime(tiempos[length(tiempos)], tiempos[length(tiempos) - 1], units="secs"), "segundos.\n")
  if (verbose) cat("Cálculos realizados en ", difftime(tiempos[length(tiempos)], tiempos[1], units="secs"), "segundos.\n")

  ans2 <- list(original=mdatai, proveedores=rutNombreN, reclamos=c(list(base=ans$base), ans2))
  ans2$Clases <- byClase
  ans2$Categorias <- byCategoria
  ans2$estado <- list(reclamos=TRUE, ranking=FALSE, series=FALSE, reporte=FALSE)
  ans2$atributos <- list(computar_reclamos= list(lags=lags,
                                                 cuantiles=cuantiles,
                                                 IQR=IQR,
                                                 coef=coef,
                                                 n_min=n_min,
                                                 rango_fechas=rango_fechas,
                                                 runtimes=tiempos))

  class(ans2) <- "reclamos"
  return(ans2)
}


#' @encoding UTF-8
#' @title Genera metricas internas
#'
#' @description Subfuncón sin ninguna utilidad fuera del entorno interno, para estrucutrar de mejor manera los cálculos.
#'
#' @param x data.table data frame.
#' @param exprs1 expresión.
#' @param metricsN data.table data frame
#' @inheritParams computar_reclamos
#'
#' @importFrom data.table melt dcast set
#'
generar_metricas <- function(x, cuantiles, IQR, coef, byClase, exprs1, metricsN) {
  #---- Definir variables
  svar <- intersect(names(x), byClase)
  svarP <- c(svar, "variable_valor", "variable")

  #---- Ejecutar
  x[, eval(parse(text=exprs1)), by=svarP]
  for (j in setdiff(names(x), c(svarP, "t"))) set(x, j = j, value = as.double(x[[j]])) # transformar todo a double, para evitar problemas en melt [are not all of the same type, coerced]
  aggsM <- melt(x, id.vars=c(svarP, "t"), variable.name="metrics", value.name="values")  # formato ancho a largo
  aggsM[, c("values_norm", "p") := norm_rank(values), by=svarP]  # ranking normalizado
  # setkey(aggsM, proveedor_rut, variable, variable_valor, metrics)
  #----- ESTA vuelta con exp_1 y exp_2 es porque los argumentos directos no son reconocidos (problemas de scope)
  exp_1 <- paste0('calcular_limites(values, cuantiles=c(', paste0(cuantiles, collapse=", "),
                  '), IQR=c(', paste0(IQR, collapse=", "),
                  '), coef=', coef, ')')
  exp_2 <- paste0('calcular_limites(scale(values)[, 1], cuantiles=c(', paste0(cuantiles, collapse=", "),
                  '), IQR=c(', paste0(IQR, collapse=", "),
                  '), coef=', coef, ')')

  limites <- aggsM[, list(metric=metricsN,
                          # values=calcular_limites(values, cuantiles=c(0, .25, .5, .75, 1), IQR=c(.25, .75), coef=1.5),
                          # values_norm=calcular_limites(scale(values)[, 1])),
                          values=eval(parse(text=exp_1)),
                          values_norm=eval(parse(text=exp_2))),
                   by=c(svarP, "metrics")]

  form1 <- paste0(c(svarP, "metrics ~ metric"), collapse= " + ")
  mtable <- dcast(limites[metric %in% c("lower_threshold", "upper_threshold")], form1, value.var="values")
  merged <- merge(aggsM, mtable, by=c(svarP, "metrics"), all.x=T, sort=F)
  merged[, outlier_internal:=obtener_outliers(values, lower_threshold, upper_threshold)]
  merged[, c("lower_threshold", "upper_threshold"):=NULL]
  return(list(datos=merged, limites=limites))
}
