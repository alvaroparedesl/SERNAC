#' @encoding UTF-8
#' @title Función que permite añadir datos nuevos
#'
#' @description Esta función es un parser para las funciones individuales para agregar nuevos datos, de una manera más cómoda y fácil.
#'
#' @param base base de datos data.frame o data.table de base, a la cual se le añadirá nueva información.
#' @param ... argumentos pasados a las funciones \link{homologar_db}, \link{consolidar_db} y \link{depurar_ruts}.
#'
#' @export
#'
agregar_dbs <- function(base, ...) {
  input <- list(...)
  input_names <- names(input)
  data <- input[input_names == ""]
  params1 <- input[input_names %in% names(formals(homologar_db))]
  params1_not1 <- c("diccionario_columnas", "codigos_comunales", "arbol_motivo_legal",
                    "datos_sii")
  params1_not2 <- c(params1_not1, "verbose", "full_output")

  iter <- 1
  datal <- list()
  arboles <- list()
  for (i in data) {
    if (iter == 1) {
      temp <- do.call("homologar_db", c(list(db=i), params1))
    } else {
      temp <- do.call("homologar_db", c(list(db=i,
                                             diccionario_columnas=temp$diccionario_columnas,
                                             codigos_comunales=temp$codigos_comunales,
                                             arbol_motivo_legal=temp$arbol_motivo_legal,
                                             datos_sii=temp$datos_sii),
                                        params1[!names(params1) %in% params1_not1]))
    }
    datal[[i]] <- temp$tabla
    arboles[[i]] <- temp$arbol_a_corregir
    iter <- iter + 1
  }
  dbff_ <- do.call("homologar_db", c(list(db=base,
                                          diccionario_columnas = 'skip',
                                          codigos_comunales = 'skip',
                                          arbol_motivo_legal=temp$arbol_motivo_legal,
                                          datos_sii=temp$datos_sii,
                                          verbose=TRUE,
                                          full_output=FALSE),
                                     params1[!names(params1) %in% params1_not2]))
  dbf_ <- dbff_$tabla

  arboles_corregir <- rbindlist(c(list(dbff_$arbol_a_corregir), arboles))
  if (nrow(arboles_corregir) > 0) {
    # arboles_corregir <- data.table::fread("arbol_mercado_a_acorregir.csv"); arboles_corregir[, Inicio:=as.POSIXct(Inicio)]; arboles_corregir[, Termino:=as.POSIXct(Termino)]
    bynames <- setdiff(names(arboles_corregir), c("Numero", "Inicio", "Termino"))
    arboles_corregir2 <- arboles_corregir[, list(Numero=sum(Numero), Inicio=min(Inicio), Termino=max(Termino)), by=bynames]
    fwrite(arboles_corregir2, "arbol_mercado_a_corregir.csv")
  }

  # dbfM <- consolidar_db(dbf_, grabar=F, nuevo1, nuevo2)
  dbfM <- do.call("consolidar_db", c(list(db=dbf_, grabar=F), unname(datal)))
  depurar_ruts(dbfM, criterio='reciente')
}

#' @encoding UTF-8
#' @title Homologa datos de entrada al formato apropiado
#'
#' @description Permite homologar nombres de columnas, comunas, motivos legales; también selecciona columnas de interés,
#' y añade datos del SII. La tabla queda lista para ser añadida a la tabla general de datos. La función trata de ser lo más
#' inteligente posible, pero es necesario mantener los formatos apropiados.
#'
#' @param db ruta al archivo, en excel o csv (formato especial flexible).
#' @param diccionario_columnas ruta al archivo de diccionario de columnas en csv/excel (formato especial estricto).
#' @param codigos_comunales ruta al archivo de codigos y nombres comunales en csv/excel (formato especial estricto). Si 'skip', se salta este paso.
#' @param arbol_motivo_legal ruta al archivo de equivalencias de motivos legales en csv/excel (formato especial estricto).
#' @param datos_sii ruta al archivo de datos del SII, en csv (formato especial estricto).
#' @param verbose si TRUE, imprime estados de avance.
#' @param full_output si TRUE, entrega todas las tablas usadas. Si FALSE, entrega solo la tabla principal modificada.
#' @param mercados_de_interes nombre de `proveedor_mercado_nombre` de interés. Por defecto es FINANCIEROS, SEGUROS, SALUD y PREVISION.
#' @param drop porcentaje (de 0 a 1) de NAs tolerables antes de descartar la fila (solo si hay csv).
#' Por defecto se usa el 80% (si hay 100 colunas y 80 o más de ellas tienen valores faltantes, se descarta).
#'
#' @details Notar que en el caso de que `db` sea un csv, se hace un revisión de valores faltantes.
#' Si estos superan o son iguales al 80% (se puede cambiar, modificando el parámetro `drop`) del número de columnas, la fila será descartada.
#'
#' @importFrom readxl read_excel
#' @importFrom data.table data.table setnames fread tstrsplit copy fwrite
#' @importFrom utils tail
#' @return
#' @export
#'
homologar_db <- function(db,
                         diccionario_columnas=NULL,
                         codigos_comunales=NULL,
                         arbol_motivo_legal=NULL,
                         datos_sii=NULL,
                         verbose=TRUE,
                         full_output=TRUE,
                         mercados_de_interes=c("FINANCIEROS", "SEGUROS", "SALUD", "PREVISION"),
                         drop=.8) {

  #-------------- 1. Nombres y seleccion
  check_dates <- FALSE
  if (is.null(db)) {
    #--- cargar de base de datos
  } else {
    if (verbose) cat("Leyendo base de datos\n")
    if (is.character(db)){
      if (endsWith(db, ".csv")) {
        dat <- fread(db)
        check_dates <- TRUE
        outl <- rowSums(is.na(dat) | dat == '') >= ncol(dat)*drop # hasta un 80% de columnas con NA
        if (any(outl)) {
          dat <- dat[!outl, ]
        }
      } else {
        dat <- data.table(read_excel(db))
      }
    } else {
      dat <- copy(data.table(db))
    }
  }

  no_skip <- TRUE
  if (is.null(diccionario_columnas)) {
    #--- cargar de base de datos
  } else {
    if (is.character(diccionario_columnas)) {
      if (diccionario_columnas == "skip"){
        no_skip <- FALSE
      } else {
        if (endsWith(diccionario_columnas, ".csv")) {
          col_dict <- fread(diccionario_columnas)
        } else {
          col_dict <- data.table(read_excel(diccionario_columnas))
        }
      }
    } else {
      col_dict <- copy(data.table(diccionario_columnas))
    }
  }

  uniq_cols <- c("caso_cierre_fecha", "caso_creacion_fecha", "categoria_motivo_legal",
                 "cierre_corto", "consumidor_genero", "consumidor_id", "caso_numero",
                 "estado_caso_nombre", "mercado_tipo_producto_nombre", "motivo_legal_descripcion",
                 "proveedor_mercado_categoria_nombre", "proveedor_mercado_nombre",
                 "proveedor_nombre_fantasia", "proveedor_rut", "reclamo_descripcion")
  if ("consumidor_comuna" %in% colnames(dat)){
    uniq_cols <- c(uniq_cols, "consumidor_comuna")
  } else {
    uniq_cols <- c(uniq_cols, "cut_comuna")
  }
  if (no_skip) {
    if (any(duplicated(colnames(dat)))){
      warning("Columnas duplicadas, podría generar efectos indeseados: ", paste(names(dat)[duplicated(colnames(dat))], collapse=", "))
    }
    nombres_comunes <- intersect(names(dat), col_dict$original)
    setnames(dat, col_dict[original %in% nombres_comunes]$original, col_dict[original %in% nombres_comunes]$nuevo)
    uniq_cols <- unique(col_dict[Uso==1, c("nuevo", "Uso")])$nuevo
  }

  # quedarse con las columnas que se necesitan
  dt2 <- dat[, ..uniq_cols]
  if (ncol(dt2) != length(uniq_cols)) {
    stop("No se han encontrado las columnas necesarias.")
  }

  #------ check dates
  if (check_dates) {
    formatos <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M", "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M", "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M")
    dt2[caso_cierre_fecha=='', caso_cierre_fecha:=NA]
    dt2[, caso_cierre_fecha:=as.POSIXct(caso_cierre_fecha, tryFormats=formatos)]
    dt2[caso_creacion_fecha=='', caso_creacion_fecha:=NA]
    dt2[, caso_creacion_fecha :=as.POSIXct(caso_creacion_fecha, tryFormats=formatos)]
  }

  #------------------------------------------------------------------------------------------------
  #------ 2. Seleccionar mercados que me interesan
  if (!is.null(mercados_de_interes)) {
    dt2 <- dt2[proveedor_mercado_nombre %in% mercados_de_interes]
  }

  #------------------------------------------------------------------------------------------------
  #------ 3. Asignar rut a proveedores faltantes (por mayoría) y nombres de razón social por rut de mayoría
  #--- si rut es string...
  #20200610: ahora se hace por fuera, en un paso extra
  if (!is.numeric(dt2$proveedor_rut)){
    dt2[, proveedor_rut:=tstrsplit(proveedor_rut, "-", fixed=TRUE, keep=1L )]
    dt2[, proveedor_rut:=as.numeric(proveedor_rut)] # si lo hago con la línea anterior, no funciona
  }
  dt3 <- dt2

  #------------------------------------------------------------------------------------------------
  #----- 4. Revisar comuna
  no_skip <- TRUE
  if (is.null(codigos_comunales)) {
    # download.file("http://datos.energiaabierta.cl/rest/datastreams/250790/data.xls?applyFormat=1",
    #               "DATA/codigos_comunales.xlsx")
    #--- cargar de base de datos
  } else {
    if (verbose) cat("Leyendo archivo de comunas\n")
    if (is.character(codigos_comunales)) {
      if (codigos_comunales == "skip"){
        no_skip <- FALSE
      } else {
        if (endsWith(codigos_comunales, ".csv")) {
          comunas_cod <- fread(codigos_comunales)
        } else {
          comunas_cod <- data.table(read_excel(codigos_comunales))
        }
      }
    } else {
      comunas_cod <- copy(data.table(codigos_comunales))
    }
  }

  if (no_skip) {
    comunas_cod[, comunaH:=iconv(tolower(nombre_comuna_dl2339), from="UTF-8", to="ASCII//TRANSLIT")] # quitar tildes y cosas raras
    dt3[, comunaH:=iconv(tolower(consumidor_comuna), from="UTF-8", to="ASCII//TRANSLIT")]

    # x1 <- sort(setdiff(comunas_cod$comunaH, dt3$comunaH))
    x2 <- sort(setdiff(dt3$comunaH, comunas_cod$comunaH))
    ## 20200714 - realizado swap NA con lima?
    repl <- setNames(c("aisen", "concon", "coihaique", "curico", "calera", NA, "marchihue", "o'higgins", "paiguano",
                       "natales", "porvenir", "puqueldon", "san javier", "san vicente"),
                     c("aysen", "con con", "coyhaique", "curico", "la calera", "lima", "marchigue", "ohiggins", "paihuano",
                       "puerto natales", "puerto porvenir", "puquelden", "san javier de loncomilla", "san vicente de tagua tagua"))
    dt3[comunaH %in% x2, comunaH:=repl[comunaH]]
    setdiff(comunas_cod$comunaH, dt3$comunaH) # está bien!
    setdiff(dt3$comunaH, comunas_cod$comunaH)
    dt4 <- merge(dt3, comunas_cod[, c("comunaH", "cut_comuna")], by="comunaH", all.x=T)
    dt4[, c("comunaH", "consumidor_comuna"):=NULL]
  } else {
    dt4 <- copy(dt3)
  }
  dt4[, cut_provincia:=substr(cut_comuna, 1, 3)]
  dt4[, cut_region:=substr(cut_comuna, 1, 2)]

  #------------------------------------------------------------------------------------------------
  #------ 5. Motivos legales
  if (is.null(arbol_motivo_legal)){
    dt5 <- dt4
    arbol_corregir <- NULL
    arbol <- NULL
  } else {
    if (verbose) cat("Leyendo motivos legales\n")
    if (is.character(arbol_motivo_legal)){
      if (endsWith(arbol_motivo_legal, ".csv")) {
        farbol <- homologar_arbol(fread(arbol_motivo_legal))
      } else {
        farbol <- homologar_arbol(data.table(read_excel(arbol_motivo_legal, "ACTUAL")))
      }
      arbol <- farbol$arbol
    } else {
      arbol <- copy(data.table(arbol_motivo_legal))
    }
    dt5 <- merge(dt4, arbol,
                 by=c("proveedor_mercado_nombre", "motivo_legal_descripcion", "categoria_motivo_legal"),
                 all.x=T, sort=F)
    arbol_corregir <- dt5[is.na(`PROPUESTA DE FUSION MOTIVO LEGAL`) & is.na(`PROPUESTA DE FUSION CATEGORIA LEGAL`)]
    if (nrow(arbol_corregir) > 0) {
      arbol_corregir <- arbol_corregir[, list(Numero=.N, Inicio=min(caso_creacion_fecha), Termino=max(caso_creacion_fecha)),
                                       by=c("proveedor_mercado_nombre", "motivo_legal_descripcion", "categoria_motivo_legal")]
    } else {
      arbol_corregir <- NULL
    }
    dt5[, c("motivo_legal_descripcion", "categoria_motivo_legal"):=NULL]
    setnames(dt5, c("PROPUESTA DE FUSION MOTIVO LEGAL", "PROPUESTA DE FUSION CATEGORIA LEGAL"), c("motivo_legal_descripcion", "categoria_motivo_legal"))
  }

  #------------------------------------------------------------------------------------------------
  # 6. SII data

  if (is.null(datos_sii)) {
    #--- cargar de base de datos? No, no hacer nada
    dt6 <- dt5
    siiData <- NULL
  } else {
    if (verbose) cat("Leyendo datos del SII\n")
    if (is.character(datos_sii)){
      siiData <- homologar_sii(fread(datos_sii, encoding="Latin-1"))
    } else {
      siiData <- copy(data.table(datos_sii))
    }
    ruts_sii <- siiData[siiData[, .I[which.max(periodo)], by=c("rut")]$V1, c("rut", "periodo", "tramo_ventas")]
    # ruts_sii[, list(N=.N), by=tramo_ventas][order(tramo_ventas)]
    setnames(ruts_sii, c("rut"), c("proveedor_rut"))
    dt6 <- merge(dt5, ruts_sii[, c("proveedor_rut", "tramo_ventas")], all.x=TRUE, sort=FALSE)
  }

  #------------------------------------------------------------------------------------------------
  # 7. Calcular columnas
  acogidos <- c("POLAR ACOGE PROPUESTA", "PROVEEDOR ACOGE") #, "RESPUESTA PARCIAL CONSULTADA A LOS CONSUMIDORES")
  no_acogidos <- setdiff(unique(dt6$cierre_corto), acogidos)
  dt6[, reclamo_acogido:=FALSE]
  dt6[is.na(cierre_corto), reclamo_acogido:=NA]
  dt6[cierre_corto %in% acogidos, reclamo_acogido:=TRUE]
  # dt6[, list(N=.N), by=cierre_corto]
  # dt6[, list(N=.N), by=reclamo_acogido]

  if (full_output) {
    return(list(tabla=dt6, diccionario_columnas=col_dict, codigos_comunales=comunas_cod,
                arbol_motivo_legal=arbol, datos_sii=siiData, arbol_a_corregir=arbol_corregir))
  } else {
    return(list(tabla=dt6, arbol_a_corregir=arbol_corregir))
  }
}

#' @encoding UTF-8
#' @title Homologa el árbol de mercados
#'
#' @description Permite realizar homologaciones y rellenar valores faltantes en las categorías de daño y facilidad de prueba
#' @param arbol_motivo_legal data.frame de data.table con la información
#'
#' @return
#' @export
#'
homologar_arbol <- function(arbol_motivo_legal) {
  mycols <- c("proveedor_mercado_nombre", "PROPUESTA DE FUSION MOTIVO LEGAL", "PROPUESTA DE FUSION CATEGORIA LEGAL")

  #-- Obteniendo únicos
  # para los que tienen grado de daño sin NA
  uni <- arbol_motivo_legal[, #!is.na(`Grado de daño`),
                            list(N=sum(`Numero`)),
                            by=c("Grado de dano", "Facilidad de prueba", mycols)]
  # para los que tienen grado de daño con NA
  uni2 <- arbol_motivo_legal[!is.na(`Grado de dano`),
                             list(N=sum(`Numero`)),
                             by=c("Grado de dano", "Facilidad de prueba", mycols[-1])]
  uni <- uni[order(proveedor_mercado_nombre, `PROPUESTA DE FUSION MOTIVO LEGAL`, `PROPUESTA DE FUSION CATEGORIA LEGAL`, -N)]
  setnames(uni, enc2utf8(names(uni)))
  dups <-  duplicated(uni, by = mycols)
  uni[, duplicado := as.numeric(dups) | c(tail(dups, -1), FALSE)]

  #-- Me quedo con la categoría que tenga más valores (max)
  unif <- uni[uni[, .I[which.max(N)], by=mycols]$V1, ]
  unif2 <- uni2[uni2[, .I[which.max(N)], by=eval(mycols[-1])]$V1, ]

  #-- Fusión con el total
  unif[, c("N", "duplicado"):=NULL]
  unif2[, c("N"):=NULL]
  temp <- merge(unif, unif2, by=c("PROPUESTA DE FUSION MOTIVO LEGAL", "PROPUESTA DE FUSION CATEGORIA LEGAL"),
                all.x=T)
  setnames(temp, c("Grado de dano.x", "Facilidad de prueba.x", "Grado de dano.y", "Facilidad de prueba.y"),
           c("grado_perjuicio", "facilidad_prueba", "grado", "facilidad"))
  temp[is.na(grado_perjuicio), c("grado_perjuicio", "facilidad_prueba") := list(grado, facilidad)]
  # temp[`PROPUESTA DE FUSIÓN MOTIVO LEGAL` == 'PROBLEMAS EJECUCIÓN CONTRATO' & `PROPUESTA DE FUSIÓN CATEGORIA LEGAL`=="NEGATIVA DEL PROVEEDOR PARA CUMPLIR SU OBLIGACIÓN"]

  arbol_motivo_legal[, c("Grado de dano", "Facilidad de prueba", "...4", "...15"):=NULL]
  arbol <- merge(arbol_motivo_legal, temp, by=mycols, all.x=T)
  arbol_out <- copy(arbol)
  arbol[, c("F", "Numero", "Inicio", "Termino", "presente?", "grado", "facilidad", "ID"):=NULL]

  return(list(arbol=arbol, arbol_completo=arbol_out))
}

#' @encoding UTF-8
#' @title Homologa datos del SII
#'
#' @description Permite homologar nombres de comunas y revisar algunas incosistencias
#' @param siiData data.frame de data.table con la información
#'
#' @return
#' @export
#'
homologar_sii <- function(siiData) {
  # codCom <- comunas_cod
  #------- ver sernac_01
  siiData[, c("f_inicio", "f_termino") := list(as.Date(f_inicio, f_termino))]
  siiData[, periodo:=as.numeric(periodo)]
  # codCom[, comunaH:=iconv(tolower(nombre_comuna_dl2339), from="UTF-8", to="ASCII//TRANSLIT")]
  # siiData[, comunaH:=iconv(tolower(comuna), from="UTF-8", to="ASCII//TRANSLIT")]
  # x1 <- sort(setdiff(codCom$comunaH, siiData$comunaH))
  # x2 <- sort(setdiff(siiData$comunaH, codCom$comunaH)[-1])
  #
  # print(x1)
  # print(x2)
  # siiData[comunaH %in% x2, comunaH:=setNames(x1, x2)[comunaH]]
  # setdiff(siiData$comunaH, codCom$comunaH) # está bien!
  # siiData[comunaH == "sin informacion", comunaH:=NA]
  # siiData <- merge(siiData, codCom[, c("comunaH", "cut_comuna")], by="comunaH", all.x=T)

  siiData[trabajadores_informados %% 1 != 0, trabajadores_informados:=trabajadores_informados*1000]

  return(siiData)
}

#' @encoding UTF-8
#' @title Homologa los ruts y su nombre de fantasía (dentro de lo posible).
#'
#' @param db db previamente homologada con \link{homologar_db}.
#' @param tolerancia número de nombres de fantasía por rut máximo para asignar por mayoría, antes de considerar el registro como extraño.
#' Ejemplo, cuando no se sabe el rut de la empresa y se rellena con 111111, etc.
#' @param criterio citerio de asignación del nombre de fantasía al rut. Valores posibles son 'mayoria' o 'reciente'. Por defecto 'reciente'.
#'
#' @importFrom data.table tstrsplit
#' @export
#'
depurar_ruts <- function(db, tolerancia=20, criterio='reciente'){
  if (!is.numeric(db$proveedor_rut)){
    db[, proveedor_rut:=tstrsplit(proveedor_rut, "-", fixed=TRUE, keep=1L )]
    db[, proveedor_rut:=as.numeric(proveedor_rut)] # si lo hago con la línea anterior, no funciona
  }
  # dt2[, list(N=.N), by=c("proveedor_rut", "proveedor_nombre_fantasia")]

  #-- hay ruts NA??
  if (any(is.na(db$proveedor_rut))) {
    warning("Hay ruts con NA")
  }

  #--- Nombres por rut de mayoría
  if (criterio == 'mayoria') {
    rutNombreMaj <- obtener_mayoria(db, c("proveedor_rut", "proveedor_nombre_fantasia"), "proveedor_nombre_fantasia")
    rutMercadoMaj <- obtener_mayoria(db, c("proveedor_rut", "proveedor_mercado_nombre"), "proveedor_mercado_nombre")
    rutMercadoCatMaj <- obtener_mayoria(db, c("proveedor_rut", "proveedor_mercado_categoria_nombre"), "proveedor_mercado_categoria_nombre")
  } else if (criterio == 'reciente') {
    rutNombreMaj <- obtener_reciente(db, c("proveedor_rut", "proveedor_nombre_fantasia"), "proveedor_nombre_fantasia")
    rutMercadoMaj <- obtener_reciente(db, c("proveedor_rut", "proveedor_mercado_nombre"), "proveedor_mercado_nombre")
    rutMercadoCatMaj <- obtener_reciente(db, c("proveedor_rut", "proveedor_mercado_categoria_nombre"), "proveedor_mercado_categoria_nombre")
  }
  db[, c("proveedor_nombre_fantasia", "proveedor_mercado_nombre", "proveedor_mercado_categoria_nombre") := NULL]
  db2 <- merge(merge(merge(db, rutNombreMaj, sort=F), rutMercadoMaj, sort=F), rutMercadoCatMaj, sort=F)

  return(db2)
}


#' @encoding UTF-8
#' @title Consolidar base de datos
#'
#' @description Agrega nuevos registros a la base de datos de inicio.
#'
#' @param db db previamente homologada con \link{homologar_db}.
#' @param grabar si es TRUE, la unión de las bases de datos se guarda como un nuevo archivo actualizado.
#' Se recomienda utilizarlo sólo cuando se esté con certeza de que los valores no cambiarán. Por defecto es FALSE.
#' @param ... base o bases de datos a agregar, previamente homologadas con \link{homologar_db}.
#'
#'#' @importFrom data.table rbindlist duplicated setorder
#' @export
#'
consolidar_db <- function(..., db, grabar=FALSE){
  nuevos <- list(...)
  todos <- c(list(db), nuevos)
  todos <- lapply(1:length(todos), function(i) todos[[i]][, numero_db:=i])
  if (is.list(db)){
    ans <- rbindlist(todos, use.names=T, fill=T)
  }
  setorder(ans, caso_creacion_fecha)
  ans[, duplicados:=duplicated(caso_numero)]
  dupl <- unlist(unique(ans[!is.na(caso_numero) & duplicados, "caso_numero"]))
  if (length(dupl) > 0) {
    preservar <- ans[caso_numero %in% dupl, list(numero_db=which.max(numero_db)), by=caso_numero]
    ans <- rbindlist(list(ans[!caso_numero %in% preservar$caso_numero, ], ans[caso_numero %in% preservar$caso_numero & numero_db %in% preservar$numero_db, ]))
    setorder(ans, caso_creacion_fecha)
  }
  ans[, c("duplicados", "numero_db"):=NULL]
  if (grabar){
    print('No graba')
  }
  return(ans)
}
# data.table(caso_numero=c(1:10, 8:10), numero_db=c(rep(1, 10), 2, 2, 2))
# guardar_db <- function(x)
