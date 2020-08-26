#' @encoding UTF-8
#' @title Genera un dashboard con shiny
#'
#' @param obj Objeto de la clase reclamos.
#'
#' @importFrom plotly renderPlotly plotlyOutput plot_ly layout config
#' @importFrom data.table setorder
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom shiny shinyApp runApp fluidRow column tableOutput renderTable selectizeInput selectInput renderUI HTML htmlOutput icon h3
#' @import shinydashboard
#' @export
#'
shinyPlot <- function(obj) {
  if (!"reclamos" %in% class(obj)) stop("Objeto db debe ser de la clase reclamos")
  if (!obj$estado$reporte) stop("Primero debe extraer la serie con reportar")
  ip <- system("ipconfig", intern=TRUE)
  port <- 6742

  #--------------------------- MAIN FUNCTIONS -----------------------------------------------------------------#
  plotSeries1 <- function(ind, data, upDir, figid, title="Ranking ascendente de alertas (top %s)", xlab="Tiempo", ylab="Número de reclamos") {
    fechas <- ind$dates
    clases <- ind$class
    sele <- data$reporte$tops[clase %in% clases & t_observado %in% as.Date(fechas) & up == upDir]
    setorder(sele, posicion)
    cols <- c(data$Clases[1:which(clases == names(data$Clases))], 'variable', 'variable_valor')
    what <- unlist(sele[, do.call(paste, c(.SD, sep=" | ")), .SDcols=cols])
    what <- factor(what, levels=what)
    # mdat la serie completa de reclamos
    mdat <- data$reclamos[[clases]]$datos[metrics=='N' & t <= as.Date(fechas)]
    mdat[, supid:=factor(do.call(paste, c(.SD, sep=" | ")), levels=what), .SDcols=cols]
    mdat <- mdat[supid %in% what]
    # mdat2 es para agregar puntaje
    mdat2 <- data$reporte$serie[clase %in% clases & t_observado %in% as.Date(fechas) & t %in% as.Date(fechas) & up == upDir,
                                c("posicion", cols, 'outlier_scorein', 'outlier_scoreex'), with=F]
    mdat2 <- setorder(unique(mdat2), posicion)
    mdat2[, supid:=factor(do.call(paste, c(.SD, sep=" | ")), levels=what), .SDcols=cols]
    mdat2 <- mdat2[supid %in% what]
    if (nrow(sele) != nrow(mdat2)) stop("Incosistencia de dimensiones. Revisar la asignación de puntaje y número de ranking en plotly.")
    levels(mdat$supid) <- sprintf('%s. [%s, %s] %s', 1:length(levels(mdat$supid)),
                                         round(mdat2$outlier_scorein, 2),
                                         round(mdat2$outlier_scoreex, 2),
                                         levels(mdat2$supid))
    xrange <- range(mdat$t, na.rm=T)
    yrange <- range(mdat$values_norm, na.rm=T)
    pl <- plot_ly(mdat, x = ~t, y=~values_norm, color =~supid, type="scatter", mode = "line",
                  colors = colorRampPalette(c("#a50026", "#ffff33", "#1f78b4"))(nrow(sele)) )
    ly <- layout(pl, title = sprintf(title, nrow(sele)),
                 xaxis = list(range = xrange, title = xlab),
                 yaxis = list(range = yrange, title = ylab),
                 showlegend=TRUE, legend = list(font = list(size = 10))
          )
    # hide_show_legend <- list(
    #   name = "Hide/Show Legend",
    #   icon = list(path="M 30 10 V 60 L 35 0 L 0 -10 L -25 0 L 0 -50 L -10 0 Z",
    #               transform = 'matrix(1 0 0 1 -2 -2) scale(0.7)',
    #               options = 'animationData: animationData.default'),
    #   click = htmlwidgets::JS(
    #     sprintf("function(gd){
    #       gd.layout.showlegend = !gd.layout.showlegend;
    #       Plotly.relayout('%s', gd);
    #     }", figid)
    #   )
    # )
    config(ly, displaylogo = FALSE) #, modeBarButtonsToAdd = list(octocat))
  }

  plotSeries2 <- function(ind, data, upDir, title="Categoría: %s | %s", xlab="Tiempo", ylab="Número") {
    fechas <- ind$dates
    clases <- ind$class
    pos <- ind$rank_pos
    db <- data$reporte
    mdat <- db$serie[posicion %in% pos & clase %in% clases & t_observado %in% as.Date(fechas) & up == upDir]
    xrange <- range(mdat$t, na.rm=T)
    yrange <- range(mdat$value, na.rm=T)
    # mdat[is.na(values), values:=0]
    # linea por el 0y linea de tendencia
    pl <- plot_ly(mdat, x = ~t, y=~values, color =~metrics, type="scatter", mode = "line", colors = "Paired")
    ly <- layout(pl, title = sprintf(title, unique(mdat$variable), unique(mdat$variable_valor)),
                 xaxis = list(range = xrange, title = xlab), hovermode = "x unified",
                 yaxis = list(range = yrange, title = ylab))
    config(ly, displaylogo = FALSE)
  }

  #--------------------------- SERVER -----------------------------------------------------------------#
  server <- function(input, output, session) {

    output$caption <- renderUI({
      # fechas <- "2020-04-15"; clases <- "industria"; pos <- 1; obj <- outt
      fechas <- input$dates
      clases <- input$class
      pos <- input$rank_pos
      db <- obj$reporte
      mdat <- db$serie[posicion %in% pos & clase %in% clases & t_observado %in% as.Date(fechas) & t %in% as.Date(fechas)]
      ups <- c()
      dows <- c()
      ups_score <- NA
      dows_score <- NA
      for (i in names(obj$Clases)) {
        for (cond in c(T, F)) {
          tp_ <- unique(mdat[up == cond, c(obj$Clases[i], 'outlier_scorein', 'outlier_scoreex'), with=F])
          if (nrow(tp_) > 0) {
            if (!is.na(tp_)[1]){
              if (cond) {
                ups <- c(ups, sprintf("<b>%s</b> [<i>%s</i>]: %s", i, obj$Clases[i], paste0(tp_[, 1], collapse=", ")))
                if (is.na(ups_score)[1]) {
                  ups_score <- round(unlist(c(tp_[, 2], tp_[, 3])), 3)
                }
              } else {
                dows <- c(dows, sprintf("<b>%s</b> [<i>%s</i>]: %s", i, obj$Clases[i], paste0(tp_[, 1], collapse=", ")))
                if (is.na(dows_score)[1]) {
                  dows_score <- round(unlist(c(tp_[, 2], tp_[, 3])), 3)
                }
              }
            }
          }
        }
      }

      if (!is.null(ups)) {
        upsc <- paste(sprintf("<b><ins>Alertas ascendentes</ins> [puntaje interno: %s| puntaje externo %s]:</b>", ups_score[1], ups_score[2]),
                      paste0(ups, collapse=" || "), sep="<br/>")
      } else {
        upsc <- ""
      }
      if (!is.null(dows)) {
        dowsc <- paste(sprintf("<b><ins>Alertas descendentes</ins> [puntaje interno: %s| puntaje externo %s]:</b>", dows_score[1], dows_score[2]),
                       paste0(dows, collapse=" || "), sep="<br/>")
      } else {
        dowsc <- ""
      }

      out <- paste(upsc, dowsc, sep="<br/>")
      HTML(out)
    })

    output$clases <- renderTable({
      (data.frame(Nombre=names(obj$Clases), Clase=obj$Clases))
    },striped=T, bordered=T, align='c')

    output$categorias <- renderTable({
      (data.frame(Categorias=sort(obj$Categorias)))
    })

    getRuts <- reactive({
      mruts <- c()
      if ('proveedor_rut' %in% names(obj$reporte$tops)){
        mruts <- unique(c(obj$reporte$tops$proveedor_rut, mruts))
      }
      if ('proveedor_rut' %in% obj$reporte$tops$variable){
        mruts <- unique(c(obj$reporte$tops[variable=='proveedor_rut', variable_valor], mruts))
      }
      datatable(buscar_ruts(sort(mruts), obj), options=list(pageLength=3,
                                                            lengthMenu=c(1, 3, 5, 10)))
    })

    output$ruts1 <- renderDataTable(getRuts())
    output$ruts2 <- renderDataTable(getRuts())

    output$plotl <- renderPlotly({
      plotSeries2(input, obj, upDir=FALSE,
                  title="Categoría: %s | %s",
                  xlab="Tiempo", ylab="Número")
    })

    output$plotr <- renderPlotly({
      plotSeries2(input, obj, upDir=TRUE,
                  title="Categoría: %s | %s",
                  xlab="Tiempo", ylab="Número")
    })

    output$rankplotr <- renderPlotly({
      plotSeries1(input, obj, upDir = TRUE, figid="rankplotr",
                  title="Ranking ascendente de alertas (top %s)",
                  xlab="Tiempo",
                  ylab="Número de reclamos (estandarizado)")
    })

    output$rankplotl <- renderPlotly({
      plotSeries1(input, obj, upDir = FALSE, figid="rankplotl",
                  title="Ranking descendentes de alertas (top %s)",
                  xlab="Tiempo",
                  ylab="Número de reclamos (estandarizado)")
    })
  }


  #--------------------------- UI -----------------------------------------------------------------#
  ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = 'Alertas SERNAC', disable = F),
    dashboardSidebar(
      title = sprintf("Visible en: %s", sprintf("%s:%s", gsub(".*? ([[:digit:]])", "\\1", ip[grep("IPv4", ip)][[1]]), port)),

      selectInput(
        'dates', '1. Fechas observadas', choices = obj$atributos$ranking$dates,
        selectize = FALSE, selected = obj$atributos$ranking$dates[1]
      ),
      selectizeInput(
        'class', '2. Clase de agrupamiento', selected = 1,
        choices = names(obj$Clases), options = list(maxItems = 1)
      ),
      selectizeInput(
        'rank_pos', '3. Posición del ranking', choices = 1:obj$atributos$ranking$topn,
        selected = 1
      ),
      sidebarMenu(
        menuItem("Ranking", tabName = "general", icon = icon("chart-line")),
        menuItem("Detalle alertas", tabName = "detalle", icon = icon("info-circle"))
      )
    ),

    dashboardBody(
      tabItems(
        tabItem(tabName = "general",
          fluidRow(
            box(
              title="Buscador de ruts", collapsible=T, status = "warning",
              dataTableOutput('ruts1'), width=12
            )
          ),
          fluidRow(
            box(
              title="Alertas ascendentes", status='danger', solidHeader = T, collapsible = F, # height=300,
              plotlyOutput('rankplotr'), width=12
            )
          ),
          fluidRow(
            box(
              title="Alertas descendentes", status='primary', solidHeader = T, collapsible = F,
              plotlyOutput('rankplotl'), width=12
            )
          )
        ),

        tabItem(tabName = "detalle",
          fluidRow(
            box(htmlOutput("caption"), width=12)
          ),

          fluidRow(
            box(
              title="Alertas descendentes", status='primary', solidHeader = T, collapsible = F,
              plotlyOutput('plotl')
            ),
            box(
              title="Alertas ascendentes", status='danger', solidHeader = T, collapsible = F, # height=300,
              plotlyOutput('plotr')
            )
          ),

          fluidRow(
            column(6, h3("Clases y categorías"),
                   box(tableOutput('clases')),
                   box(tableOutput('categorias'))
            ),
            column(6, h3("Buscador de ruts"),
                   box(dataTableOutput('ruts2'), width=12)
            )
          ),

          fluidRow(
            box(dataTableOutput('ranking'), width=12)
          )
        )
      )
    )
  )

  #--------------------------- APP -----------------------------------------------------------------#
  app <- shinyApp(ui = ui, server = server)
  runApp(app, port=port, host = getOption("shiny.host", "0.0.0.0"))
}
