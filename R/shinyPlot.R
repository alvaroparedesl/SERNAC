#' @encoding UTF-8
#' @title Genera un dashboard con shiny
#'
#' @param obj Objeto de la clase reclamos.
#'
#' @importFrom plotly renderPlotly plotlyOutput plot_ly layout
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom shiny shinyApp runApp fluidRow column tableOutput renderTable selectizeInput selectInput renderUI HTML htmlOutput
#' @import shinydashboard
#' @export
#'
shinyPlot <- function(obj) {
  if (!"reclamos" %in% class(obj)) stop("Objeto db debe ser de la clase reclamos")
  if (!obj$estado$reporte) stop("Primero debe extraer la serie con reportar")
  ip <- system("ipconfig", intern=TRUE)
  port <- 6742

  server <- function(input, output) {

    output$caption <- renderUI({
      fechas <- input$dates
      clases <- input$class
      pos <- input$rank_pos
      db <- obj$reporte
      mdat <- db$serie[posicion %in% pos & clase %in% clases & t_observado %in% as.Date(fechas)]
      ups <- c()
      dows <- c()
      for (i in names(obj$Clases)) {
        tp <- unique(mdat[up == T, obj$Clases[i], with=F])
        if (!is.na(tp)[1]){
          ups <- c(ups,   sprintf("<b>%s</b> [<i>%s</i>]: %s", i, obj$Clases[i], paste0(tp, collapse=", ")))
        }
        tp <- unique(mdat[up == F, obj$Clases[i], with=F])
        if (!is.na(tp)[1]){
          dows <- c(dows, sprintf("<b>%s</b> [<i>%s</i>]: %s", i, obj$Clases[i], paste0(tp, collapse=", ")))
        }
      }
      out <- paste("<b><ins>Alertas ascendentes:</b></ins>", paste0(ups, collapse=" || "), "<b><ins>Alertas descendentes:</b></ins>", paste0(dows, collapse=" || "), sep="<br/>")
      HTML(out)
    })
    output$clases <- renderTable({
      (data.frame(Nombre=names(obj$Clases), Clase=obj$Clases))
    },striped=T, bordered=T, align='c')
    output$categorias <- renderTable({
      (data.frame(Categorias=sort(obj$Categorias)))
    })
    output$ruts <- renderDataTable({
      mruts <- c()
      if ('proveedor_rut' %in% names(obj$reporte$tops)){
        mruts <- unique(c(obj$reporte$tops$proveedor_rut, mruts))
      }
      if ('proveedor_rut' %in% obj$reporte$tops$variable){
        mruts <- unique(c(obj$reporte$tops[variable=='proveedor_rut', variable_valor], mruts))
      }
      datatable(buscar_ruts(sort(mruts), obj), options=list(pageLength=5))
    })
    output$ranking <- renderDataTable({
      datatable(obj$reporte$tops)
    })
    output$plotl <- renderPlotly({
      fechas <- input$dates
      clases <- input$class
      pos <- input$rank_pos
      db <- obj$reporte
      mdat <- db$serie[posicion %in% pos & clase %in% clases & t_observado %in% as.Date(fechas) & up == FALSE]
      xrange <- range(mdat$t, na.rm=T)
      yrange <- range(mdat$value, na.rm=T)
      # mdat[is.na(values), values:=0]
      pl <- plot_ly(mdat, x = ~t, y=~values, color =~metrics, type="scatter", mode = "line", colors = "Paired")
      layout(pl, title = sprintf("Categoría: %s | %s", unique(mdat$variable), unique(mdat$variable_valor)),
               xaxis = list(range = xrange, title = "Tiempo"), hovermode = "x unified",
               yaxis = list(range = yrange, title = "Número"))
      # linea por el 0y linea de tendencia
    })
    output$plotr <- renderPlotly({
      fechas <- input$dates
      clases <- input$class
      pos <- input$rank_pos
      db <- obj$reporte
      mdat <- db$serie[posicion %in% pos & clase %in% clases & t_observado %in% as.Date(fechas) & up == TRUE]
      xrange <- range(mdat$t, na.rm=T)
      yrange <- range(mdat$value, na.rm=T)
      pl <- plot_ly(mdat, x = ~t, y=~values, color =~metrics, type="scatter", mode = "line", colors = "Paired")
      layout(pl, title = sprintf("Categoría: %s | %s", unique(mdat$variable), unique(mdat$variable_valor)),
               xaxis = list(range = xrange, title = "Tiempo"), hovermode = "x unified",
               yaxis = list(range = yrange, title = "Número"))
    })
  }

  ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = 'Semáforo SERNAC', disable = F),
    dashboardSidebar(
      title = sprintf("Visibile en: %s",
                      sprintf("%s:%s", gsub(".*? ([[:digit:]])", "\\1", ip[grep("IPv4", ip)]), port)),
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
      )
    ),
    dashboardBody(
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
        column(6,
               box(tableOutput('clases')),
               box(tableOutput('categorias'))
        ),
        column(6,
               box(dataTableOutput('ruts'), width=6)
        )
      ),

      fluidRow(
        box(dataTableOutput('ranking'), width=12)
      )
    )
  )

  app <- shinyApp(ui = ui, server = server)
  runApp(app, port=port, host = getOption("shiny.host", "0.0.0.0"))
}