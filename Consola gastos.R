#-------------------------------------------------------
# LIBRERIAS Y PAQUETES
#-------------------------------------------------------
install.packages(c(
  "svDialogs",   # ventanas de diálogo
  "dplyr",       # manipulación de datos
  "ggplot2",     # gráficos
  "scales",      # formato de ejes
  "shiny",       # framework web
  "bslib",       # temas Bootstrap para Shiny
  "shinyalert",  # alertas modales
  "shinyAce",    # editor de código
  "plotly",      # gráficos interactivos
  "gganimate",   # animaciones ggplot
  "gifski"       # renderizador de GIF para gganimate
))

library(shiny)
library(bslib)
library(shinyalert)
library(shinyAce)
library(ggplot2)
library(gganimate)
library(gifski)
library(scales)
library(dplyr)
#-------------------------------------------------------
# DEFINICION FUNCIONES
#-------------------------------------------------------
#Función que define los pesos colombianos como un número con texto
en_pesos <- function(x) {paste0( "$", format(  round(x), 
big.mark    = ".", decimal.mark = ",", scientific  = FALSE ) )}

#Funcion que permite leer un csv para validar datos y entrega un dataframe con columnas:
# Fecha, descripcion, categoria, monto, mes

cargar_gastos <- function(ruta) {
    if (!file.exists(ruta)) stop("Archivo no encontrado: ", ruta)
    df <- read.csv(ruta, stringsAsFactors = FALSE, encoding = "UTF-8")
    #Verifica que las tres columnas esperadas existan
    faltantes <- setdiff(c("fecha", "categoria", "monto"), names(df))
    if (length(faltantes) > 0) {stop("Faltan columnas: ", paste(faltantes, collapse = ", "))}
    #Convertimos las fechas para obtener el mes
    df$fecha <- as.Date(df$fecha)
    df$mes <- format(df$fecha, "%Y-%m")
    #Tener en cuenta los valores NA e ignorar los ingresos que se refieran a esos montos
    sin_monto <- sum(is.na(df$monto))
    if (sin_monto > 0) {
        warning("Hay ", sin_monto, " movimiento(s) sin monto.")
    df <- df[!is.na(df$monto), ]}
    #return
    df
    }

#Funcion que hace los calculos estadisticos
resumen <- function(df, presupuesto = 1500000) { 
    list(
    total = sum(df$monto),
    promedio = mean(df$monto),
    movimientos = nrow(df),
    excedido = sum(df$monto) > presupuesto,
    exceso = max(0, sum(df$monto) - presupuesto)
)}

#Funcion que agrupa por categoria y oorganiza de mayor a menor
por_categoria <- function(df) {df |> group_by(categoria) |>
    summarise(total       = sum(monto),
    movimientos = n(),
    .groups     = "drop") |>
    mutate(porcentaje = round(total / sum(total) * 100, 1)) |>
    arrange(desc(total))}

#-------------------------------------------------------
# CREACION UI
#-------------------------------------------------------

ui <- page_sidebar(
    title = "PlayProject R — Registro y control de gastos",
    theme = bs_theme(bootswatch = "sketchy"),

    sidebar = sidebar(h5("Información"),
    fileInput("archivo",
    "Subir CSV de gastos",
    accept      = ".csv",
    buttonLabel = "Elegir archivo",
    placeholder = "No se selecionó archivo" ),
    actionButton("usar_demo",
    "Cargar datos demo",
    class = "btn-outline-secondary btn-sm w-100"),hr(),
    h5("Presupuesto mensual"),
    numericInput("presupuesto",
    label = NULL,
    value = 1500000,
    min   = 0,
    step  = 50000), hr(),
    uiOutput("info_datos") ),

    navset_tab(
    nav_panel("Control de gastos",br(),
    uiOutput("tarjetas"),br(),
    plotOutput("grafico_categorias", height = "420px")),

    nav_panel("Resumen",br(),
    p("Evolución mensual del gasto por categoría.",
    em("(Tarda unos segundos en renderizar.)")),
    imageOutput("animacion", width = "100%", height = "520px")),

    )
)

#-------------------------------------------------------
# CREACION SERVER
#-------------------------------------------------------
server <- function(input, output, session) {
    datos_store <- reactiveVal(NULL)
    observeEvent(input$archivo, {
    req(input$archivo)
    df <- tryCatch(cargar_gastos(input$archivo$datapath),
    error = function(e) {
        shinyalert("No se puede leer el archivo", conditionMessage(e), type = "error")
        NULL},

    warning = function(w) {
        showNotification(conditionMessage(w), type = "warning", duration = 6)
        suppressWarnings(cargar_gastos(input$archivo$datapath))}
    )
    datos_store(df) })

    #Boton de cargar el csv de ejemplo
    observeEvent(input$usar_demo, {
    df <- tryCatch(
    cargar_gastos("data/gastos_demo.csv"),
    error = function(e) {
        shinyalert("Error con datos demo", conditionMessage(e), type = "error")
        NULL})
    datos_store(df)
    if (!is.null(df)) {
      showNotification("Datos ejemplo cargados: 6 meses, 8 categorías.",type = "message", duration = 4)
    }})


    output$info_datos <- renderUI({
    df <- datos_store()
    if (is.null(df)) return(p(em("No hay datos cargados.")))
    r <- resumen(df, input$presupuesto)
    tagList(
      p(em(paste0(r$movimientos, " Movimientos | ",
                  length(unique(df$mes)), " mes(es)"))),
      p(em(paste0("Total: ", en_pesos(r$total))))
    )})

observeEvent(datos_store(), {
    df <- datos_store()
    req(df)
    r <- resumen(df, input$presupuesto)
    if (r$excedido) {
    showModal(modalDialog(
        title    = paste0("Exceso en gastos: ", en_pesos(r$exceso)),
        tableOutput("tabla_modal"),
        p("Categorías de la mayor parte del gasto."),
        easyClose = TRUE,
        footer    = modalButton("Entendido")))
    }})


output$tabla_modal <- renderTable({
    req(datos_store())
    head(por_categoria(datos_store()), 3)})

# Definir metricas para realizar los graficos necesarios

    output$tarjetas <- renderUI({
    df <- datos_store()
    if (is.null(df)) {return(p(class = "text-muted",
               "Selecciona un CSV o usa el ejemplo para ver el tablero."))}

    r <- resumen(df, input$presupuesto)

    fluidRow(
        column(3, div(class = "card border-primary mb-3",
        div(class = "card-body",
          h6(class = "text-primary", "Total de gastos"),
          h4(en_pesos(r$total)))
      )),

        column(3, div(class = "card border-info mb-3",
        div(class = "card-body",
          h6(class = "text-info", "Promedio por movimiento"),
          h4(en_pesos(r$promedio)))
      )),

        column(3, div(class = "card border-secondary mb-3",
        div(class = "card-body",
          h6(class = "text-secondary", "Movimientos"),
          h4(r$movimientos))
      )),

        column(3, div(
        class = if (r$excedido) "card border-danger mb-3"
                else            "card border-success mb-3",
        div(class = "card-body",
          h6(class = if (r$excedido) "text-danger" else "text-success",
             if (r$excedido) "Exceso" else "Disponible"),
          h4(if (r$excedido) en_pesos(r$exceso)
             else             en_pesos(input$presupuesto - r$total)))
      ))
    )})

# primer grafico que aparece
output$grafico_categorias <- renderPlot({
    req(datos_store()) 
    cat_df <- por_categoria(datos_store())
    ggplot(cat_df, aes(reorder(categoria, total), total, fill = categoria)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(porcentaje, "%")),hjust = -0.1,size  = 3.5) +
    coord_flip() +
    scale_y_continuous(labels = comma,expand = expansion(mult = c(0, 0.18))) +
    labs(title = "Gasto total por categoría", x = NULL, y = "Total ($)") +
    theme_minimal(base_size = 14)})

# Animación generada
output$animacion <- renderImage({
    req(datos_store())
    df    <- datos_store()
    meses <- unique(df$mes)
    if (length(meses) < 2) {
        tmp <- tempfile(fileext = ".png")
        png(tmp, width = 700, height = 180)
        plot.new()
        text(0.5, 0.5,"Se necesitan al menos 2 meses de datos para lograr crear la animación.",cex = 1.4, col = "gray40")
        dev.off()
        return(list(src = tmp, contentType = "image/png", width = "100%"))}

    anim_df <- df |>
      group_by(mes, categoria) |>
      summarise(total = sum(monto), .groups = "drop")

    todas_cats  <- unique(anim_df$categoria)
    todos_meses <- unique(anim_df$mes)
    grilla <- expand.grid(
    mes       = todos_meses,
    categoria = todas_cats,
    stringsAsFactors = FALSE)

    anim_df <- merge(grilla, anim_df,by = c("mes", "categoria"), all.x = TRUE)
    anim_df$total[is.na(anim_df$total)] <- 0

    p <- ggplot(anim_df,
                aes(reorder(categoria, total), total, fill = categoria)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(title = "Mes: {closest_state}", x = NULL, y = "Total gastado ($)") +
      theme_minimal(base_size = 14) +
      transition_states(mes, transition_length = 2, state_length = 1) +
      ease_aes("cubic-in-out")
      tmp <- tempfile(fileext = ".gif")
      animate(p, width = 800, height = 500, fps = 20,
            renderer = gifski_renderer(file = tmp))
      list(src = tmp, contentType = "image/gif", width = "100%")}, 
      deleteFile = TRUE)
}

#Inicio consola
shinyApp(ui, server)
