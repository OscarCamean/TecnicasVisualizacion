library(shiny)
library(ggplot2)
library(MASS)

ui <- fluidPage(
  titlePanel('Ejercicio modelos polinomiales'),
  sidebarLayout(
    sidebarPanel(
      
      numericInput('tamanio',
                   'Tamaño muestral:',
                   min = 1,
                   step = 5,
                   value = 10),
      
      numericInput('cor',
                   'Correlación:',
                   min = 0,
                   max = 1,
                   step = .1,
                   value = .5),
      
      actionButton('muestra','Generar muestra'),
      
      hr(),hr(),
      
      numericInput('pol',
                   'Orden polinómico:',
                   min = 0,
                   step = 1,
                   value = 1),
      
      actionButton('modelo','Añadir modelo')
    ),
    
    mainPanel(fluidRow(plotOutput('plot1')))
  )
)


server <- function(input,output){
  
  mi.dis <- reactive({
    
    input$muestra
    
    t <- isolate(input$tamanio)
    c <- isolate(input$cor)
    
    set.seed(Sys.time())
    
    mvrnorm(n = t,mu = 0,Sigma = c)
  })
  
  mi.grafico <- reactive({
    
    input$muestra
    input$modelo
    plot(mi.dis(), ylab = 'Correlación',
         xlab = 'Nº obs.') 
  })
  
  mi.modelo <- reactive({
    
    input$modelo
    
    lm(mi.dis()~ poly(mi.dis(),degree = input$pol))
    
  }) 
  
  output$plot1 <- renderPlot({
    
    mi.grafico()
    tt <- isolate(input$pol)
    if (tt != 1)
      
      mi.grafico2()
    
  })
 
  mi.pred <- reactive({
    input$modelo
    predict(lm(mi.dis()~ poly(mi.dis(), degree = input$pol)))
  }) 
  
  mi.grafico2 <- reactive({
    
    input$modelo
    
    mi.grafico() + lines(lowess(mi.pred())) +
      lines(mi.modelo()$fitted.values,type = 'l',col = 'red')  
    })
}

shinyApp(ui,server)  




