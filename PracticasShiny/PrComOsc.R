library(shinydashboard)
library(shiny)
library(tidyverse)
library(MASS)


##### UI #####
Ui <- dashboardPage(
  
  dashboardHeader(title = 'Práctica combinada: Modelos polinomiales - K-means'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('GIF',tabName = 't1',icon = icon('table')),
      menuItem('modelo',tabName = 't2',icon = icon('dashboard')))),
  
  #### cuerpo ####    
  dashboardBody(
    tabItems(
      tabItem(tabName = 't1',
              h1('Esto es una foto para animarte!',
                 tags$img(src = 'http://as01.epimg.net/epik/imagenes/2018/04/28/portada/1524913221_572475_1524913364_noticia_normal.jpg',
                          width = '500px', height = '270px'))), 
      
      ##### fin ventana 1 ##### 
      tabItem(
        tabName = 't2',
        h2('Elige que tipo de gráfica quieres:'),
        box(plotOutput('hey')),
        
        box(checkboxInput('select', 'Quiero K-means:',
                          value = F), 
            ##### condicional 1 ####            
            conditionalPanel(
              condition = "input.select == false",
              
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
              
              hr(), hr(),
              
              numericInput(
                'pol',
                'Orden polinómico:',
                min = 0,
                step = 1,
                value = 1
              ),
              
              actionButton('modelo', 'Añadir modelo')),
            #### condicional 2 ####          
            
            conditionalPanel(condition = "input.select == true",
                             
                             selectInput("varx","Variable X", 
                                         choices = c("Sepal.Length",
                                                     "Sepal.Width",
                                                     "Petal.Length",
                                                     "Petal.Width")),
                             
                             selectInput("vary","Variable Y", 
                                         choices = c("Petal.Length",
                                                     "Sepal.Length",
                                                     "Sepal.Width",
                                                     "Petal.Width")),
                             
                             numericInput("nclusters",
                                          label = "Clusters", 
                                          min = 1,
                                          step = 1, 
                                          value = 3),
                             
                             actionButton("run","Run"))
        ))
    )
  )
)
###### UI #####
Server <- function(input,output){
  
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
 
  mi.pred <- reactive({
    input$modelo
    predict(lm(mi.dis()~ poly(mi.dis(),degree = input$pol)))
  }) 
  
  mi.grafico2 <- reactive({
    
    input$modelo
    
    mi.grafico() + lines(lowess(mi.pred())) +
      lines(mi.modelo()$fitted.values,type = 'l',col = 'red')
  })
  
  miModelo <- reactive({
    
    input$run
    
    isx <- isolate(input$varx)
    isy <- isolate(input$vary)
    clus <- isolate(input$nclusters)
    

      iris %>%
        dplyr::select(isx,isy) %>% 
      kmeans(clus)
  })
  
  output$hey <- renderPlot({
    
    if (input$select == F) {
      mi.grafico()
      tt <- isolate(input$pol)
      if (tt != 1)
        mi.grafico2()
    } else{
      input$run
      
      isx <- isolate(input$varx)
      isy <- isolate(input$vary)
      
      ggplot(iris, aes_string(
        x = isx,
        y = isy,
        color = as.factor(miModelo()$cluster)
      )) +
        geom_point(
        )
    }
    
  })
  
}

shinyApp(Ui,Server)

