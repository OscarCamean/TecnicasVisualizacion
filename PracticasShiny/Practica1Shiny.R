library(shiny)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(scatterplot3d)

#1. Genere un diagrama de dispersión de mtcars
#2. Tenga un selector para elegir el eje de las X para el dataset mtcars
#3. Tenga otro selector para elegir el eje de las Y para el mismo daaset.
#4. Tenga un checkbox que permita dividir la gráfica por facets por cualqueir variable que el alumno desee.
#5. Haz que los dos selectores del apartado 2 y 3 tomen los nombres de todas las columnas del dataset sin necesidad de que tu los escribas en el programa de R



ui <- fluidPage(titlePanel(
  h1('Práctica número 1 en Shiny')),
                sidebarLayout(
                  sidebarPanel(
                  
                  checkboxInput('facets', h4('¿Facets?'),
                               value = F),
                  
                  selectInput('var', 'Fila:',
                              c(None = '.', names(mpg))), 
                  
                  selectInput('col', 'Columna:',
                              c(None = '.', names(mpg))),
                  hr(),
                  
                  hr(),

                  selectInput('ejex','Elige el eje x:',
                              choices = names(mpg)),
                  
                  selectInput('ejey','Elige el eje y:',
                              choices = names(mpg)),
                  
                  actionButton('grafico','Gráfico')
                ),
                
                mainPanel(
                  fluidRow(
                  box(
                  plotOutput('plot1',width = 600,height = 650)
                )))))

server <- function(input,output){
  
  output$plot1 <- renderPlot({
    
    input$grafico
    
    ejeX <- isolate(input$ejex)
    
    ejeY <- isolate(input$ejey)
   
     vari <- isolate(input$var)
    
     coli <- isolate(input$col)
    
    d <- ggplot(mpg, aes_string(x = ejeX, y = ejeY)) + geom_point(aes(color=mpg$class))
    
    facets <- paste(isolate(input$var) , '~', isolate(input$col))
    
    if (input$facets == T)
      
      d <- d + facet_grid(facets)
    
    d
    
  })
}
  
shinyApp(ui,server)

