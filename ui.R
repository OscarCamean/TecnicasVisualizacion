library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(shiny)

ui <- dashboardPage(skin = 'purple',
              #### título ####  
                    dashboardHeader(title = "Diseña tu gráfico:",titleWidth = 250),
                    
                #### Margen ####
                  #### mpg ####
                    dashboardSidebar(
                      selectInput('data','Elige la base de datos:',
                                  choices = c("mpg" = 'mpg',
                                              "diamonds" = 'diamonds',
                                              "mtcars" = 'mtcars',
                                              "iris" = 'iris',
                                              "-" ),
                                  selected = 'iris'),
                      
                      conditionalPanel(
                        condition = "input.data == 'mpg'",
                        
                        selectInput("varxm", "Elige la variable X del dataset 'mpg'",
                                    choices = names(mpg),
                                    selected = 'cyl'),
                        
                        selectInput("varym", "Elige la variable Y del dataset 'mpg'",
                                    choices = names(mpg),
                                    selected = 'model'),
                        
                        selectInput("sizem", "Elige en base a qué variable quieres el tamaño:",
                                    choices = names(mpg),
                                    selected = 'model'),
                        
                        checkboxInput('colorm', 'Añadir color',
                                      value = F),
                        
                        conditionalPanel(
                          condition = "input.colorm == true",
                          selectInput("colormpg",
                                      "¿En base a qué variable prefieres el color?",
                                      choices = names(mpg))),
                        
                        checkboxInput('facetsmpg','¿Facets?',
                                      value = F),
                        
                        conditionalPanel(condition = "input.facetsmpg == true",
                                         selectInput('varmpg', 'Elige las filas:',
                                                     c(None = '.', names(mpg))),
                                         
                                         selectInput('colmpg', 'Elige las columnas:',
                                                     c(None = '.', names(mpg)))),
                        
                        checkboxInput('slidermpg','¿Animar?',
                                      value = F),
                        
                        conditionalPanel(
                          condition = "input.slidermpg == true",
                          sliderInput("animation1","Tamaño muestral:",
                                      min = 1,max = nrow(mpg),
                                      value = 1,step = 1,
                                      animate =
                                        animationOptions(interval = 160, loop = F))),
                        hr(),
                        actionButton("run1", "Visualizar"),
                        hr()),
                  #### diamonds #####
                      conditionalPanel(
                        condition = "input.data == 'diamonds'",
                        
                        selectInput(
                          "varxd",
                          "Elige la variable X del dataset 'diamonds'",
                          choices = names(diamonds),
                          selected = 'color'
                        ),
                        
                        selectInput(
                          "varyd",
                          "Elige la variable Y del dataset 'diamonds'",
                          choices = names(diamonds),
                          selected = 'cut'
                        ),
                        
                        selectInput("sized", "Elige en base a qué variable quieres el tamaño:",
                                    choices = names(diamonds)),
                        
                        checkboxInput('colordi', 'Añadir color',
                                      value = F),
                        
                        conditionalPanel(
                          condition = "input.colordi == true",
                          selectInput("colordiamonds",
                                      "¿En base a qué variable prefieres el color?",
                                      choices = names(diamonds))),
                        
                        checkboxInput('facetsdiamonds','¿Facets?',
                                      value = F),
                        
                        conditionalPanel(condition = "input.facetsdiamonds == true",
                                         selectInput('vardi', 'Elige las filas:',
                                                     c(None = '.', names(diamonds))),
                                         
                                         selectInput('coldi', 'Elige las columnas:',
                                                     c(None = '.', names(diamonds)))),
                        
                        checkboxInput('sliderdi','¿Animar?',
                                      value = F),
                        
                        conditionalPanel(
                          condition = "input.sliderdi == true",
                          
                          sliderInput("animation2", "Tamaño muestral:",
                                      min = 1, max = nrow(diamonds),
                                      value = 1, step = 1,
                                      animate =
                                        animationOptions(interval = 160, loop = F))),
                        
                        actionButton("run2", "Visualizar"),hr()),
                      
                  #### mtcars #####
                      conditionalPanel(
                        condition = "input.data == 'mtcars'",
                        
                        selectInput("varxmt",
                                    "Elige la variable X del dataset 'mtcars'",
                                    choices = names(mtcars)),
                        
                        selectInput("varymt",
                                    "Elige la variable Y del dataset 'mtcars'",
                                    choices = names(mtcars)),
                        
                        selectInput("sizemtc", "Elige en base a qué variable quieres el tamaño:",
                                    choices = names(mtcars)),
                        
                        checkboxInput('colormt', 'Añadirle color',
                                      value = F),
                        
                        conditionalPanel(
                          condition = "input.colormt == true",
                          selectInput("colormtcars",
                                      "¿En base a qué variable prefieres el color?",
                                      choices = names(mtcars))),
                        
                        checkboxInput('facetsmtc','¿Facets?',
                                      value = F),
                        
                        conditionalPanel(condition = "input.facetsmtc == true",
                                         selectInput('varmt', 'Elige las filas:',
                                                     c(None = '.', names(mtcars))),
                                         
                                         selectInput('colmt', 'Elige las columnas:',
                                                     c(None = '.', names(mtcars)))),
                        
                        checkboxInput('slidermtc','¿Animar?',
                                      value = F),
                        
                        conditionalPanel(
                          condition = "input.slidermtc == true",
                          
                          sliderInput("animation3", "Tamaño muestral:",
                                      min = 1, max = nrow(mtcars),
                                      value = 1, step = 1,
                                      animate =
                                        animationOptions(interval = 160, loop = F))),
                        
                        
                        actionButton("run3", "Visualizar"), hr()
                      ), 
                  #### Iris ####
                      conditionalPanel(condition = "input.data == 'iris'",
                                       selectInput("varxi","Elige la variable X del dataset 'iris'",
                                                   choices = names(iris),
                                                   selected = 'Sepal.Length'),
                                       
                                       selectInput("varyi","Elige la variable Y del dataset 'iris'",
                                                   choices = names(iris),
                                                   selected = 'Petal.Width'),
                                       
                                       selectInput("sizei", "Elige en base a qué variable quieres el tamaño:",
                                                   choices = names(iris)),
                                       
                                       checkboxInput('colorir', 'Añadir color',
                                                     value = F),
                                       
                                       conditionalPanel(
                                         condition = "input.colorir == true",
                                         selectInput("coloriris",
                                                     "¿En base a qué variable prefieres el color?",
                                                     choices = names(iris),
                                                     selected = 'Species')),
                                       
                                       checkboxInput('facetsiris','¿Facets?',
                                                     value = F),
                                       
                                       conditionalPanel(condition = "input.facetsiris == true",
                                                        selectInput('varir', 'Elige las filas:',
                                                                    c(None = '.', names(iris))),
                                                        
                                                        selectInput('colir', 'Elige las columnas:',
                                                                    c(None = '.', names(iris)))),
                                       
                                       checkboxInput('sliderir','¿Animar?',
                                                     value = F),
                                       
                                       conditionalPanel(
                                         condition = "input.sliderir == true",
                                         
                                         sliderInput("animation4", "Tamaño muestral:",
                                                     min = 1, max = nrow(iris),
                                                     value = 1, step = 1,
                                                     animate =
                                                       animationOptions(interval = 160, loop = F))),
                                       
                                       actionButton("run4", "Visualizar"),hr()
                                       
                      )),
                    
               ##### Main #####
                    dashboardBody(tags$head(tags$style(
                      HTML(
                        '.main-header .logo {
                        font-family: "Georgia", Times, "Times New Roman", serif;
                        font-weight: bold;
                        font-size: 24px;
                        }'))),

                      fluidPage(plotOutput("maingraph"), lty = 'solid'),
                      hr(),
                      downloadButton('guardar', 'Descargar código (ui)', class = 'butt1'),
                      tags$head(
                        tags$style(
                          ".butt1{background-color:green;} .butt1{color: white;} .butt1{font-family: Courier New}")),
                      
                      downloadButton('guardars', 'Descargar código (server)', class = 'butt1'),
                      tags$head(
                        tags$style(
                          ".butt1{background-color:green;} .butt1{color: white;} .butt1{font-family: Courier New}"))
                      )
#####                    
)

