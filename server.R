library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(shiny)

server <- function(input, output) {
#### gráficos ####
  output$maingraph <- renderPlot({

    if (input$data != '-') {
      if (input$data == 'diamonds') grd()
      
      else 
        if (input$data == 'mpg') grc()
      
      else 
        if (input$data == 'mtcars') grmt()
      
      else 
        if (input$data == 'iris') gri()
      }
  })
  
  mi.modelo.mp <- reactive({
    input$run1
    
    isxm <- isolate(input$varxm)
    isym <- isolate(input$varym)
    iscolm <- isolate(input$colormpg)
    issim <- isolate(input$sizem)
    issim
 
      if (input$slidermpg == T)
        data <- head(mpg,input$animation1)
      else 
        data <- mpg
    
    ggplot(data, aes_string(x = isxm , y = isym)) + geom_point(aes_string(color = iscolm,size = issim)) + theme(legend.position = "none")
  }) 
  grc <- reactive({
    input$run1
    
    isxm <- isolate(input$varxm)
    isym <- isolate(input$varym)
    isvarmpg <- isolate(input$varmpg)
    iscolmpg <- isolate(input$colmpg)
    facets <- paste(isvarmpg , '~', iscolmpg)
    issim <- isolate(input$sizem)
    data <- head(mpg,input$animation1)
    issim
    
    if (input$slidermpg == T)
      data <- head(mpg,input$animation1)
    else 
      data <- mpg
    
    d <- ggplot(data, aes_string(x = isxm, y = isym)) + geom_point(aes_string(size = issim)) + theme(legend.position = "none")
    
    if (input$colorm == F) {
      if (input$facetsmpg == F) d
      else d + facet_grid(facets)
    }
    else 
      if (input$facetsmpg == F) 
        mi.modelo.mp() 
    else 
      if (input$facetsmpg == T) 
        mi.modelo.mp() + facet_grid(facets)
  })
  
  mi.modelo.di <- reactive({
    
    input$run2
    dat <- isolate(input$data)
    isxd <- isolate(input$varxd)
    isyd <- isolate(input$varyd)
    iscold <- isolate(input$colordiamonds)
    issidi <- isolate(input$sized)
    data <- head(diamonds,input$animation2)
    issidi
    
    if (input$sliderdi == T)
      data <- head(diamonds,input$animation2)
    else 
      data <- diamonds
    

    ggplot(data, aes_string(x = isxd, y = isyd)) + geom_point(aes_string(color = iscold,size = issidi)) + theme(legend.position = "none")
  }) 
  grd <- reactive({
    input$run2
    
    dat <- isolate(input$data)
    isxd <- isolate(input$varxd)
    isyd <- isolate(input$varyd)
    isvardi <- isolate(input$vardi)
    iscoldi <- isolate(input$coldi)
    facets <- paste(isvardi , '~', iscoldi)
    issidi <- isolate(input$sized)
    data <- head(diamonds,input$animation2)
    issidi
    
    if (input$sliderdi == T)
      data <- head(diamonds,input$animation2)
    else 
      data <- diamonds
    
    d <- ggplot(data, aes_string(x = isxd, y = isyd)) + geom_point(aes_string(size = issidi)) + theme(legend.position = "none")
  
    if (input$colordi == F) {
      if (input$facetsdiamonds == F) d
      else d + facet_grid(facets)
    }
    else 
      if (input$facetsdiamonds == F) 
        mi.modelo.di() 
    else 
      if (input$facetsdiamonds == T) 
        mi.modelo.di() + facet_grid(facets)
  })
  
  mi.modelo.mt <- reactive({
    
    input$run3
    dat <- isolate(input$data)
    isxmt <- isolate(input$varxmt)
    isymt <- isolate(input$varymt)
    iscolmt <- isolate(input$colormtcars)
    issimtc <- isolate(input$sizemtc)
    data <- head(mtcars,input$animation3)
    issimtc
    
    if (input$slidermtc == T)
      data <- head(mtcars,input$animation3)
    else 
      data <- mtcars

    ggplot(data, aes_string(x = isxmt, y = isymt)) + geom_point(aes_string(color = iscolmt,size = issimtc)) + theme(legend.position = "none")
    
  }) 
  grmt <- reactive({
    input$run3
    
    dat <- isolate(input$data)
    isxmt <- isolate(input$varxmt)
    isymt <- isolate(input$varymt)
    iscolmt <- isolate(input$colormtcars)
    isvarmt <- isolate(input$varmt)
    iscolmt <- isolate(input$colmt)
    facets <- paste(isvarmt , '~', iscolmt)
    issimtc <- isolate(input$sizemtc)
    data <- head(mtcars,input$animation3)
    issimtc

    if (input$slidermtc == T)
      data <- head(mtcars,input$animation3)
    else 
      data <- mtcars
    
    d <- ggplot(data, aes_string(x = isxmt, y = isymt)) + geom_point(aes_string(size = issimtc)) + theme(legend.position = "none")
    
    if (input$colormt == F) {
      if (input$facetsmtc == F) d
      else d + facet_grid(facets)
    }
    else 
      if (input$facetsmtc == F) 
        mi.modelo.mt() 
    else 
      if (input$facetsmtc == T) 
        mi.modelo.mt() + facet_grid(facets)
    
  })
  
  mi.modelo.ir <- reactive({
    
    input$run4
    dat <- isolate(input$data)
    isxi <- isolate(input$varxi)
    isyi <- isolate(input$varyi)
    iscoli <- isolate(input$coloriris)
    issii <- isolate(input$sizei)
    data <- head(iris,input$animation4)
    issii
  
    if (input$sliderir == T)
      data <- head(iris,input$animation4)
    else 
      data <- iris
    
    ggplot(data, aes_string(x = isxi , y = isyi)) + geom_point(aes_string(color = iscoli, size = issii)) + theme(legend.position = "none")
    
  }) 
  gri <- reactive({
    input$run4
    
    dat <- isolate(input$data)
    isxi <- isolate(input$varxi)
    isyi <- isolate(input$varyi)
    isvarir <- isolate(input$varir)
    iscolir <- isolate(input$colir)
    facets <- paste(isvarir , '~', iscolir)
    issii <- isolate(input$sizei)
    data <- head(iris,input$animation4)
    issii
    
    if (input$sliderir == T)
      data <- head(iris,input$animation4)
    else 
      data <- iris
    
    d <- ggplot(data, aes_string(x = isxi, y = isyi)) + geom_point(aes_string(size = issii)) + theme(legend.position = "none")
    
    if (input$colorir == F) {
      if (input$facetsiris == F) d
      else d + facet_grid(facets)
    }
    else 
      if (input$facetsiris == F) 
        mi.modelo.ir() 
    else 
      if (input$facetsiris == T) 
        mi.modelo.ir() + facet_grid(facets)
  })
#### descarga ####  
    uiparadescargar <- read_lines('ui.R')
    serverparadescar <- read_lines('server.R')
  
  output$guardar <- downloadHandler(
      filename = function(){
        paste("ui",Sys.time(),".txt")},
      content = function(con){writeLines(uiparadescargar,con)}
      )
  
  output$guardars <- downloadHandler(
    filename = function(){
      paste("server",Sys.time(),".txt")},
    content = function(con){writeLines(serverparadescar,con)}
  )
    
}




