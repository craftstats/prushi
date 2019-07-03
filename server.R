#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source(file = "utils.R")
library(shiny)
library(demography)
library(StMoMo)

# Define server logic required to draw a histogram

paises <-c("AUS","ESP","ITA")
names(paises) <- c("Australia", "España", "Italia")


function(input, output, session) {
 
  bases<-list()
  output$ui <- renderUI({
    if (is.null(input$input_type))
    return()
  
  switch(input$input_type,
         "hmd" = sidebarPanel(
           textInput("usuario", "Usuario", "rebeldatalab@gmail.com"),
           textInput("passw", "Contraseña", "1562189576"),
           selectInput("pais", "Pais", choices = paises, selected = NULL, selectize = TRUE),
           actionButton("carga","Cargar")
         ),
         "archivo" =  fileInput("file1", "Choose CSV File",
                                multiple = FALSE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
         ))
       
  })
  
  output$ui2 <- renderUI({
      if (is.null(pais()))
        return()
    
    
      selectInput("pais2", "Pais", choices = c("",names(bases)), selected="", selectize = FALSE)
  })
  
  output$uislider1 <- renderUI({
    req(input$pais2)
    mi <- min(bases[[input$pais2]]$year)
    mx <- max(bases[[input$pais2]]$year)
    
     sliderInput("anos", "Años", min = mi, max = mx , value = c(mi,mx), step = 1)
  })
  
  output$uislider2 <- renderUI({
    req(input$pais2)
    mi <- min(bases[[input$pais2]]$age)
    mx <- max(bases[[input$pais2]]$age)
    
    sliderInput("edad", "Edad", min = mi, max = mx , value = c(mi,mx), step = 1)
  })
  
  output$plot1 <- renderPlot({
    req(input$pais2)
    auxi<- bases[[input$pais2]]
    plot(auxi, transform = input$transf, 
         years = seq(input$anos[[1]], input$anos[[2]], 1),
         ages = seq(input$edad[[1]], input$edad[[2]], 1))
  })
  
  pais <- eventReactive(input$carga, {
    key_pais(paises, input$pais)
  })
  
  bas <- eventReactive(input$carga, {
     hmd.mx2(country=input$pais, username=input$usuario, password=input$passw, label= pais())
  })
  
  observe({
    
    if (!(pais() %in% names(bases))) {
      bases[[pais()]] <<-  hmd.mx2(country=input$pais, username=input$usuario, password=input$passw, label= pais())
    }
  })  
  
 
 output$cargadas <- renderText({
    paste("la longitud de ", pais(), names(bases))
  })
 
 
 output$summary  <- renderPrint({
   req(input$pais)
   cat(names(bases))
   auxi <- StMoMoData(bases[[pais()]])
   auxi
   
 })

}