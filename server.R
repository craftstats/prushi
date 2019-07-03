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
           textInput("usuario", "Usuario", "naderwizani@gmail.com"),
           textInput("passw", "Contraseña", "21759759"),
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
   auxi <- StMoMoData(bases[[pais()]])
   auxi
   
 })

}