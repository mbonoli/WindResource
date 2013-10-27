library(shiny)
library(datasets)
library(ggplot2)
setwd("C:\\congreso")
source('C:\\CONGRESO\\funcionesMB.R', encoding='UTF-8', echo=TRUE)
source('C:\\CONGRESO\\RUNshiny.R', encoding='UTF-8', echo=TRUE)
source('C:\\CONGRESO\\tableWD.R', encoding='UTF-8', echo=TRUE)
source('C:\\CONGRESO\\plotWD.R', encoding='UTF-8', echo=TRUE)
# setwd("~/Investigacion/Vientos/BD/INTI")
load(file="wd.Rdata")
load(file="wd10.Rdata")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    anelist<-c()
    if (input$Ane1==T) anelist<-c(anelist,"Ane1")
    if (input$Ane2==T) anelist<-c(anelist,"Ane2")
      tableWD(data=wd,var="speed", ane=anelist, input$type, by=input$by)
  })
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
#     ane <- c()
#     if (input$Ane1==T) ane <- "Ane1"
#     if (input$Ane2==T) ane <- c(ane,"Ane2")
#     ane
#     paste('plotWD(data=wd,var="speed", ane=',ane,',type=',input$type,', by=',input$by,')')
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  output$captionT1 <- renderText({
    names(datasetInput())[1]
  })
  output$captionT2 <- renderText({
    if (length(datasetInput())>=2) 
      names(datasetInput())[2]
    else NULL
  })
  output$captionT3 <- renderText({
    if (length(datasetInput())>=3) 
      names(datasetInput())[3]
    else NULL
  })
  output$table1 <- renderTable({
    datasetInput()[[1]]
    })
  output$table2 <- renderTable({
    if (length(datasetInput())>=2) 
      datasetInput()[[2]]
    else NULL
  })
  output$table3 <- renderTable({
    if (length(datasetInput())>=3) 
      datasetInput()[[3]]
    else NULL
  })
  
  output$selectUI <- renderUI({ 
    checkboxInput("Ane1", "Ane1", T)
    checkboxInput("Ane2", "Ane2", T)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 'data.csv' },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )

  output$captionP1 <- renderText({
    result <- NULL
    if (input$Ane1==T) result<-"Ane1"    
    else if (input$Ane2==T) result<-"Ane2"
    if (input$type=="fit") result<-"Fit"
    result
  })
  output$captionP2 <- renderText({
    result <- NULL
    if (input$Ane2==T & input$Ane1==T) result<-"Ane2"
    if (input$type=="fit") result<-NULL
    result
  })

  
  output$plot1 <- renderPlot({
    if (input$type=="histogram") {
      if (input$Ane1==T) print(plotWD(data=wd,var="speed", ane="Ane1" ,type=input$type, by=input$by))
      else if (input$Ane2==T) print(plotWD(data=wd,var="speed", ane="Ane2" ,type=input$type, by=input$by))
    }
    if (input$type=="rose"){
      print(plotWD(data=wd,var="speed", ane=c("Ane1","Ane2") ,type=input$type, by=input$by))
    }
  })
  
  output$plot2 <- renderPlot({
    if (input$Ane2==T & input$Ane1==T & input$type!="rose") {
      if (input$Ane2==T) print(plotWD(data=wd,var="speed", ane="Ane2" ,type=input$type, by=input$by))
    }
  })
  
  output$pp <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- "c:\\congreso\\plot_zoom_png.jpg"
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
})
