
#' @title Complete numbers
#' 
#' @description
#' Function to Complete numbers
#' 
#' @param data a list of paramenters.
#' @param charnum number of digits.
#' @return parameter's value extracted.
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' @export
datawd<-wd10
shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    anelist<-c()
    if (input$Ane1==T) anelist<-c(anelist,"Ane1")
    if (input$Ane2==T) anelist<-c(anelist,"Ane2")
    tableWD(data=datawd,var="speed", ane=anelist, input$type, by=input$by)
  })
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
#     ane <- c()
#     if (input$Ane1==T) ane <- "Ane1"
#     if (input$Ane2==T) ane <- c(ane,"Ane2")
#     ane
#     paste('plotWD(data=data,var="speed", ane=',ane,',type=',input$type,', by=',input$by,')')
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
    result
  })
  output$captionP2 <- renderText({
    result <- NULL
    if (input$Ane2==T & input$Ane1==T  & input$type!="rose") result<-"Ane2"

    result
  })

  
  output$plot1 <- renderPlot({
    if (input$type=="histogram" | input$type=="fit"  | input$type=="turbulence") {
      if (input$Ane1==T) print(plotWD(data=datawd,var="speed", ane="Ane1" ,type=input$type, by=input$by))
      else if (input$Ane2==T) print(plotWD(data=datawd,var="speed", ane="Ane2" ,type=input$type, by=input$by))
    }
    if (input$type=="rose"){
      print(plotWD(data=datawd,var="speed", ane=c("Ane1","Ane2") ,type=input$type, by=input$by))
    }
  })
  
  output$plot2 <- renderPlot({
    if (input$Ane2==T & input$Ane1==T & input$type!="rose" & input$type!="turbulence") {
      if (input$Ane2==T) print(plotWD(data=datawd,var="speed", ane="Ane2" ,type=input$type, by=input$by))
    }
  })
  
})
