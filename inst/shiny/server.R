
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

library("WindResource")
data(wd10)
wd10$dir$ang_16 <- (wd10$dir$sect_16-1)*22.5
class(wd10)<-"windata"
datawd<-wd10
nane <- 2

shinyServer(function(input, output) {
  
  ane.names <- wd10$info$ane$ane.names
  # Plot Type Selector
  output$UIplottype <- renderUI({
    if(input$SELanalysis=="plots"){
      selectInput("SELplottype","Plot type:",
                  list("Histogram" = "histogram", 
                       "Rose" = "rose",
                       "Profile" = "profile",
                       "BoxPlot" = "boxplot", 
                       "TimeSerie" = "ts"))
    } else NULL
  })
  # Plot By Selector with none
  output$UIplotby3 <- renderUI({
    if(input$SELanalysis=="plots"){
      if(input$SELplottype=="histogram" | input$SELplottype=="rose"){
        radioButtons("SELplotby","By:",selected="none",
                       list("None" = "none", 
                            "Month" = "month", 
                            "Hour" = "hour"))
      }
    } else NULL
  })
  # Plot By Selector without none
  output$UIplotby2 <- renderUI({
    if(input$SELanalysis=="plots"){
      if(input$SELplottype=="profile"){
        radioButtons("SELplotby","By:",selected="hour",
                     list("Month" = "month", 
                          "Hour" = "hour"))
      }
    } else NULL
  })
  # Anemometer Checklist 
  output$UIane <- renderUI({
    anelist <- ane.names
    names(anelist) <- ane.names
    checkboxGroupInput("SELane", "Anemometers:",anelist,choices=ane.names )
  })  
  
  #   UI Tabs
  output$UItabs <- renderUI ({
    if(input$SELanalysis=="plots"){
      tabs <- list(NULL)
      skip <-0
      if (input$SELplottype=="rose" | input$SELplottype=="profile" | input$SELplottype=="boxplot"){ 
        tabs[[1]] <- tabPanel("All", h1("www"), plotOutput("plotAll"))
        skip <- 1
      }
      for (i in 1:nane){
        tabs[[i+skip]] <- tabPanel(ane.names[i], 
                                   tabsetPanel(
                                     tabPanel("Plot", h1("www") ,plotOutput(paste("plot",ane.names[i],sep=""))),
                                     tabPanel("Data", h1("www"),tableOutput(as.name(paste("table",ane.names[i],sep=""))))
                                   ))
      }
      do.call(tabsetPanel, tabs)
    } else if(input$SELanalysis=="fit"){
      tabsetPanel(
        tabPanel("Summary",
                 h4("Aca va el resumen y comparacion") #,
                 #                plotOutput("plot1"),
                 #                h4(textOutput("captionP2")),
                 #                plotOutput("plot2")
        ),
        tabPanel("Weibull",
                 h4("Aca va todo lo de Weibull") #,
                 #                  plotOutput("plot1"),
                 #                
                 # h4(textOutput("captionP2")),
                 #                  plotOutput("plot2")
        ),
        tabPanel("Gamma",
                 h4("Aca va todo lo de Gamma") #,
                 #                  plotOutput("plot1"),
                 #                
                 # h4(textOutput("captionP2")),
                 #                  plotOutput("plot2")
        ),
        tabPanel("LogNormal",
                 h4("Aca va todo lo de LogNormal") #,
                 #                  plotOutput("plot1"),
                 #                
                 # h4(textOutput("captionP2")),
                 #                  plotOutput("plot2")
        )
      )
    } else if(input$SELanalysis=="turbulence"){
      tabsetPanel(
          tabPanel("Plot", plotOutput("plotTurbulence")),
          tabPanel("Data", verbatimTextOutput(("tableTurbulence"))
        )
      )
    }
    
  })
  
  datasetInput <- reactive({
    tableWD(data=datawd,var="speed", ane=input$SELane, input$SELplottype, by=input$SELplotby)
  })
  #   
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
#   formulaText <- reactive({
#     ane <- c()
#     if (input$Ane1==T) ane <- "Ane1"
#     if (input$Ane2==T) ane <- c(ane,"Ane2")
#     ane
#     paste('plotWD(data=data,var="speed", ane=',ane,',type=',input$SELplottype,', by=',input$SELplotby,')')
#   })
  
  output$downloadData <- downloadHandler(
    filename = function() { 'data.csv' },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  # 
  output$captionP1 <- renderText({
    result <- NULL
    if (input$Ane1==T) result<-"Ane1"    
    else if (input$Ane2==T) result<-"Ane2"
    result
  })
  output$captionP2 <- renderText({
    result <- NULL
    if (input$Ane2==T & input$Ane1==T  & input$SELplottype!="rose") result<-"Ane2"
    result
  })
  
  # Plot Generation by Ane: histogram
  for (x in ane.names){local({
    i <- x
    output[[paste("plot",i,sep="")]] <- renderPlot({
      if (input$SELplottype=="histogram" | input$SELplottype=="rose" | input$SELplottype=="profile" | input$SELplottype=="boxplot"){
        if (i %in% input$SELane){
          print(plotWD(data=datawd,var="speed", ane=i ,type=input$SELplottype, by=input$SELplotby))
        } else NULL
      }
    })
  })}
  
  # Plot Generation All Ane
  output$plotAll <- renderPlot({
    if (input$SELplottype=="rose" | input$SELplottype=="profile"){
      print(plotWD(data=datawd,var="speed", ane=input$SELane ,type=input$SELplottype, by=input$SELplotby))
    } else NULL
  })
  
  # Table Generation
  for (x in ane.names){local({
    i <- x
    output[[paste("table",i,sep="")]] <- renderTable({
      if (input$SELplottype=="histogram" | input$SELplottype=="rose" | input$SELplottype=="profile" | input$SELplottype=="boxplot"){
        if (i %in% input$SELane){
          as.data.frame(tableWD(data=datawd,var="speed", ane=i ,type=input$SELplottype, by=input$SELplotby)[[i]])
        }
      } else NULL
    })
  })}
  
  # Plot Generation All Ane
  output$tableAll <- renderTable({
    if (input$SELplottype=="rose"){
      tableWD(data=datawd,
              var="speed", 
              ane=input$SELane ,
              type=input$SELplottype, 
              by=input$SELplotby3)
    } else if (input$SELplottype=="profile"){
      tableWD(data=datawd,
              var="speed", 
              ane=input$SELane ,
              type=input$SELplottype, 
              by=input$SELplotby2)
    } else NULL
  })
 
  # tableTurbulence
  output$tableTurbulence <- renderPrint({
#     summary(c(1:10))
    if(input$SELanalysis=="turbulence"){
      tableWD(data=datawd, type="turbulence")
    } else NULL
  })
  
  # plotTurbulence
  output$plotTurbulence <- renderPlot({
    if (input$SELanalysis=="turbulence"){
      print(plotWD(data=datawd, type="turbulence"))
    } else NULL
  })
  
  output$dlCurPlot <- downloadHandler(
    filename = 'curPlot.pdf',
    content = function(file){
      pdf(file = file, width=11, height=8.5)
#       plotWD(data=datawd,var="speed", ane=i ,type=input$SELplottype, by=input$SELplotby)
      #doPlot(margins=c(6,6,10,2))
      dev.off()
    }
  )
  
  output$dldat <- downloadHandler(
    filename = function() { paste("name", '.csv', sep='') },
    content = function(file) {
#       write.csv(as.data.frame(tableWD(data=datawd,var="speed", ane=i ,type=input$SELplottype, by=input$SELplotby)[[1]]),file)
      #           as.data.frame(as.matrix(1:10,nrows=2))), file)
    }
  )
  
  
})

