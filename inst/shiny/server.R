
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
mindate <- min(wd10[["time"]]$dt)
maxdate <- max(wd10[["time"]]$dt)
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
    } else return(NULL)
  })
  # Plot By Selector with none
  output$UIplotby3 <- renderUI({
      if(input$SELanalysis=="plots"){
       if(!is.null(input$SELplottype)){ # Esto no se muy bien porque pero tirar error sino.
        if(input$SELplottype=="histogram" | input$SELplottype=="rose"){
          radioButtons("SELplotby","By:",selected="none",
                       list("None" = "none", 
                            "Month" = "month", 
                            "Hour" = "hour"))
        } else return(NULL)
      } else return(NULL)
     } else return(NULL)
  })
  # Plot By Selector without none
  output$UIplotby2 <- renderUI({
    if(input$SELanalysis=="plots"){
      if(!is.null(input$SELplottype)){ # Esto no se muy bien porque pero tirar error sino.
        if(input$SELplottype=="profile"){
          radioButtons("SELplotby","By:",selected="hour",
                       list("Month" = "month", 
                            "Hour" = "hour"))
        } else return(NULL)
      } else return(NULL)
    } else return(NULL)
  })
  # Anemometer Checklist 
  output$UIane <- renderUI({
    anelist <- ane.names
    names(anelist) <- ane.names
    checkboxGroupInput("SELane", "Anemometers:",anelist,choices=ane.names )
  })  
  # sliderInput Histogram bins
  output$UIbin <- renderUI({
    if(!is.null(input$SELplottype)){ # Esto no se muy bien porque pero tirar error sino.
      if (input$SELplottype == 'histogram'){
        wellPanel(sliderInput("binwidth", 
                    "bin:", 
                    min = .1,
                    max = 2, 
                    step = .1,
                    value = 1))
      }
    }
  })
  
  output$UIdates <- renderUI({
    wellPanel(
      dateRangeInput("dates", label="Dates filter", start = mindate, end = maxdate, min = mindate,
                   max = maxdate, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                   language = "en", separator = " to ")
    )
  })
  
  
  #   UI Tabs
  output$UItabs <- renderUI ({
    if(input$SELanalysis=="plots"){
      if(!is.null(input$SELplottype)){ # Esto no se muy bien porque pero tirar error sino.
        tabs <- list(NULL)
        skip <-0
        if (input$SELplottype=="rose" | input$SELplottype=="profile" | input$SELplottype=="boxplot"){ 
          tabs[[1]] <- tabPanel("All", h1("www"), plotOutput("plotAll"))
          skip <- 1
        }
        for (i in 1:nane){
          tabs[[i+skip]] <- tabPanel(ane.names[i], 
                                     tabsetPanel(
                                       tabPanel("Plot",
                                                h1("www1"),
                                                plotOutput(paste("plot",ane.names[i],sep="")),
                                                h1("www3")),
                                       tabPanel("Data", 
                                                h1("www"),
                                                tableOutput(as.name(paste("table",ane.names[i],sep="")))) #, downloadButton('dldat', 'Download Sample'))
                                     ))
        }
        do.call(tabsetPanel, tabs)
      } else return(NULL)
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
          print(plotWD(data=datawd,var="speed", ane=i ,type=input$SELplottype, by=input$SELplotby,binwidth=input$binwidth))
        } else return(NULL)
      }
    })
  })}
  
  # Plot Generation All Ane
  output$plotAll <- renderPlot({
    if (input$SELplottype=="rose" | input$SELplottype=="profile"){
      print(plotWD(data=datawd,var="speed", ane=input$SELane ,type=input$SELplottype, by=input$SELplotby,binwidth=input$binwidth))
    } else return(NULL)
  })
  
  # Table Generation
  for (x in ane.names){local({
    i <- x
    output[[paste("table",i,sep="")]] <- renderTable({
      if (input$SELplottype=="histogram" | input$SELplottype=="rose" | input$SELplottype=="profile" | input$SELplottype=="boxplot"){
        if (i %in% input$SELane){
          as.data.frame(tableWD(data=datawd,var="speed", ane=i ,type=input$SELplottype, by=input$SELplotby,binwidth=input$binwidth)[[i]])
        }
      } else return(NULL)
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
    } else return(NULL)
  })
 
  # tableTurbulence
  output$tableTurbulence <- renderPrint({
#     summary(c(1:10))
    if(input$SELanalysis=="turbulence"){
      tableWD(data=datawd, type="turbulence")
    } else return(NULL)
  })
  
  # plotTurbulence
  output$plotTurbulence <- renderPlot({
    if (input$SELanalysis=="turbulence"){
      print(plotWD(data=datawd, type="turbulence"))
    } else return(NULL)
  })
  
  output$dlCurPlot <- downloadHandler(
    filename = function() { paste(input$SELanalysis,
                                  '-',
                                  input$SELplottype,'.pdf', sep='')},
    content = function(file){
      pdf(file = file, width=11, height=8.5)
      plotWD(data=datawd,var="speed", ane="Ane1" ,type=input$SELplottype, by=input$SELplotby)
      dev.off()
    }
  )
  
  output$dldat <- downloadHandler(
    filename = function() { paste(input$SELanalysis,
                                  '-',
                                  input$SELplottype,'.csv', sep='') },
    content = function(file) {
        write.csv(as.data.frame(tableWD(data=datawd,var="speed", ane="Ane1" ,type=input$SELplottype, by=input$SELplotby)[[1]]),file)
    }
  )
  
  
})

