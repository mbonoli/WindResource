
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
data(wtgData)
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
  output$UIwtg <- renderUI({
    if(input$SELanalysis=="pc"){
      selectInput("SELturbinetype","Turbine type:",
                  list("E33" = "E33", 
                       "E48" = "E48"))
    } else return(NULL)
  })
  output$UIplotopt <- renderUI({
    if(input$SELanalysis=="plots"){
      if(!is.null(input$SELplottype)) {
        if(input$SELplottype=="rose"){
          wellPanel(div(class="row", 
                        div(class="span5 offset1", 
                            radioButtons("SELplotvar","Var:",
                                         list("Ave" = "ave", 
                                              "Min" = "min", 
                                              "Max" = "max"))),
                        div(class="span3", 
                            radioButtons("SELplotby","By:",
                                         list("None" = "none", 
                                              "Hour" = "hour", 
                                              "Month" = "month")))))
          
        }
        if(input$SELplottype=="histogram"){
          wellPanel(h4("Plot Options",align = "center"),
                    div(class="row", 
                        div(class="span5 offset1", 
                            radioButtons("SELplotvar","Var:",
                                         list("Ave" = "ave", 
                                              "Min" = "min", 
                                              "Max" = "max"))),
                        div(class="span3", 
                            radioButtons("SELplotby","By:",
                                         list("None" = "none", 
                                              "Hour" = "hour", 
                                              "Month" = "month")))),
                    sliderInput("binwidth", 
                                "bin:", 
                                min = .1,
                                max = 2, 
                                step = .1,
                                value = 1))
        } else if(input$SELplottype=="profile"){
          wellPanel(h4("Plot Options",align = "center"),
                    div(class="row", div(class="span5 offset1", 
                                         radioButtons("SELplotvar","Var:",
                                                      list("Ave" = "ave", 
                                                           "Min" = "min", 
                                                           "Max" = "max"))),
                        div(class="span3", 
                            radioButtons("SELplotby","By:",
                                         list("Hour" = "hour", 
                                              "Month" = "month")))))
        } else if (input$SELplottype=="boxplot"){
          wellPanel(h4("Plot Options",align = "center"),
                    div(class="row",
                        div(class="span3", 
                            radioButtons("SELplotby","By:",
                                         list("Hour" = "hour", 
                                              "Month" = "month")))))
        } else return(NULL)
      } else return(NULL)
    }
  })
  
  # Anemometer Checklist 
  #   output$UIane <- renderUI({
  #     anelist <- ane.names
  #     names(anelist) <- ane.names
  #     checkboxGroupInput("SELane", "Anemometers:",anelist,choices=ane.names )
  #   })  
  # sliderInput Histogram bins
  #   output$UIbin <- renderUI({
  #     if(!is.null(input$SELplottype)){ # Esto no se muy bien porque pero tirar error sino.
  #       if (input$SELplottype == 'histogram'){
  #         wellPanel(sliderInput("binwidth", 
  #                     "bin:", 
  #                     min = .1,
  #                     max = 2, 
  #                     step = .1,
  #                     value = 1))
  #       }
  #     }
  #   })
  
  output$UIdates <- renderUI({
    wellPanel(h4("Date Filter",align = "center"),
              dateRangeInput("dates", label="", start = mindate, end = maxdate, min = mindate,
                             max = maxdate, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                             language = "en", separator = " to ")
    )
  })
  
  #   UI Tabs
  output$UItabs <- renderUI ({
    if(input$SELanalysis=="plots" | input$SELanalysis=="fit"){
      if(!is.null(input$SELplottype)){ # Esto no se muy bien porque pero tirar error sino.
        tabs <- list(NULL)
        skip <-0
        if (input$SELplottype=="rose" | input$SELplottype=="profile"){ 
          tabs[[1]] <- tabPanel("All", h1("www"), plotOutput("plotAll"))
          skip <- 1
        }
        for (i in 1:nane){
          if(input$SELplottype=="ts"){
            tabs[[i+skip]] <- tabPanel(ane.names[i], 
                                       tabsetPanel(
                                         tabPanel("Plot",
                                                  h1("serie"),
                                                  div(htmlOutput("plotSerie"))),      
                                         tabPanel("Data", 
                                                  h1("www"),
                                                  verbatimTextOutput("tableTurbulence"))
                                       ))
          } else {
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
        }
        do.call(tabsetPanel, tabs)
      } else return(NULL)
    } else if (input$SELanalysis=="pc"){
      tabsetPanel(
        tabPanel("Plot", plotOutput("plotTurbine")),
        tabPanel("Data", verbatimTextOutput(("tableTurbine"))
        )
      )            
    } else if(input$SELanalysis=="turbulence"){
      tabsetPanel(
        tabPanel("Plot", plotOutput("plotTurbulence")),
        tabPanel("Data", verbatimTextOutput(("tableTurbulence"))
        )
      )
    } else return(NULL)
    
  })
  
  datasetInput <- reactive({
    tableWD(data=datawd,var="speed", ane=ane.names, input$SELplottype, by=input$SELplotby)
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
    if (input$Ane2==T & input$Ane1==T  & input$SELplottype!="rose") result<-"Ane2"
    result
  })
  
  # Plot Generation by Ane: histogram
  for (x in ane.names){local({
    i <- x
    output[[paste("plot",i,sep="")]] <- renderPlot({
      if(input$SELanalysis=="plots"){
        if (input$SELplottype=="histogram" | input$SELplottype=="rose" | input$SELplottype=="profile" | input$SELplottype=="boxplot"){
          print(plotWD(data=datawd,var="speed", ane=i ,type=input$SELplottype, by=input$SELplotby,binwidth=input$binwidth))
        }
      } else if (input$SELanalysis=="fit") {
        print(plotWD(data=datawd, var="speed", ane=i, type=input$SELanalysis, binwidth=input$binwidth))
      }
    })
  })}
  
  # Plot Generation All Ane
  output$plotAll <- renderPlot({
    #     if (input$SELplottype=="rose" | input$SELplottype=="profile"){
    print(plotWD(data=datawd,var="speed", ane=ane.names ,type=input$SELplottype, by=input$SELplotby,binwidth=input$binwidth))
    #     } else return(NULL)
  })
  
  # Plot Generation All Ane
  output$plotSerie <- renderGvis({
    if (input$SELplottype=="ts"){
      plot.wind.serie(wd10,2013,2,vars=c("Ave"),
                      axis=c("Ave","Min","Max","Dir"))
    } else return(NULL)
  })
  
  # Table Generation
  for (x in ane.names){local({
    i <- x
    output[[paste("table",i,sep="")]] <- renderTable({
      if(input$SELanalysis=="plots"){
        if (input$SELplottype=="histogram" | input$SELplottype=="rose" | input$SELplottype=="profile" | input$SELplottype=="boxplot"){
          #         if (i %in% input$SELane){
          as.data.frame(tableWD(data=datawd,var="speed", ane=i ,type=input$SELplottype, by=input$SELplotby,binwidth=input$binwidth)[[i]])
          #         }
        } else return(NULL)
      } else if (input$SELanalysis=="fit"){
        #         if (i %in% input$SELane){
        as.data.frame(tableWD(data=datawd, var=input$SELplotvar, ane=i, type=input$SELanalysis)[[i]])
        #         }
      }  
    })
  })}
  
  # Plot Generation All Ane
  output$tableAll <- renderTable({
    if (input$SELplottype=="rose"){
      tableWD(data=datawd,
              var="speed", 
              ane=ane.names,
              type=input$SELplottype, 
              by=input$SELplotby) # by=input$SELplotby3)
    } else if (input$SELplottype=="profile"){
      tableWD(data=datawd,
              var="speed", 
              ane=ane.names,
              type=input$SELplottype, 
              by=input$SELplotby) # by=input$SELplotby2)
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
  
  # tableTurbine
  output$tableTurbine <- renderPrint({
    if(input$SELanalysis=="pc"){
      tablewtg(data=wtgData, Model=input$SELturbinetype)
    } else return(NULL)
  })
  
  # plotTurbine
  output$plotTurbine <- renderPlot({
    if (input$SELanalysis=="pc"){
      print(plotwtg(data=wtgData, Model=input$SELturbinetype))
    } 
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

