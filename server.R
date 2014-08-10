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

# data(wtgData)
# wd10$dir$ang_16 <- (wd10$dir$sect_16-1)*22.5
# class(wd10)<-"windata"

# dataGUIwd<-wd10
# print(getwd())
# load(file=paste(path.package("WindResource"),"/shiny/dataGUIwd.Rd",sep=""))

data(wdOlavarria)
data(wdMtTom)
data(wd)
data(wd10)


shinyServer(function(input, output) {
  
  datasetInput2 <- reactive({
    switch(input$dataset,
           wdOlavarria=wdOlavarria,
           wdMtTom=wdMtTom,
           wd=wd,
           wd10=wd10)
  })
  mindate <- reactive({
    data <- datasetInput2()
    min(data[["time"]]$dt)
  })
  maxdate <- reactive({
    data <- datasetInput2()
    max(data[["time"]]$dt)    
  })
  year.list <- reactive({
    data <- datasetInput2()
    s <- list()
    for (i in unique(format(wd$time$dt,"%Y"))){
      s[[i]] <- i
    }
    s
  })
  month.list <- reactive({
    data <- datasetInput2()
    s <- list()
    for (i in unique(format(wd$time$dt,"%m"))){
      s[[i]] <- i
    }
    s
  })
  ane.names <- reactive({
    data <- datasetInput2()
    data[["ane"]]$ane.names
  })
  ane.names.char <- reactive({
    data <- datasetInput2()
    as.character(paste(data[["ane"]]$ane.names, sep=", "))
  })
  data.length <- reactive({
    data <- datasetInput2()
    length(data[["time"]][,1])
  })
  time.interval <- reactive({
    data <- datasetInput2()
    data[["interval.minutes"]]
  })
  ane.names <- reactive({
    data <- datasetInput2()
    data[["ane"]]$ane.names
  })
  dataset.name <- reactive({
    data <- datasetInput2()
    data[["name"]]
  })
  # Plot Type Selector
  output$UIplottype <- renderUI({
    if(input$SELanalysis=="plots"){
      selectInput("SELplottype","Plot type:",
                  list("Histogram" = "histogram", 
                       "Rose" = "rose",
                       "Profile" = "profile",
                       "BoxPlot" = "boxplot", 
                       "TimeSerie" = "ts",
                       "Calendar" = "calendar"))
    } else return(NULL)
  })
  
  # Plot WTG Selector
  output$UIwtg <- renderUI({
    if(input$SELanalysis=="pc"){
      selectInput("SELturbinetype","Turbine type:",
                  list("E33" = "E33", 
                       "E48" = "E48"))
    } else return(NULL)
  })
  
  # Plot Options Selector
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
        else if(input$SELplottype=="histogram"){
          wellPanel(h4("Plot Options",align = "center"),
                    div(class="row", 
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
        } 
        else if(input$SELplottype=="profile"){
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
        } 
        else if (input$SELplottype=="boxplot"){
          wellPanel(h4("Plot Options",align = "center"),
                    div(class="row",
                        div(class="span3", 
                            radioButtons("SELplotby","By:",
                                         list("Hour" = "hour", 
                                              "Month" = "month")))))
        } 
        else if (input$SELplottype=="ts"){
          listane <- ane.names()
          names(listane) <- ane.names()
          listane <- as.list(listane)
          wellPanel(h4("Plot Options",align = "center"),
                    selectInput("SELane","Anemometer:",
                                listane),
                    selectInput("SELyear","Year:",
                                year.list()),
                    selectInput("SELmonth","Month:",
                                month.list()))
        } 
        else if (input$SELplottype=="calendar"){
          listane <- ane.names()
          names(listane) <- ane.names()
          listane <- as.list(listane)
          wellPanel(h4("Plot Options",align = "center"),
                    selectInput("SELane","Anemometer:",
                                listane),
                    radioButtons("SELplotvar","Var:",
                                 list("Ave" = "ave", 
                                      "Min" = "min", 
                                      "Max" = "max")))
        }
        else return(NULL)
      } else return(NULL)
    }
  })
  
  # Dates Selector
  output$UIdates <- renderUI({
    if (input$SELanalysis!="info"){
          wellPanel(h4("Date Filter"), 
                    dateRangeInput(label="",
                                   inputId = "SELdate",
                                   start = mindate(),
                                   end = maxdate(), 
                                   min = mindate(),
                                   max = maxdate(), 
                                   format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                   language = "en"))
    }
})
  
  # UI Tabs
  output$UItabs <- renderUI ({
    data <- datasetInput2()
    if(input$SELanalysis=="info"){
      tabsetPanel(
        tabPanel("Dataset Information",
                 h2(paste("Name: ",dataset.name(),sep="")),
                 h4(paste("Number of records: ",data.length(),sep="")),
                 h4(paste("Anemometers: ", paste(ane.names.char(), collapse = ", "))),
                 h4(paste("Time interval: ",time.interval()," minutes",sep="")),
                 h4(paste("First time: ",mindate(),sep="")),
                 h4(paste("Last time: ",maxdate(),sep=""))
                 )
        )
    }
    else if(input$SELanalysis=="plots"){
      if(!is.null(input$SELplottype)){ # Esto no se muy bien porque pero tirar error sino.
        tabs <- list(NULL)
        skip <- 0
        if (input$SELplottype=="ts"){
            tabsetPanel(
              tabPanel("Plot", htmlOutput("plotTs")),
              tabPanel("Data", verbatimTextOutput(("dataTs"))
              )
            )  
          }
        else if (input$SELplottype=="calendar"){
          tabsetPanel(
            tabPanel("Plot", htmlOutput("plotCalendar")),
            tabPanel("Data", verbatimTextOutput(("dataCalendar"))
            )
          )  
        }
        else {
          if (input$SELplottype=="rose" | input$SELplottype=="profile"){ 
            tabs[[1]] <- tabPanel("All", h1("www"), plotOutput("plotAll"))
            skip <- 1
          }
          for (i in 1:length(ane.names())){
            tabs[[i+skip]] <- tabPanel(ane.names()[i], 
                                       tabsetPanel(
                                         tabPanel("Plot",
                                                  h1("www1"),
                                                  plotOutput(paste("plot",ane.names()[i],sep=""))),
                                         tabPanel("Data", 
                                                  h1("www"),
                                                  tableOutput(as.name(paste("table",ane.names()[i],sep="")))) 
                                       ))
          }
          do.call(tabsetPanel, tabs)
        }
        
      } else return(NULL)
    } 
    else if(input$SELanalysis=="fit"){
        tabs <- list(NULL)
        skip <- 0
          for (i in 1:length(ane.names())){
            tabs[[i+skip]] <- tabPanel(ane.names()[i], 
                                       tabsetPanel(
                                         tabPanel("Plot",
                                                  h1("www1"),
                                                  plotOutput(paste("plot",ane.names()[i],sep=""))),
                                         tabPanel("Data", 
                                                  h1("www"),
                                                  tableOutput(as.name(paste("table",ane.names()[i],sep="")))) 
                                       ))
          }
          do.call(tabsetPanel, tabs)
    }
    else if (input$SELanalysis=="pc"){
      tabsetPanel(
        tabPanel("Plot", plotOutput("plotTurbine")),
        tabPanel("Data", verbatimTextOutput(("tableTurbine"))
        )
      )            
    } 
    else if(input$SELanalysis=="turbulence"){
      tabsetPanel(
        tabPanel("Plot", plotOutput("plotTurbulence")),
        tabPanel("Data", verbatimTextOutput(("tableTurbulence"))
        )
      )
    } 
    else return(NULL)
  })

  output$downloadData <- downloadHandler(
    filename = function() { 'data.csv' },
    content = function(file) {
      data <- datasetInput2()
      write.csv(data, file)
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
  
  for (x in isolate(ane.names())){
    data <- isolate(datasetInput2())
    local({
      i <- x
      output[[paste("plot",i,sep="")]] <- renderPlot({
        if(input$SELanalysis=="plots"){
          if (input$SELplottype=="histogram" | input$SELplottype=="profile" | 
                input$SELplottype=="boxplot" | input$SELplottype=="rose"){
            if (input$SELplottype == 'rose' & input$SELplotby != 'none'){
              plot(plotWD(data=data,var="mean", ane=i ,type=input$SELplottype, by=input$SELplotby,binwidth=input$binwidth))
            } else {
              plotWD(data=data,var="mean", ane=i ,type=input$SELplottype, by=input$SELplotby,binwidth=input$binwidth)
            }
          }
          else return(NULL)
        } else {
          if (input$SELanalysis=="fit") {
          plotWD(data=data, ane=i, type=input$SELanalysis, binwidth=input$binwidth)
        }
        }
      })
    })}

  # Plot Generation All Ane
  output$plotAll <- renderPlot({
    data <- datasetInput2()
    if (input$SELplottype == 'rose' & input$SELplotby != 'none'){
      print(plotWD(data=data,var="mean", ane=ane.names() ,type=input$SELplottype, by=input$SELplotby,binwidth=input$binwidth))
    } else {
      plotWD(data=data,var="mean", ane=ane.names() ,type=input$SELplottype, by=input$SELplotby,binwidth=input$binwidth)
    }
  })
  
  # Plot Serie
  output$plotTs <- renderGvis({
      if(input$SELanalysis=="plots"){
        if (input$SELplottype=="ts"){
          data <- datasetInput2()
          print(as.numeric(input$SELyear))
          print(as.numeric(input$SELmonth))
          plotwindserie(data,
                        as.numeric(input$SELyear),
                        as.numeric(input$SELmonth),
                        vars=c("Ave"),
                        axis=c("Ave"),
                        shiny=T)
        } else return(NULL)
      }  else return(NULL)
    })
  
  # Plot Calendar
  output$plotCalendar <- renderGvis({
      if(input$SELanalysis=="plots"){
        if (input$SELplottype=="calendar"){
          data <- datasetInput2()
          plotcalendar (data, 
                        var="ave", 
                        ane="Ane1",
                        shiny=T)
        } else return(NULL)
      }  else return(NULL)
    })
  
  # Table Generation
  for (x in isolate(ane.names())){
    data <- isolate(datasetInput2())
    local({
      i <- x
      output[[paste("table",i,sep="")]] <- renderTable({
        if(input$SELanalysis=="plots"){
          if (input$SELplottype=="histogram" | input$SELplottype=="rose" | input$SELplottype=="profile" | input$SELplottype=="boxplot"){
            #         if (i %in% input$SELane){
            as.data.frame(tableWD(data=data,var="mean", ane=i ,type=input$SELplottype, by=input$SELplotby,binwidth=input$binwidth)[[i]])
            #         }
          } else return(NULL)
        } else if (input$SELanalysis=="fit"){
          #         if (i %in% input$SELane){
          as.data.frame(tableWD(data=data, var=input$SELplotvar, ane=i, type=input$SELanalysis)[[i]])
          #         }
        }  
      })
    })}

  
  # Plot Generation All Ane
  output$tableAll <- renderTable({
    data <- datasetInput2()
    if (input$SELplottype=="rose"){
      tableWD(data=data,
              var="mean", 
              ane=ane.names(),
              type=input$SELplottype, 
              by=input$SELplotby) 
    } 
    else if (input$SELplottype=="profile"){
      tableWD(data=data,
              var="mean", 
              ane=ane.names(),
              type=input$SELplottype, 
              by=input$SELplotby) 
    }
    else return(NULL)
  })
  
  # tableTurbulence
  output$tableTurbulence <- renderPrint({
    #     summary(c(1:10))
    data <- datasetInput2()
    if(input$SELanalysis=="turbulence"){
      tableWD(data=data, type="turbulence")
    } else return(NULL)
  })
  
  # plotTurbulence
  output$plotTurbulence <- renderPlot({
    if (input$SELanalysis=="turbulence"){
      data <- datasetInput2()
      plotWD(data=data, type="turbulence")
    } else return(NULL)
  })
  
  # Info
  output$info <- renderPrint({
    Panel(h4("Hola"),
    h3("yyyy"))
  })

  # tableTurbine
  output$tableTurbine <- renderPrint({
    if(input$SELanalysis=="pc"){
      data <- datasetInput2()
      tablewtg(data=wtgData, Model=input$SELturbinetype)
    } else return(NULL)
  })
  
  # plotTurbine
  output$plotTurbine <- renderPlot({
    if (input$SELanalysis=="pc"){
      data <- datasetInput2()
      plotwtg(data=wtgData, Model=input$SELturbinetype)
    } 
  })
  
  output$dlCurPlot <- downloadHandler(
    filename = function() { paste(input$SELanalysis,
                                  '-',
                                  input$SELplottype,'.pdf', sep='')},
    content = function(file){
      pdf(file = file, width=11, height=8.5)
      data <- datasetInput2()
      plotWD(data=data,var="mean", ane="Ane1" ,type=input$SELplottype, by=input$SELplotby)
      dev.off()
    }
  )
  
  output$dldat <- downloadHandler(
    filename = function() { paste(input$SELanalysis,
                                  input$SELplottype,'.csv', sep='') },
    content = function(file) {
      write.csv(as.data.frame(tableWD(data=data,var="mean", ane="Ane1" ,type=input$SELplottype, by=input$SELplotby)[[1]]),file)
    }
  )
  
  
})

