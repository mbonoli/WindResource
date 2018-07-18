#' @title Complete numbers
#' 
#' @description
#' Function to Complete numbers
#' 
#' @param data a list of paramenters.
#' @param charnum number of digits.
#' @return parameter's value extracted.
#' 
#' @importFrom googleVis renderGvis
#' 
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' @export

# devtools::install_github("mbonoli/WindResource")

require(WindResource)
require(googleVis)

data(wdOlavarria)
data(wdMtTom)
data(wd)
data(wd10)
data(wtgData)


shinyServer(function(input, output) {
  
  datasetInput2 <- reactive({
    inFile <- input$file1
    if (!is.null(inFile)){
      load(input$file1$datapath)
    }
    if (is.null(input$dataset)) return(NULL)
    if (input$dataset=="wdMtTom") return(wdMtTom)
    if (input$dataset=="wdOlavarria") return(wdOlavarria)
    if (input$dataset=="wd") return(wd)
    if (input$dataset=="wd10") return(wd10)
    if (input$dataset=="filedata") 
      return(get(strsplit(input$file1$name,"[.]")[[1]][1]))
  })

  mindate <- reactive({
    data <- datasetInput2()
    if (is.null(data)) return()
    min(data[["time"]]$dt)
  })
  maxdate <- reactive({
    data <- datasetInput2()
    if (is.null(data)) return()
    max(data[["time"]]$dt)    
  })
  year.list <- reactive({
    data <- datasetInput2()
    s <- list()
    for (i in unique(format(data$time$dt,"%Y"))){
      s[[i]] <- i
    }
    s
  })
  month.list <- reactive({
    data <- datasetInput2()
    s <- list()
    for (i in unique(format(data$time$dt,"%m"))){
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
  var.options <- reactive({
    data <- datasetInput2()
    result <- list("Ave" = "mean")
    if (!is.null(wd$ane[[ane.names()[1]]]["min"][1])) result <- c(result, list("Min" = "min"))
    if (!is.null(wd$ane[[ane.names()[1]]]["max"][1])) result <- c(result, list("Max" = "max"))
    result <- c(result, list("Freq" = "freq"))
    result
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
  
  # Dataset Selector
  output$UIdataset <- renderUI({
    inFile <- input$file1
    if (is.null(inFile)) {
      selectInput("dataset", "Choose a dataset:", 
                  list("wdMtTom (demo)" = "wdMtTom", 
                       "wdOlavarria (demo)" = "wdOlavarria",
                       "wd (demo)" = "wd",
                       "wd10 (demo)" = "wd10"))
    } else {
      filelist <- list("wdMtTom (demo)" = "wdMtTom", 
                       "wdOlavarria (demo)" = "wdOlavarria",
                       "wd (demo)" = "wd",
                       "wd10 (demo)" = "wd10",
                       "filedata" ="filedata")
      names(filelist)[5] <- strsplit(input$file1$name,"[.]")[[1]][1]
      selectInput("dataset", "Choose a dataset:", filelist)
    }
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
  observe({
    output$UIplotopt <- renderUI({
      if(input$SELanalysis=="plots"){
        if(!is.null(input$SELplottype)){
          if(input$SELplottype=="rose"){
            wellPanel(div(class="row", 
                          div(class="span5 offset1", 
                              radioButtons("SELplotvarRose","Var:",
                                           var.options())
                          ),
                          div(class="span3", 
                              radioButtons("SELplotbyRose",label="By:",selected="none",
                                           choices=list("None" = "none", 
                                                        "Hour" = "hour", 
                                                        "Month" = "month")))))
          }
          else if(input$SELplottype=="histogram"){
            wellPanel(h4("Plot Options",align = "center"),
                      div(class="row", 
                          div(class="span3", 
                              radioButtons("SELplotbyHist","By:",
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
                                           radioButtons("SELplotvarProf","Var:",
                                                        list("Ave" = "ave", 
                                                             "Min" = "min", 
                                                             "Max" = "max"))),
                          div(class="span3", 
                              radioButtons("SELplotbyProf","By:",
                                           list("Hour" = "hour", 
                                                "Month" = "month")))))
          } 
          else if (input$SELplottype=="boxplot"){
            wellPanel(h4("Plot Options",align = "center"),
                      div(class="row",
                          div(class="span3", 
                              radioButtons("SELplotbyBox","By:",
                                           list("Hour" = "hour", 
                                                "Month" = "month")))))
          } 
          else if (input$SELplottype=="ts"){
            listane <- ane.names()
            names(listane) <- ane.names()
            listane <- as.list(listane)
            wellPanel(h4("Plot Options",align = "center"),
                      selectInput("SELaneTS","Anemometer:",
                                  listane),
                      selectInput("SELyearTS","Year:",selected = format(wd$time$dt[1],"%Y"),
                                  year.list()),
                      selectInput("SELmonthTS","Month:",selected = format(wd$time$dt[1],"%m"),
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
      else if(input$SELanalysis=="fit" | input$SELanalysis=="turbulence"){
        listane <- ane.names()
        names(listane) <- ane.names()
        listane <- as.list(listane)
        wellPanel(h4("Options",align = "center"),
                  selectInput("SELane","Anemometer:",
                              listane))
      }
    })}
  )
  
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
            tabPanel("Data", verbatimTextOutput("dataTs")
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
            tabs[[1]] <- tabPanel("All", plotOutput("plotAll"))
            skip <- 1
          }
          for (i in 1:length(ane.names())){
            tabs[[i+skip]] <- tabPanel(ane.names()[i], 
                                       tabsetPanel(
                                         tabPanel("Plot",
                                                  plotOutput(paste("plot",ane.names()[i],sep=""))),
                                         tabPanel("Data", 
                                                  tableOutput(as.name(paste("table",ane.names()[i],sep="")))) 
                                       ))
          }
          do.call(tabsetPanel, tabs)
        }
        
      } else return(NULL)
    } 
    else if(input$SELanalysis=="fit"){
      tabsetPanel(
        tabPanel("Plot", plotOutput("plotFit")),
        tabPanel("Data", htmlOutput("dataFit")))
    }
    else if (input$SELanalysis=="pc"){
      tabsetPanel(
        tabPanel("Plot", plotOutput("plotTurbine")),
        tabPanel("Data", verbatimTextOutput(("tableTurbine"))),
        tabPanel("AEP",
                 h3(paste(input$SELturbinetype,": Anual Energy Production",sep="")),
                 h3("-  "),
                 #h3(paste(round(wt(wd10, datawtg=wtgData, ane="ane10", model="E33") ,2), " M"),color="red")
                 h3(paste(switch(input$SELturbinetype,"E33"=519238,"E48"=1102365)," KWh"),sep="")
        )
      )            
    } 
    else if(input$SELanalysis=="turbulence"){
      tabsetPanel(
        tabPanel("Plot", plotOutput("plotTurbulence")),
        tabPanel("Data", verbatimTextOutput("tableTurbulence")
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
  
  observe({for (x in ane.names()){
    local({
      data <- isolate(datasetInput2())
      i <- x
      output[[paste("plot",i,sep="")]] <- renderPlot({
        if(input$SELanalysis=="plots"){
          if (input$SELplottype=="histogram" | input$SELplottype=="profile" | 
                input$SELplottype=="boxplot" | input$SELplottype=="rose"){
            if (!(input$SELplottype=="rose" & is.null(input$SELplotvarRose))){
              # Este if es porque sino la primera vez que entra ni bien se cambia el combo tira error
              plotWD(data=data,
                     var=switch(input$SELplottype,
                                rose=input$SELplotvarRose,
                                profile=input$SELplotvarProf,
                                histogram="mean"),
                     ane=i, type=input$SELplottype, 
                     by=switch(input$SELplottype,
                               rose=input$SELplotbyRose,
                               profile=input$SELplotbyProf,
                               boxplot=input$SELplotbyBox,
                               histogram=input$SELplotbyHist),
                     binwidth=input$binwidth)}
          }
          else return(NULL)
        }
      })
    })}
  })
  
  # Plot Generation All Ane
  output$plotAll <- renderPlot({
    data <- datasetInput2()
    # Aqui solo deberíamos estar en rosas y profiles
    if (!is.null(input$SELplottype)){
      plotWD(data=data,
             var=switch(input$SELplottype,
                        input$SELplotvarRose,
                        profile=input$SELplotvarProf),
             ane=ane.names(),
             type=input$SELplottype, 
             by=switch(input$SELplottype,
                       rose=input$SELplotbyRose,
                       profile=input$SELplotbyProf),
             binwidth=input$binwidth) }     
  })
  
  # Plot Serie
  output$plotTs <- renderGvis({
    if(input$SELanalysis=="plots"){
      if (input$SELplottype=="ts"){
        data <- datasetInput2()
        plotwindserie(data,
                      year=as.numeric(input$SELyearTS),
                      month=as.numeric(input$SELmonthTS),
                      ane=input$SELaneTS,
                      var=c("ave"),
                      axis=c("ave"),
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
                      ane=input$SELane,
                      shiny=T)
      } else return(NULL)
    }  else return(NULL)
  })
  
  # Table Generation
  observe({for (x in ane.names()){
    local({
      data <- isolate(datasetInput2())
      i <- x
      output[[paste("table",i,sep="")]] <- renderTable({
        if(input$SELanalysis=="plots"){
          if (input$SELplottype=="histogram" | input$SELplottype=="profile" | 
                input$SELplottype=="boxplot" | input$SELplottype=="rose"){
            as.data.frame(
              tableWD(data=data,
                      var=switch(input$SELplottype,
                                 rose=input$SELplotvarRose,
                                 profile=input$SELplotvarProf,
                                 histogram="mean"),
                      ane=i ,type=input$SELplottype, 
                      by=switch(input$SELplottype,
                                rose=input$SELplotbyRose,
                                profile=input$SELplotbyProf,
                                boxplot=input$SELplotbyBox,
                                histogram=input$SELplotbyHist),
                      binwidth=input$binwidth))
          }
          else return(NULL)
        }
      })
    })}
  })
  
  # Plot Generation All Ane
  output$tableAll <- renderTable({
    data <- datasetInput2()
    print(4)
    if (input$SELplottype=="profile" | input$SELplottype=="rose"){
      as.data.frame(
        tableWD(data=data,
                var=switch(input$SELplottype,
                           rose=input$SELplotvarRose,
                           profile=input$SELplotvarProf),
                ane=ane.names(),
                type=input$SELplottype, 
                by=switch(input$SELplottype,
                          rose=input$SELplotbyRose,
                          profile=input$SELplotbyProf),
                binwidth=input$binwidth)
      )
    }
    else return(NULL)
  })
  
  # tableTurbulence
  output$tableTurbulence <- renderPrint({
    data <- datasetInput2()
    if(input$SELanalysis=="turbulence"){
      plotWD
      #tableWD(data=data, type="turbulence", ane=input$SELane)
    } else return(NULL)
  })
  
  # plotTurbulence
  output$plotTurbulence <- renderPlot({
    if (input$SELanalysis=="turbulence"){
      data <- datasetInput2()
      plotWD(data=data, type="turbulence", ane=input$SELane)
    } else return(NULL)
  })
  
  # tableTurbine
  output$tableTurbine <- renderPrint({
    if(input$SELanalysis=="pc"){
      data <- datasetInput2()
      tablewtg(data=wtgData, model=input$SELturbinetype)
    } else return(NULL)
  })
  
  # plotTurbine
  output$plotTurbine <- renderPlot({
    if (input$SELanalysis=="pc"){
      data <- datasetInput2()
      plotwtg(data=wtgData, model=input$SELturbinetype)
    } 
  })
  
  # plotFit
  output$plotFit <- renderPlot({
    if (input$SELanalysis=="fit"){
      data <- datasetInput2()
      plotWD(data=data, ane=input$SELane, type=input$SELanalysis, binwidth=input$binwidth)
    } 
  })
  
  # tableFit
  output$dataFit <- renderTable({
    if (input$SELanalysis=="fit"){
      data <- datasetInput2()
      # Revisar DED
      as.data.frame(tableWD(data=data, var=input$SELplotvar, ane=input$SELane, type=input$SELanalysis))
    } 
  })
  
  output$dlCurPlot <- downloadHandler(
    filename = function() { paste(input$SELanalysis,
                                  '-',
                                  input$SELplottype,'.pdf', sep='')},
    content = function(file){
      pdf(file = file, width=11, height=8.5)
      data <- datasetInput2()
      plotWD(data=datasetInput2(),
              var=switch(input$SELplottype,
                         rose=input$SELplotvarRose,
                         profile=input$SELplotvarProf,
                         histogram="mean"),
              ane=ane.names()[1],
              type=input$SELplottype, 
              by=switch(input$SELplottype,
                        rose=input$SELplotbyRose,
                        profile=input$SELplotbyProf,
                        boxplot=input$SELplotbyBox,
                        histogram=input$SELplotbyHist),
              binwidth=input$binwidth)
      dev.off()
    }
  )
  
  output$dldat <- downloadHandler(
    filename = function() {paste('data-', Sys.Date(),'.txt', sep='') },
    content = function(file) {
      write.table(as.data.frame(tableWD(data=datasetInput2(),
                                      var=switch(input$SELplottype,
                                                 rose=input$SELplotvarRose,
                                                 profile=input$SELplotvarProf,
                                                 histogram="mean"),
                                      ane=ane.names()[1],
                                      type=input$SELplottype, 
                                      by=switch(input$SELplottype,
                                                rose=input$SELplotbyRose,
                                                profile=input$SELplotbyProf,
                                                boxplot=input$SELplotbyBox,
                                                histogram=input$SELplotbyHist),
                                      binwidth=input$binwidth)),
                  file=file, sep="\t", row.names=T)
    },
    contentType="text/csv"
  )
  
  
})

