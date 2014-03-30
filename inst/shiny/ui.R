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
#' 
#' 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Wind Resource"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    
    selectInput("SELanalysis","Analysis type:",list(
                        "Plots" = "plots", 
                        "Turbulence" = "turbulence",
                        "Fit" = "fit", 
                        "Power Curve" = "pc")) ,
    uiOutput("UIplottype"),
    uiOutput("UIplotby2"),
    uiOutput("UIplotby3"),
    wellPanel(uiOutput("UIbin")),
    uiOutput("UIane"),
    uiOutput("UIdates"),
    downloadButton('dldat', 'Download Table'),
    downloadButton("dlCurPlot", "Download Plot")

  ),
 
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    uiOutput("UItabs")
#     h3(textOutput("caption")),
#     tabsetPanel(
#       tabPanel("Plot",
#                h4(textOutput("captionP1")),
#                plotOutput("plot1"),
#                h4(textOutput("captionP2")),
#                plotOutput("plot2")
#       ),
#       tabPanel("Tables",
#                h4(textOutput("captionT1")),
#                htmlOutput("table1"),
#                h4(textOutput("captionT2")),
#                htmlOutput("table2"),
#                h4(textOutput("captionT3")),
#                htmlOutput("table3")             
#       )
#     )
)))