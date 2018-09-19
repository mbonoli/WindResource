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
    uiOutput("UIdataset"),
    fileInput('file1', 'Choose file to upload',
              accept = c(
                '.RData',
                '.rda'
              )
    ),
    tags$hr(),
#     selectInput("dataset", "Choose a dataset:", 
#                 choices = c( "wdMtTom", "wdOlavarria", "wd", "wd10")),
    selectInput("SELanalysis","Analysis type:",list("Data Info" = "info",
      "Plots" = "plots", 
      "Turbulence" = "turbulence",
      "Fit" = "fit", 
      "Power Curve" = "pc",
      "Roughness" = "roughness")) ,
    uiOutput("UIplottype"),
    uiOutput("UIturbinetype"),
    uiOutput("UIwtg"),
    uiOutput("UIplotopt"),
    uiOutput("UIplotby"),
    uiOutput("UIbin"),
    uiOutput("UIdates"),
    downloadButton('dldat', 'Download Table'),
    downloadButton("dlCurPlot", "Download Plot")
    
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    uiOutput("UItabs")
  )
)
)