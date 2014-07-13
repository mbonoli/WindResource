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
    
    selectInput("dataset", "Choose a dataset:", 
                choices = c("sample1", "sample2", "sample3", "sample4")),
    selectInput("SELanalysis","Analysis type:",list(
      "Plots" = "plots", 
      "Turbulence" = "turbulence",
      "Fit" = "fit", 
      "Power Curve" = "pc")) ,
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