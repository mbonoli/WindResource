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
    selectInput("type", "Type:",
                list(
                     "Histogram" = "histogram", 
                     "Rose" = "rose",
                     "Boxplot" = "boxplot", 
                     "Correlation" = "correlation")),
    selectInput("by", "By:",
                list("None" = "none", 
                     "Month" = "month", 
                     "Hour" = "hour")),
#     htmlOutput("selectUI"),
    checkboxInput("Ane1", "Ane1", T),
    checkboxInput("Ane2", "Ane2", T),
    submitButton("Update View"),
    downloadButton('downloadData', 'Download')
  ),
 
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h3(textOutput("caption")),
    tabsetPanel(
    tabPanel("Plot",
             h4(textOutput("captionP1")),
             plotOutput("plot1"),
             h4(textOutput("captionP2")),
             plotOutput("plot2")
             ),
#       tabPanel("Tables",
#                h4(textOutput("captionT1")), 
#                htmlOutput("table1"),
#                h4(textOutput("captionT2")),
#                htmlOutput("table2"),
#                h4(textOutput("captionT3")),
#                htmlOutput("table3")),
    tabPanel("Fit",
                h4("Fit"),
                img(imageOutput("pp",height=600))
               )
  )
)))

