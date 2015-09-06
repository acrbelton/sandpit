# load required libraries

library(shiny)

# load global variables

source("globals.R")

# define UI

shinyUI(fluidPage(
  
  # application title
  
  titlePanel("Community dashboard visualisation"),
  
  # layout with sidebar on left and main panel
  
  sidebarLayout(
    
    sidebarPanel(
      
      # slider
      
      sliderInput("scale", label = h3("Scale"), min = 1, max = 5, value = 2, step = 0.25, animate = TRUE),
      
      br(),
      
      # community-data table for debugging
      
      h3("Community data"),
      dataTableOutput("communityData"),
      
      width = 4),
 
    # main panel
    
    mainPanel(
      
      # community visualisation
      
      plotOutput("communityGraph",
                 height = "640px",
                 width = paste(640 * ( XDIM / YDIM ), "px", sep = "")),
      
      br(),
      
      hr(),
      
      # dropdown menu to select community members
      # with checkbox to highlight nearest neighbours as well
      
      uiOutput("dropdown"),
      
      checkboxInput("neighbours", 'Include nearest neighbour?', value = FALSE),
      
      br(),
      
      # text for debugging
      
      textOutput("text1"))
  )
))