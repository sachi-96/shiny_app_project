library(shiny)
library(tidyverse)
library(here)
library(bslib)

### Choose theme 
my_theme <- bs_theme(
  bg = '#0b3d91',
  fg = 'white',
  primary = '#FCC780',
  base_font = font_google("Space Mono")
)

### create user interface:
ui <- fluidPage(theme = my_theme,
  titlePanel(h1("Gender Inequality", align = "center")),
  sidebarLayout(
    sidebarPanel(selectInput(inputId = "pt_color",
                             label = "Choose Region",
                             choices = c("North America",
                                         "Central America", 
                                         "South America",
                                         "Asia",
                                         "Europe",
                                         "Africa")),
                 radioButtons(inputId = "gender_data",
                              label = "Choose Variables:",
                              choices = c("Year", 
                                          "Income", 
                                          "Education")),
    ), # end sidebar panel
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Home", "Summary of what this whole app is all about"),
                  tabPanel("Statement of the Problem", "Quick summary on state of affairs"),
                  tabPanel("Map", "Here you can see an interactive world map aint it pretty?"),
                  tabPanel("Interactive Plot", "lookie here its a plot that you can play with")),
              plotOutput(outputId = "gender_map")), # end mainPanel
    
   ), # end sidebarLayout
    
  fluidRow(
    column(4,
           
           # Copy the line below to make a slider range 
           sliderInput("slider2", label = h3("Slider Range"), min = 0, 
                       max = 100, value = c(40, 60))
    )
  ),
  
  hr(),
  
  fluidRow(
    column(4, verbatimTextOutput("value")),
    column(4, verbatimTextOutput("range"))
  )
  
)

### create server function:
server <- function(input, output) {
}

### combine into an app:
shinyApp(ui = ui, server = server) 