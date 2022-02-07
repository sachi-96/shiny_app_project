library(shiny)
library(tidyverse)
library(here)
library(bslib)
library(ggplot2)
library(shinythemes)

### Choose theme 
my_theme <- bs_theme(
  bg = '#0b3d91',
  fg = 'white',
  primary = '#FCC780',
  base_font = font_google("Space Mono")
)

ui <- 
  navbarPage("Gender Equality", theme = my_theme,
             tabPanel("Home","Summary of what this whole app is all about"),
             tabPanel("Statistics by Region", "Quick summary on state of affairs", # Can change later to make drop down with different outcomes to chose from
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Women's Empowerment Score", br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput(inputId = "pt_color",
                                                   label = "Choose World Region",
                                                   choices = c("North America",
                                                               "Central America", 
                                                               "South America",
                                                               "Asia",
                                                               "Europe",
                                                               "Africa")),
                                       radioButtons(inputId = "gender_data",
                                                    label = "Choose Variables",
                                                    choices = c("Year", 
                                                                "Income", 
                                                                "Education")),
                                     ), # end sidebar panel
                                     mainPanel(plotOutput(outputId = "gender_map")) # end mainpanel
                                   ) # end sidebarLayout
                                   ), # end tabPanel "women empowerment score"
                          tabPanel("Acceptance of IPV", br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput(inputId = "pt_color",
                                                   label = "Choose World Region",
                                                   choices = c("North America",
                                                               "Central America", 
                                                               "South America",
                                                               "Asia",
                                                               "Europe",
                                                               "Africa")),
                                       radioButtons(inputId = "gender_data",
                                                    label = "Choose Variables",
                                                    choices = c("Year", 
                                                                "Income", 
                                                                "Education")),
                                     ), # end sidebar panel
                                     mainPanel(plotOutput(outputId = "gender_map")) # end mainpanel
                                   ) # end sidebarLayout
                          ), # end tabPanel "Acceptance of IPV")
                          
                        ) # end tabsetPanel
                      )),
             tabPanel("Interactive Map", "Here you can see an interactive world map aint it pretty?"),
             tabPanel("Slider of GE Index", "Here you can see an interactive world map aint it pretty?",
                      fluidRow(
                        column(4,
                               sliderInput("slider2", label = h3("Slider Range"), min = 0,
                                           max = 100, value = c(40, 60))
                        ) # end column
                        ) # end fluid row
                      ), # end tab panel "Slider of ge"
             tabPanel("Scatter Plot", "lookie here its a plot that you can play with"))
             
### create user interface:
# ui <- fluidPage(theme = my_theme,
#   titlePanel(h1("Gender Inequality", align = "center")),
#   sidebarLayout(
#     sidebarPanel(selectInput(inputId = "pt_color",
#                              label = "Choose Region",
#                              choices = c("North America",
#                                          "Central America", 
#                                          "South America",
#                                          "Asia",
#                                          "Europe",
#                                          "Africa")),
#                  radioButtons(inputId = "gender_data",
#                               label = "Choose Variables:",
#                               choices = c("Year", 
#                                           "Income", 
#                                           "Education")),
#     ), # end sidebar panel
#     mainPanel(
#       tabsetPanel(type = "tabs", # could also use navlist instead if want to put tabs on the side
#                   tabPanel("Home", "Summary of what this whole app is all about"),
#                   tabPanel("Statistics by region", "Quick summary on state of affairs"),
#                   tabPanel("Interactive Map", "Here you can see an interactive world map aint it pretty?"),
#                   tabPanel("Slider of GE Index", "Here you can see an interactive world map aint it pretty?"),
#                   tabPanel("Scatter Plot", "lookie here its a plot that you can play with")),
#               plotOutput(outputId = "gender_map")), # end mainPanel
#     
#    ), # end sidebarLayout
#     
#   fluidRow(
#     column(4,
# 
#            # Copy the line below to make a slider range
#            sliderInput("slider2", label = h3("Slider Range"), min = 0,
#                        max = 100, value = c(40, 60))
#     )
#   ),
# #   
#   hr(),
#   
#   fluidRow(
#     column(4, verbatimTextOutput("value")),
#     column(4, verbatimTextOutput("range"))
#   )
#   
# )

### create server function:
server <- function(input, output) {
}

### combine into an app:
shinyApp(ui = ui, server = server) 