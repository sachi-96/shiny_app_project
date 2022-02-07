library(shiny)
library(tidyverse)
library(here)
library(bslib)
library(echarts4r)
library(golem)
library(fullPage)

  options <- list(
    loopBottom = TRUE
  )
  
  ### Choose theme 
  my_theme <- bs_theme(
    bg = 'lightblue',
    fg = 'purple',
    primary = 'yellow',
    base_font = font_google('Fascinate')
  )
  
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
                                             label = "Choose Vairables:",
                                             choices = c("Year", 
                                                         "Income", 
                                                         "Education")),
                   ), # end sidebar panel
                   mainPanel("put map here",
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
  pagePiling(
    center = TRUE,
    sections.color = c('#f2f2f2', '#2C3E50', '#39C'),
    opts = options,
    menu = c("Home" = "home",
             "Map" = "map",
             "Series" = "ts",
             "About" = "about"
             ),
    pageSectionImage(
      center = TRUE,
      img = "https://www.gwp.org/contentassets/f385a7cd9a1a417d807a956c2087222b/gender-page_v-08.jpg",
      menu = "home",
      h1("HOME")
    ),
    pageSection(
      center = TRUE,
      menu = "map",
      h1("Map of Gender Equality Index by country")
    ),
    pageSection(
      menu = "ts",
      h1("Series")
    ),
    pageSection(
      center = TRUE,
      menu = "about",
      h1("ABOUT")
    ),
    pageSectionImage( # will not show in viewer, open in browser
      menu = "image",
      img = "https://www.gwp.org/contentassets/f385a7cd9a1a417d807a956c2087222b/gender-page_v-08.jpg",
      h1("")
    )
  )
  
  ### create server function:
  server <- function(input, output) {
  }
  
  ### combine into an app:
  shinyApp(ui = ui, server = server) 

  
  
  
  
  
  
  
  
  