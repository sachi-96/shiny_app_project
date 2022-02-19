library(shiny)
library(tidyverse)
library(here)
library(bslib)
library(ggplot2)
library(shinythemes)
library(readxl)


# Read in the data
gender_data <- read_xlsx(here("data", "Gender.xlsx"))

# Data wrangling
gender_mod <- gender_data %>% 
rename("HDI Rank" = ...1,
       "Country" = ...2,
       "Gender Equality Index '18" = ...3,
       "Rank '18" = ...5,
       "Maternal Mortality Ratio '15" = SDG3.1,
       "Adolescent Birth Rate '15-'20" = SDG3.7,
       "Seats in Parliment '18" = SDG5.5,
       "Secondary Education (F)'10-'18" = SDG4.6,
       "Secondary Education (M)'10-'18" = ...15,
       "Labour Force Participation (F)'18" = ...17,
       "Labour Force Participation (M)'18" = ...19) %>% 
  select(-...4,-...6,-...8,-...10,-...12,-...14,-...16,-...18,-...20) %>% 
  filter(!row_number() %in% c(1, 2, 3, 4, 5, 228:261)) 


### Choose theme 
my_theme <- bs_theme(
  bg = '#0b3d91',
  fg = 'white',
  primary = '#FCC780',
  base_font = font_google("Righteous")
)






ui <- 
  navbarPage("Understanding the State of Gender Equality Globally", theme = my_theme,
             tabPanel("Home",
                      "While Gender Equality has been identified as a global goal, 
                      progress towards Gender Equality remains slow with evidence of backsliding across some key indicators. 
                      The purpose of this application is to increase awareness of the current state of affairs across regions and countries to promote positive change.",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Key Articles", "See the Key articles below
                                   see: https://www.nytimes.com/2019/12/04/us/domestic-violence-international.html", br()),
                          tabPanel("Current Interventions", "See list of ongoing interventions and ways to get involved"),
                          mainPanel(img(src = "gender-page_v-08.jpeg", height = 350, width = 350))
                        )
                      )),
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
                                     mainPanel(
                                       img(src = "gender-page_v-08.jpeg", height = 350, width = 350),
                                       plotOutput(outputId = "gender_map")) # end mainpanel
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
                                     mainPanel(img(src = "gender-page_v-08.jpeg", height = 350, width = 350),
                                               plotOutput(outputId = "gender_map")) # end mainpanel
                                   ) # end sidebarLayout
                          ), # end tabPanel "Acceptance of IPV")
                          
                        ) # end tabsetPanel
                      )),
             tabPanel("Interactive Map", "Here you can see an interactive world map aint it pretty?"),
             tabPanel("Slider of GE Index", "Use the slider tool to select a value from the gender equality index and see which countries reflect these values",
                      fluidRow(
                        column(4,
                               sliderInput("slider2", label = h3("Slider Range"), min = 0,
                                           max = 100, value = c(40, 60))
                        ) # end column
                        ) # end fluid row
                      ), # end tab panel "Slider of ge"
             tabPanel("Scatter Plot", "lookie here its a plot that you can play with"))
             

### create server function:
server <- function(input, output) {
}

### combine into an app:
shinyApp(ui = ui, server = server) 