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
  base_font = font_google("Righteous")
)

ui <- 
  navbarPage("Understanding the State of Gender Equality Globally", theme = my_theme,
             tabPanel("Home",
                      "While Gender Equality has been identified as a global goal, 
                      progress towards Gender Equality remains slow with evidence of backsliding across some key indicators. 
                      The purpose of this application is to increase awareness of the current state of affairs across regions and countries to promote positive change."),
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
             tabPanel("Scatter Plot", "lookie here its a plot that you can play with"),
             tabPanel("Get Informed & Involved", "We understand that understanding the problem is only the start to working towards solutions. 
                      The resources below provide opportunities for users to get informed on and involved in ongoing global movements towards gender equality. These resources include interventions
                      targeted at women's empowerment, initiatives to engage men as changemakers and promoters of gender equality, and resources for those who themselves may need support.",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Gender Equality in the News", 
                          "New York Times- 'Across The Globe, a Serious Backlash Against Women's Rights' 
                                   --https://www.nytimes.com/2019/12/04/us/domestic-violence-international.html",
                          
                                   "'BBC-- 'Gender study finds 90% of people are biased against women' ----- https://www.bbc.com/news/world-51751915",
                          ""
                                   , br()),
                          tabPanel("Current Interventions", "See list of ongoing interventions and ways to get involved",
                                   "Promundo--- https://promundoglobal.org/work/",
                                   "Men Engage--- https://menengage.org/",
                                   "Techno Serve--- https://www.technoserve.org/what-we-do/women/?post_type=project",
                                   "Global Giving---- https://www.globalgiving.org/search/?size=25&nextPage=1&sortField=sortorder&selectedThemes=gender&loadAllResults=true",
                                   "Women Voice Network--- https://womenvoicenetwork.com/?gclid=CjwKCAiA6seQBhAfEiwAvPqu15TX_8nrpABjwIieQdyJgTCRgwCC42rOqrwOa0OMwL_3iHk4PXtJnhoCe24QAvD_BwE",
                                   "Women for Women International--- https://www.womenforwomen.org/why-women?src=GGEV222A&ms=cpc_google_awarness&utm_medium=cpc&utm_source=google&utm_campaign=awarness&utm_content=gg+ad&gclid=CjwKCAiA6seQBhAfEiwAvPqu15YD5SrFnaJz-3sM3iKAcftHXb8qZJyfFvTu5r889wcdBEAYa4STQhoC7nwQAvD_BwE"
                                   ),  # End tabPanel "current interventions,
                          tabPanel("Resources for Support", "Here is a list of organizations that you can reach out to if you are experiencing harm/in need of support",
                                   "National Domestic Violence Hotline---- https://www.thehotline.org/",
                                   "Safe Horizon---- https://www.womenindistress.org/we-can-help/24-hour-crisis-hotline/")
                      
                        )
                      ) # end fluid page "get involved"
                      ) # end tab panel "get involved"
             )
             

### create server function:
server <- function(input, output) {
}

### combine into an app:
shinyApp(ui = ui, server = server) 