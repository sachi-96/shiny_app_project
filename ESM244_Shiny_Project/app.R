library(shiny)
library(tidyverse)
library(here)
library(bslib)
library(ggplot2)
library(shinythemes)
library(readxl)
library(magrittr)
library(rvest)
library(dplyr)
library(httr)
library(maps)
library(reshape2)
library(ggiraph)
library(RColorBrewer)
library(leaflet)
library(magrittr)
library(rvest)
library(tidyr)
library(maptools)
library(rgdal)
library(rworldmap)
library(sp)
library(plotly)
library(lubridate)
library(rAmCharts)
library(rsconnect)
library(shinydashboard)

# Read in the data
# gender_data <- read_xlsx(here("data", "Gender.xlsx"))
# 
# # Tidying the data
# gender_mod <- gender_data %>%
#   rename("HDI Rank" = ...1,
#          "Country" = ...2,
#          "Gender Equality Index '18" = ...3,
#          "Rank '18" = ...5,
#          "Maternal Mortality Ratio '15" = SDG3.1,
#          "Adolescent Birth Rate '15-'20" = SDG3.7,
#          "Seats in Parliment '18" = SDG5.5,
#          "Secondary Education (F)'10-'18" = SDG4.6,
#          "Secondary Education (M)'10-'18" = ...15,
#          "Labour Force Participation (F)'18" = ...17,
#          "Labour Force Participation (M)'18" = ...19) %>%
#   select(-...4,-...6,-...8,-...10,-...12,-...14,-...16,-...18,-...20) %>%
#   filter(!row_number() %in% c(1, 2, 3, 4, 5, 228:261))

### Choose theme 
app_theme <- bs_theme(
  bg = '#0b3d91',
  fg = 'white',
  primary = '#FCC780',
  base_font = font_google("Righteous")
)

## Dashboard
header <- dashboardHeader(title = "Gender Equality")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home"),
    menuItem("Statistics by Region", tabName = "stats"),
    menuItem("Interactive Map", tabName = "map"),
    menuItem("Slider of GE Index", tabName = "slider"),
    menuItem("Scatter Plot", tabName = "scatterplot"),
    menuItem("Get Informed & Involved", tabName = "involved" )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            fluidRow(
              box(title = "Status of Gender Equality Globally",
                  h1('summary'),
                  p("While Gender Equality has been identified as a critical global goal by the UN and various multinational organizations, 
                      Gender Equality remains far from realized.The purpose of this application is to increase awareness of the current state of 
                      gender equality globally (i.e. across regions and countries) to increase awareness, give individuals resources to become changemakers in their own communities,
                      and promote positive change."))
            ), # end fluid row "home"
            fluidPage(
              mainPanel(     
              img(src = "gender-page_v-08.jpeg", height = 350, width = 350)
                                     ))),
    tabItem(tabName = "stats",
            fluidRow(
              box(title = "Statistics by World Region", 
                  h1("Different stats across world regions"),
                  p("Here you can see a Quick summary on state of affairs, i.e. women's empowerment, IPV, etc."))
            )),
    tabItem(tabName = "map",
            fluidRow(
              box(title = "Interactive world map",
                  h1('look at this interactive world map its so cool'),
                  p("wow can it be real, play with the map be amazed"))
            )),
    tabItem(tabName = "slider",
            fluidRow(
              column(4,
                     sliderInput("slider2", label = h3("Slider Range"), min = 0, max = 100, value = c(40, 60)),
              box(title = "Slider of GE index",
                  h1('slide the slider to see the different scores'),
                  p('set the value on the slider and see the output of the different scores by countries'))
            ))),
    tabItem(tabName = "scatterplot",
            fluidRow(
              box(title = "Scatterplot on Gender Equality outcomes and predictors",
                  h1("by outcome meausre and predictors"),
                  p("alter this interactive plot to see how it affects outputs"))
            )),
    tabItem(tabName = "involved",
            fluidRow(
              box(title = "Get Informed & Involved",
                  h1("Gender Equality in the News"),
                  p("New York Times- 'Across The Globe, a Serious Backlash Against Women's Rights' 
                                   --https://www.nytimes.com/2019/12/04/us/domestic-violence-international.html",
                    
                    "'BBC-- 'Gender study finds 90% of people are biased against women' ----- https://www.bbc.com/news/world-51751915",
                    ""),
                  h2("Get Involved"),
                  p("See list of ongoing interventions and ways to get involved",
                    "Promundo--- https://promundoglobal.org/work/",
                    "Men Engage--- https://menengage.org/",
                    "Techno Serve--- https://www.technoserve.org/what-we-do/women/?post_type=project",
                    "Global Giving---- https://www.globalgiving.org/search/?size=25&nextPage=1&sortField=sortorder&selectedThemes=gender&loadAllResults=true",
                    "Women Voice Network--- https://womenvoicenetwork.com/?gclid=CjwKCAiA6seQBhAfEiwAvPqu15TX_8nrpABjwIieQdyJgTCRgwCC42rOqrwOa0OMwL_3iHk4PXtJnhoCe24QAvD_BwE",
                    "Women for Women International--- https://www.womenforwomen.org/why-women?src=GGEV222A&ms=cpc_google_awarness&utm_medium=cpc&utm_source=google&utm_campaign=awarness&utm_content=gg+ad&gclid=CjwKCAiA6seQBhAfEiwAvPqu15YD5SrFnaJz-3sM3iKAcftHXb8qZJyfFvTu5r889wcdBEAYa4STQhoC7nwQAvD_BwE"))
            )
            )
    
  ) # end tabItems
) # end dashboardBody

## define UI for application
ui <- dashboardPage(header, sidebar, body, skin = "blue")

## Making the user interface
# ui <- 
#   navbarPage("Understanding the State of Gender Equality Globally", theme = app_theme,
#              tabPanel("Home",
#                       "While Gender Equality has been identified as a critical global goal by the UN and various multinational organizations, 
#                       Gender Equality remains far from realized.The purpose of this application is to increase awareness of the current state of 
#                       gender equality globally (i.e. across regions and countries) to increase awareness, give individuals resources to become changemakers in their own communities,
#                       and promote positive change.",
#                       fluidPage(
#                         mainPanel(
#                           img(src = "gender-page_v-08.jpeg", height = 350, width = 350)
#                         )
#                       )# end  fluid page "Home"
#              ), # end tab panel "home"
#              tabPanel("Statistics by Region", "Quick summary on state of affairs", # Can change later to make drop down with different outcomes to chose from
#                       fluidPage(
#                         tabsetPanel(
#                           tabPanel("Women's Empowerment Score", br(),
#                                    sidebarLayout(
#                                      sidebarPanel(
#                                        selectInput(inputId = "pt_color",
#                                                    label = "Choose World Region",
#                                                    choices = c("North America",
#                                                                "Central America", 
#                                                                "South America",
#                                                                "Asia",
#                                                                "Europe",
#                                                                "Africa")),
#                                        radioButtons(inputId = "gender_data",
#                                                     label = "Choose Variables",
#                                                     choices = c("Year", 
#                                                                 "Income", 
#                                                                 "Education"))
#                                      ), # end sidebar panel
#                                      mainPanel(
#                                        img(src = "gender-page_v-08.jpeg", height = 350, width = 350),
#                                        plotOutput(outputId = "gender_map")) # end mainpanel
#                                    ) # end sidebarLayout
#                           ), # end tabPanel "women empowerment score"
#                           tabPanel("Acceptance of IPV", br(),
#                                    sidebarLayout(
#                                      sidebarPanel(
#                                        selectInput(inputId = "pt_color",
#                                                    label = "Choose World Region",
#                                                    choices = c("North America",
#                                                                "Central America", 
#                                                                "South America",
#                                                                "Asia",
#                                                                "Europe",
#                                                                "Africa")),
#                                        radioButtons(inputId = "gender_data",
#                                                     label = "Choose Variables",
#                                                     choices = c("Year", 
#                                                                 "Income", 
#                                                                 "Education"))
#                                      ), # end sidebar panel
#                                      mainPanel(img(src = "gender-page_v-08.jpeg", height = 350, width = 350),
#                                                plotOutput(outputId = "gender_map")) # end mainpanel
#                                    ) # end sidebarLayout
#                           ), # end tabPanel "Acceptance of IPV")
#                           
#                         ) # end tabsetPanel
#                       )),
#              tabPanel("Interactive Map", 
#                       fluidPage(
#                         
#                         # App title
#                         titlePanel("Gender Gap Index Data"),
#                         
#                         # Sidebar layout with input and output definitions
#                         sidebarLayout(
#                           
#                           # Sidebar panel for inputs 
#                           sidebarPanel(
#                             
#                             # First input: Type of data
#                             selectInput(inputId = "data_type",
#                                         label = "Choose the type of data you want to see:",
#                                         choices = list("Childlessness" = "Childlessness", "Gender Gap Index" = "Gender Gap Index")),
#                             
#                             # Second input (choices depend on the choice for the first input)
#                             uiOutput("secondSelection"),
#                             
#                             # Third input (choices depend on the choice for the first and second input)
#                             uiOutput("thirdSelection")
#                             
#                           ),
#                           
#                           # Main panel for displaying outputs
#                           mainPanel(
#                             
#                             # Hide errors
#                             tags$style(type = "text/css",
#                                        ".shiny-output-error { visibility: hidden; }",
#                                        ".shiny-output-error:before { visibility: hidden; }"),
#                             
#                             # Output: interactive world map
#                             girafeOutput("distPlot")
#                             
#                           )
#                         ) #end sidebarlayout ##interactivemap
#                       ) ## end fluidpage "interactive map"
#              ),
#              tabPanel("Slider of GE Index", "Use the slider tool to select a value from the gender equality index and see which countries reflect these values",
#                       fluidRow(
#                         column(4,
#                                sliderInput("slider2", label = h3("Slider Range"), min = 0,
#                                            max = 100, value = c(40, 60))
#                         ) # end column
#                       ) # end fluid row
#              ), # end tab panel "Slider of ge"
#              tabPanel("Scatter Plot", "lookie here its a plot that you can play with",
#                       tabPanel("Scatter Plot", 
#                                plotOutput("plot1",
#                                           click = "plot_click",
#                                           dblclick = "plot_dblclick",
#                                           hover = "plot_hover",
#                                           brush = "plot_brush"
#                                ),
#                                verbatimTextOutput("info") 
#                       ) # end scatter plot
#              ), # END END 
#              tabPanel("Get Informed & Involved", "We understand that understanding the problem is only the start to working towards solutions. 
#                       The resources below provide opportunities for users to get informed on and involved in ongoing global movements towards gender equality. These resources include interventions
#                       targeted at women's empowerment, initiatives to engage men as changemakers and promoters of gender equality, and resources for those who themselves may need support.",
#                       fluidPage(
#                         tabsetPanel(
#                           tabPanel("Gender Equality in the News", 
#                                    "New York Times- 'Across The Globe, a Serious Backlash Against Women's Rights' 
#                                    --https://www.nytimes.com/2019/12/04/us/domestic-violence-international.html",
#                                    
#                                    "'BBC-- 'Gender study finds 90% of people are biased against women' ----- https://www.bbc.com/news/world-51751915",
#                                    ""
#                                    , br()),
#                           tabPanel("Current Interventions", "See list of ongoing interventions and ways to get involved",
#                                    "Promundo--- https://promundoglobal.org/work/",
#                                    "Men Engage--- https://menengage.org/",
#                                    "Techno Serve--- https://www.technoserve.org/what-we-do/women/?post_type=project",
#                                    "Global Giving---- https://www.globalgiving.org/search/?size=25&nextPage=1&sortField=sortorder&selectedThemes=gender&loadAllResults=true",
#                                    "Women Voice Network--- https://womenvoicenetwork.com/?gclid=CjwKCAiA6seQBhAfEiwAvPqu15TX_8nrpABjwIieQdyJgTCRgwCC42rOqrwOa0OMwL_3iHk4PXtJnhoCe24QAvD_BwE",
#                                    "Women for Women International--- https://www.womenforwomen.org/why-women?src=GGEV222A&ms=cpc_google_awarness&utm_medium=cpc&utm_source=google&utm_campaign=awarness&utm_content=gg+ad&gclid=CjwKCAiA6seQBhAfEiwAvPqu15YD5SrFnaJz-3sM3iKAcftHXb8qZJyfFvTu5r889wcdBEAYa4STQhoC7nwQAvD_BwE"
#                           ),  # End tabPanel "current interventions,
#                           tabPanel("Resources for Support", "Here is a list of organizations that you can reach out to if you are experiencing harm/in need of support",
#                                    "National Domestic Violence Hotline---- https://www.thehotline.org/",
#                                    "Safe Horizon---- https://www.womenindistress.org/we-can-help/24-hour-crisis-hotline/")
#                           
#                         )
#                       ) # end fluid page "get involved"
#              ) # end tab panel "get involved"
#              )



### create server function:
server <- function(input, output) { ### start server function
  
  
  # output$info <- renderText({
  #   xy_str <- function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
  #   }
  #   xy_range_str <- function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
  #            " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
  #   }
  #   
  #   paste0(
  #     "click: ", xy_str(input$plot_click),
  #     "dblclick: ", xy_str(input$plot_dblclick),
  #     "hover: ", xy_str(input$plot_hover),
  #     "brush: ", xy_range_str(input$plot_brush)
  #   )
  # }) ### end scatter plot
} ### end server function 

### combine into an app:
shinyApp(ui = ui, server = server) 