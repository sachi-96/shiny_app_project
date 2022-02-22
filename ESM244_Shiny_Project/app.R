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
library(DT)

# Read in the data
gender_data <- read_xlsx(here("data", "Gender.xlsx"))

# # Tidying the data
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

## Creating a subset of the data to use to make the map
map_data <- gender_mod %>% 
  janitor::clean_names()

## dropping rows that do not contain countries
map_data <- map_data %>% slice(-c(193, 200:222))
## need to drop some more--- 
map_data <- map_data %>% slice(-c(63, 118, 156))

## now putting in alphabetical order
map_data <- map_data %>% arrange(country)
# Adding ISO3 codes to country data to use as join key
ISO3 <- c("AFG",
          "ALB",
          "DZA",
          "AGO",
          "ATG",
          "ARG",
          "ARM",
          "AUS",
          "AUT",
          "AZE",
          "BHS",
          "BHR",
          "BGD",
          "BRB",
          "BlR",
          "BEL",
          "BLZ",
          "BEN",
          "BTN",
          "BOL",
          "BIH",
          "BWA",
          "BRA",
          "BRN",
          "BGR",
          "BFA",
          "BDI",
          "KHM",
          "CMR",
          "CAN",
          "CPV",
          "CAF",
          "TCD",
          "CHL",
          "CHN",
          "COL",
          "COM",
          "COG",
          "COD",
          "CRI",
          "CIV",
          "HRV",
          "CUB",
          "CYP",
          "CZE",
          "DNK",
          "DJI",
          "DMA",
          "DOM",
          "ECU",
          "EGY",
          "SLV",
          "GNQ",
          "ERI",
          "EST",
          "ETH",
          "FJI",
          "FIN",
          "FRA",
          "GAB",
          "GEO",
          "DEU",
          "GHA",
          "GRC",
          "GRD",
          "GTM",
          "GIN",
          "GNB",
          "GUY",
          "HTI",
          "HND",
          "HKG",
          "HUN",
          "ISL",
          "IND",
          "IDN",
          "IRN",
          "IRQ",
          "IRL",
          "ISR",
          "PSE",
          "ITA",
          "JAM",
          "JPN",
          "JOR",
          "KAZ",
          "KEN",
          "KIR",
          "KOS",
          "KWT",
          "KGZ",
          "LAO",
          "LVA",
          "LBN",
          "LSO",
          "LBR",
          "LBY",
          "LIE",
          "LTU",
          "LUX",
          "MKD",
          "MDG",
          "MWI",
          "MYS",
          "MDV",
          "MLI",
          "MLT",
          "MHL",
          "MRT",
          "MUS",
          "MEX",
          "FSM",
          "MDA",
          "MCO",
          "MNG",
          "MNE",
          "MAR",
          "MOZ",
          "MMR",
          "NAM",
          "NRU",
          "NPL",
          "NLD",
          "NZL",
          "NIC",
          "NER",
          "NGA",
          "PRK",
          "NOR",
          "OMN",
          "PAK",
          "PLW",
          "PAN",
          "PNG",
          "PRY",
          "PER",
          "PHL",
          "POL",
          "PRT",
          "QAT",
          "ROU",
          "RUS",
          "RWA",
          "KNA",
          "LCA",
          "VCT",
          "WSM",
          "SMR",
          "STP",
          "SAU",
          "SEN",
          "SRB",
          "SYC",
          "SLE",
          "SGP",
          "SVK",
          "SVN",
          "SLB",
          "SOM",
          "ZAF",
          "KOR",
          "SON",
          "ESP",
          "LKA",
          "SON",
          "SUR",
          "SWZ",
          "SWE",
          "CHE",
          "SYR",
          "TWN",
          "TJK",
          "TZA",
          "THA",
          "GMB",
          "TLS",
          "TGO",
          "TON",
          "TTO",
          "TUN",
          "TUR",
          "TKM",
          "TUV",
          "UGA",
          "UKR",
          "ARE",
          "GBR",
          "USA",
          "URY",
          "UZB",
          "VUT",
          "VEN",
          "VNM",
          "PSE",
          "YEM",
          "ZMB",
          "ZWE") 

## cant seem to get this to work I shall return
# i <- rep(ISO3, each = 80)
# map_data <- map_data %>% mutate("ISO3" = ISO3)
# pfg <- pfg %>% rename(ISO3 = i)

# Creating data used to rank country press freedoms

map.rankings <- map_data$

# Creating the Map Data
# pfgts <- pfg %>% filter(measure %in% "Total_Score")
# pal <- colorBin("YlOrRd", domain = pfgts$score, bins = 5)




# Interactive Data Table
map_data_table <- map_data %>%
  group_by(country, gender_equality_index_18) 

map_data_table$Status <- NULL

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
                  p("Here you can see a Quick summary on state of affairs, i.e. women's empowerment, IPV, etc."),
                  DTOutput('table'))
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

### Choose theme 
app_theme <- bs_theme(
  bg = '#0b3d91',
  fg = 'white',
  primary = '#FCC780',
  base_font = font_google("Righteous")
)

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
  
  ## Interactive map function---- need to edit to make work for my data
  # observe({
  #   if(!is.null(input$year)){
  #     map <- joinCountryData2Map(selected(), joinCode = "ISO3",
  #                                nameJoinColumn = "ISO3")
  #     leafletProxy("worldmap", data = map) %>%
  #       addTiles() %>% 
  #       clearShapes() %>% 
  #       addPolygons(fillColor = ~pal(map$score),
  #                   weight = 2,
  #                   opacity = 1,
  #                   color = "white",
  #                   dashArray = "3",
  #                   fillOpacity = 0.7,
  #                   highlight = highlightOptions(
  #                     weight = 5,
  #                     color = "white",
  #                     dashArray = "3",
  #                     fillOpacity = .8,
  #                     bringToFront = TRUE),
  #                   label = ~paste(as.character(map$country),
  #                                  "Total Index Score: ", as.character(map$score)))
  #   }})
  
  output$table <- renderDT({
    map_data_table
  })
  
  ## Scatterplot function
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  }) ### end scatter plot
} ### end server function 

### combine into an app:
shinyApp(ui = ui, server = server) 