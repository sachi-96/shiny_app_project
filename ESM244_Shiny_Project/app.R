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
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")

## Reading in country data
library(magrittr)
library(rvest)


## reading in some gender data
gender_index_data <- read.csv("https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/743/data.csv")
head(gender_index_data)

## cleaning it up
library(dplyr)
gender_index_data["RecentYear"] <- apply(gender_index_data, 1, function(x){as.numeric(x[max(which(!is.na(x)))])})
gender_index_data <- gender_index_data[gender_index_data$Subindicator.Type == "Rank", ] %>% 
  select(-Subindicator.Type, -Indicator.Id)
names(gender_index_data) <- c("ISO3", "Country", "Indicator", as.character(c(2006:2016, 2018)), "RecentYear")
head(gender_index_data)

## Creating map
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
head(world_data)

# changing names of countries
old_names <- c("French Southern and Antarctic Lands", "Antigua", "Barbuda", "Saint Barthelemy", "Brunei", "Ivory Coast",
               "Democratic Republic of the Congo", "Republic of Congo", "Falkland Islands", "Micronesia", "UK", 
               "Heard Island", "Cocos Islands", "Iran", "Nevis", "Saint Kitts", "South Korea", "Laos", "Saint Martin",
               "Macedonia", "Pitcairn Islands", "North Korea", "Palestine", "Russia", "South Sandwich Islands",
               "South Georgia", "Syria", "Trinidad", "Tobago", "Taiwan", "Tanzania", "USA", "Vatican", "Grenadines",
               "Saint Vincent", "Venezuela", "Vietnam", "Wallis and Fortuna")
new_names <- c("French Southern Territories", rep("Antigua and Barbuda", 2), "Saint-Barthélemy",
               "Brunei Darussalam", "Côte d'Ivoire", "Congo, (Kinshasa)", "Congo (Brazzaville)", 
               "Falkland Islands (Malvinas)", "Micronesia, Federated States of", "United Kingdom",
               "Heard and Mcdonald Islands", "Cocos (Keeling) Islands", "Iran, Islamic Republic of",
               rep("Saint Kitts and Nevis", 2), "Korea (South)", "Lao PDR", "Saint-Martin (French part)",
               "Macedonia, Republic of", "Pitcairn", "Korea (North)", "Palestinian Territory", "Russian Federation",
               rep("South Georgia and the South Sandwich Islands", 2), 
               "Syrian Arab Republic (Syria)", rep("Trinidad and Tobago", 2), "Taiwan, Republic of China",
               "Tanzania, United Republic of", "United States of America", "Holy See (Vatican City State)",
               rep("Saint Vincent and Grenadines", 2), "Venezuela (Bolivarian Republic)", "Viet Nam", "Wallis and Futuna Islands")

for (i in 1:length(old_names)){
  world_data$region[world_data$region == old_names[i]] <- new_names[i]
}

# Now the name changes for countries have been made, we can add the ISO3 codes to our world map data. 
# The gender gap index data already contain these codes, so there's no need for us to add these there.
world_data["ISO3"] <- gender_index_data$ISO3[match(world_data$region, gender_index_data$Country)]

## Now merging to datasey
library(reshape2)
gender_index_melt <- melt(gender_index_data, id = c("ISO3", "Country", "Indicator"), 
                          variable.name = "Period", value.name = "Value")

# After melting the data and ensuring they're in the same format, we merge them together using the *rbind()* 
# function, which we can do here because the data have the same colum names.

gender_index_melt["DataType"] <- rep("Gender Gap Index", nrow(gender_index_melt))
df <- rbind(gender_index_melt)
df[] <- lapply(df, as.character)
df$Value <- as.numeric(df$Value)

# Next, it's time to define the function that we'll use for building our world maps. The inputs to this 
# function are the merged data frame, the world data containing geographical coordinates, and the data type, 
# period and indicator the user will select in the R Shiny app. We first define our own theme, *my_theme()* 
# for setting the aesthetics of the plot. Next, we select only the data that the user has selected to view, 
# resulting in *plotdf*. We keep only the rows for which the ISO3 code has been specified. For some countries 
# (e.g. Channel Islands in the childlessness data), this was not the case, as these are not contained in the 
# ISO code data. We then add the data the user wants to see to the geographical world data. Finally, we plot 
# the world map. The most important part of this plot is that contained in the *geom_polygon_interactive()* 
# function from the *ggiraph* package. This function draws the world map in white with grey lines, fills it 
# up according to the value of the data selected (either childlessness or gender gap rank) in a red-to-blue 
# color scheme set using the *brewer.pal()* function from the *RColorBrewer* package, and interactively shows 
# at the tooltip the ISO3 code and value when hovering over the plot.

worldMaps <- function(df, world_data, data_type, period, indicator){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Select only the data that the user has selected to view
  plotdf <- df[df$Indicator == indicator & df$DataType == data_type & df$Period == period,]
  plotdf <- plotdf[!is.na(plotdf$ISO3), ]
  
  # Add the data the user wants to see to the geographical world data
  world_data['DataType'] <- rep(data_type, nrow(world_data))
  world_data['Period'] <- rep(period, nrow(world_data))
  world_data['Indicator'] <- rep(indicator, nrow(world_data))
  world_data['Value'] <- plotdf$Value[match(world_data$ISO3, plotdf$ISO3)]
  
  # Create caption with the data source to show underneath the map
  capt <- paste0("Source: ", ifelse(data_type ==  "United Nations" , "World Bank"))
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = subset(world_data, lat >= -60 & lat <= 90), color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Value, group = group, 
                                 tooltip = sprintf("%s<br/>%s", ISO3, Value))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
    labs(fill = data_type, color = data_type, title = NULL, x = NULL, y = NULL, caption = capt) + 
    my_theme()
  
  return(g)
}

# Read in the data
gender_data <- read_xlsx(here("data", "Gender.xlsx"))

# Tidying the data 
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
app_theme <- bs_theme(
  bg = '#0b3d91',
  fg = 'white',
  primary = '#FCC780',
  base_font = font_google("Righteous")
)


## Making the user interface
ui <- 
  navbarPage("Understanding the State of Gender Equality Globally", theme = app_theme,
             tabPanel("Home",
                      "While Gender Equality has been identified as a critical global goal by the UN and various multinational organizations, 
                      Gender Equality remains far from realized.The purpose of this application is to increase awareness of the current state of 
                      gender equality globally (i.e. across regions and countries) to increase awareness, give individuals resources to become changemakers in their own communities,
                      and promote positive change.",
                      fluidPage(
                        mainPanel(
                        img(src = "gender-page_v-08.jpeg", height = 350, width = 350),
                      )
                      )# end  fluid page "Home"
                      ), # end tab panel "home"
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
             tabPanel("Interactive Map", 
                      fluidPage(
                        
                        # App title
                        titlePanel("Gender Gap Index Data"),
                        
                        # Sidebar layout with input and output definitions
                        sidebarLayout(
                          
                          # Sidebar panel for inputs 
                          sidebarPanel(
                            
                            # First input: Type of data
                            selectInput(inputId = "data_type",
                                        label = "Choose the type of data you want to see:",
                                        choices = list("Childlessness" = "Childlessness", "Gender Gap Index" = "Gender Gap Index")),
                            
                            # Second input (choices depend on the choice for the first input)
                            uiOutput("secondSelection"),
                            
                            # Third input (choices depend on the choice for the first and second input)
                            uiOutput("thirdSelection")
                            
                          ),
                          
                          # Main panel for displaying outputs
                          mainPanel(
                            
                            # Hide errors
                            tags$style(type = "text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"),
                            
                            # Output: interactive world map
                            girafeOutput("distPlot")
                            
                          )
                        ) #end sidebarlayout ##interactivemap
                      ) ## end fluidpage "interactive map"
                      ),
             tabPanel("Slider of GE Index", "Use the slider tool to select a value from the gender equality index and see which countries reflect these values",
                      fluidRow(
                        column(4,
                               sliderInput("slider2", label = h3("Slider Range"), min = 0,
                                           max = 100, value = c(40, 60))
                        ) # end column
                        ) # end fluid row
                      ), # end tab panel "Slider of ge"
             tabPanel("Scatter Plot", "lookie here its a plot that you can play with",
                      tabPanel("Scatter Plot", 
                               plotOutput("plot1",
                                          click = "plot_click",
                                          dblclick = "plot_dblclick",
                                          hover = "plot_hover",
                                          brush = "plot_brush"
                               ),
                               verbatimTextOutput("info") 
                      ) # end scatter plot
             ), # END END 
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
server <- function(input, output) { ### start server function

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
