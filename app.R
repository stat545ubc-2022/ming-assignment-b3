#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/


# Load packages
library(shiny)
library(datateachr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(leaflet)


# Load data
# limit the trees to Japanese flowering cherry, otherwise there are too many points to display
tree <- vancouver_trees %>% 
  filter(common_name == "JAPANESE FLOWERING CHERRY") %>% 
  select(tree_id, common_name, on_street, neighbourhood_name, street_side_name, diameter, longitude, latitude)



# Define UI for application 
ui <- fluidPage(
  
  # Website title
  titlePanel( h1("Scouting for Japanese Cherry Tree in Vancouver", align = "center")),

  # customize font
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Roboto+Mono", rel = "stylesheet"),
    tags$style(HTML('
      * {
        font-family: Roboto Mono;
        font-size: 100%;
      }
      #sidebar {
         background-color: #fff;
         border: 0px
      }
    '))
  ),
  
  # description of the website
  tags$br(),
  HTML(paste0(
    "<p>Vancouver is a great place for spotting cherry blossoms during spring time!
        According to this <a href='https://www.flyovercanada.com/stories/where-to-see-cherry-blossoms-in-vancouver/'>cherry blossom guide</a>,
        there are more than 40,000 cherry trees in Vancouver, 
        with greater than 50 cultivars.
        Here, I have mapped all the locations for Japanese cherry cultivars (<i>Prunus serrulata</i>) in Vancouver.
        Feel free to explore the tree diameter by neighbourhood and the side of street below. 
        The tabs contain a dynamic histogram and data table used to produce the histogram.</p>"
  )),
  

  # place text and cherry picture on the same row
  fluidRow(
    
    # Display an image
    column(4,
           mainPanel(img(src = "cherry.png", 
                         height="400px", width = "450px"))
           ),
    
    # Display open street map with all cherry trees
    column(8,
           leafletOutput("locations", height = "400px")
    )
  ), 
  
  # add a break
  tags$br(),
  
  # Sidebar for selecting neighbourhood and side of street in Vancouver
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "neighbourhood",
                  label = "Neighbourhood",
                  choices = c(unique(tree$neighbourhood_name)),
                  size = 10, selectize = F,
                  selected = "DUNBAR-SOUTHLANDS"),
      radioButtons(inputId = "side_of_street",
                   label = "Side of Street",
                   choices = c(unique(tree$street_side_name)),
                   selected = "EVEN"),
      helpText("street side the tree is located on (odd, even, or median).")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("cherryPlot")),
                  tabPanel("Table", tableOutput("table"))
               )
  )

  )
)  



# Define server logic 
server <- function(input, output) {

    # Define server logic required to draw a map
    output$locations <- renderLeaflet({
        # set up an empty map
        locations <- leaflet(data = tree) %>% addProviderTiles("CartoDB.Positron")
        
        # put dots onto the map
        locations <- addCircleMarkers(
          locations, lng = ~ longitude, lat = ~latitude,
          clusterOptions = markerClusterOptions(),
          popup = paste("Tree:", tree$common_name,
                          "<br>", "Latitude:", tree$latitude,
                          "<br>", "Longitude:", tree$longitude,
                          "<br>", "Neighbourhood:", tree$neighbourhood_name))
    })
    
    
    # Make the input reactive
    filtered <- reactive({
      tree %>% 
        filter(neighbourhood_name == input$neighbourhood,
               street_side_name == input$side_of_street 
               )  
      
    })
    
    # Define server logic required to draw a histogram
    output$cherryPlot <- renderPlot({
      
        # draw the DBH histogram of cherry tree
        ggplot(filtered(), aes(diameter)) + 
          geom_histogram(color = "white", fill = "#f5a6a6") +
          labs(title = "Histogram of DBH", x = "Diameter at breast height (inch)", y = "Count")
    })
    
    # Define server logic required to draw a table
    output$table <- renderTable({
      filtered()
    })
}


# Run the application 
shinyApp(ui = ui, server = server)



