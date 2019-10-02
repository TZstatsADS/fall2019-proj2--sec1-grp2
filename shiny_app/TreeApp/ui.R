#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
boro_choices = c("Manhattan", "Brooklyn", "Queens",
                 "Staten Island", "The Bronx")
tree_choices = c("American elm", "American linden", "Amur maple", "Ash", "Atlantic white cedar", "Black cherry", "Black oak", 
                 "Callery pear", "Chinese fringetree", "Crab apple", "Crepe myrtle", "Douglas-fir", "Eastern redcedar", "Ginkgo", "Hedge maple", 
                 "Honeylocust", "Japanese zelkova", "Kentucky yellowwood", "London planetree", "Mulberry", "Northern red oak", "Norway maple", "Ohio buckeye", 
                 "Pignut hickory", "Pin oak", "Red maple", "Sawtooth oak", "Scarlet oak", "Silver linden", "Silver maple", "Sophora", 
                 "Southern magnolia", "Swamp white oak", "Sweetgum", "Sycamore maple", "Tulip-poplar", "Turkish hazelnut", "White oak", "Willow oak")

status_choices = c("Alive","Stump","Dead")
health_choices = c("Good","Fair","Poor")
# Define UI for application for the tree map.



shinyUI(fluidPage(
  
  # Application title
  titlePanel("Tree Distribution in New York City & Polution Status"),
  # Sidebar customization
  sidebarPanel(
    id = "tPanel",style = "overflow-y:scroll; max-height: 600px; position:relative;",
    actionButton("go", "Go"),
    selectInput("year", label = "Year:",
                choices = c(  "2015" = "sample_data_2015.rds",
                              "2005" = "sample_data_2005.rds",
                              "1995" = "sample_data_1995.rds"), selected = "1995"),
    
    checkboxGroupInput("boro","Borough(s): ", boro_choices, selected = "Manhattan"),
    checkboxGroupInput("tree_type","Tree Type(s): ", tree_choices, selected = tree_choices),
    checkboxGroupInput("status_status","Current Status: ", status_choices, selected = status_choices),
    checkboxGroupInput("health_status","Health Status: ", health_choices, selected = NULL)
  ),
  
  
    
    
  
  
    
    # Show a plot of the generated distribution
  mainPanel(
      leafletOutput("mymap",height = 800) 
    )
  ))

