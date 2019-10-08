library(shinydashboard)
library(leaflet)


dashboardPage(
  dashboardHeader(title = "Tree App"),
  dashboardSidebar(
    sliderInput("year", h5("Year"),
                min = 1995, max = 2015, value = 2015,
                step = 10, animate = TRUE,
                pre = 'Year'),
    sidebarMenu(
      menuItem("Tree Maps", tabName = "treemaps"),
      menuItem("Analytics", tabName = "analysis")
    )
  ),
  dashboardBody(
  tabItems(
    tabItem("treemaps",
      fluidRow(
        column(width = 9, 
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("treemap", height = 500))),
        column(width = 3,
               box(width = NULL,
                   helpText("Use Backspace in your keyboard to deselect items."),
                   uiOutput("treeSelect"))),
        column(width = 3,
               box(width = NULL,
                   uiOutput("boroSelect"))),
        column(width = 3,
               box(width = NULL,
                   helpText("Click to zoom in which borough you want to see!"),
                   uiOutput("boroSelect2"))),
        column(width = 3,
               box(width = NULL,
                   selectInput("pollutant", "Pollutants", 
                               choices = c("None" = "None", "PM2.5" = "pm", "Nitrogen Dioxide (NO2)" = "no2", 
                                           "Nitric Oxide (NO)" = "no", "Black Carbon" = "bc"),
                               selected = "None"))),
        column(width = 3,
               box(width = NULL,
                   selectInput("count", "Measure", 
                               choices = c("Disable", "Total number of trees" = "n",
                                           "Number of trees per KM2" = "density"),
                               selected = "Disable"))),
        column(width = 3,
               box(width = NULL,
                   actionButton("reset", "Clean All Filters.")))
        
      )
    ),
    
    tabItem("analysis",
      fluidRow(
        column(width = 5,
               box(width = NULL,
                   plotOutput("boro_count_plot")
                   )),
        column(width = 5,
               box(width = NULL,
                   plotOutput("boro_size_plot"))),
        column(width = 5,
               box(width = NULL,
                   plotOutput("health_plot"))),
        column(width = 5,
               box(width = NULL,
                   plotOutput("alive_tree_plot")))
      )
    )
  ))
)