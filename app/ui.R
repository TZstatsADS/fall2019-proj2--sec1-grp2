library(shinydashboard)
library(leaflet)


dashboardPage(
  skin = "green",
  dashboardHeader(title = "NYC Canopy"),
  dashboardSidebar(
    tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge , .js-irs-0 .irs-bar {background: green}',
                    '#reset{background-color:#7DCEA0}', 
                    ".main-sidebar { font-size: 18px; }",
                    ".irs-grid-text { font-size: 12px; }")),
    sliderInput("year", h5("Year"),
                min = 1995, max = 2015, value = 2015,
                step = 10, animate = TRUE,
                pre = 'Year'),
    sidebarMenu(
      menuItem("Forest", tabName = "treemaps",icon = icon("map")),
      menuItem("Update Forest", tabName = "treeupdate",icon = icon("tree")),
      menuItem("Tree Facts", tabName = "analysis", icon = icon("dashboard")),
      menuItem("Population", tabName = "analysis2",icon = icon("smile-wink"))
    
    )
  ),
  dashboardBody(
  tabItems(
    tabItem("treemaps",
            fluidRow(
              column(width = 6, 
                     box(width = NULL, solidHeader = TRUE,
                         leafletOutput("treemap", height = 750))),
              column(width = 6,
                     box(width = NULL, 
                         helpText("Use Backspace in your keyboard to deselect items"),
                         uiOutput("treeSelect")), 
                     box(width = NULL,
                         helpText("Click to zoom in which borough you want to see"),
                         uiOutput("boroSelect2"))),
              column(width = 3,
                     box(width = NULL,
                         helpText("Too see a specific borough"),
                         uiOutput("boroSelect")),
                     box(width = NULL,
                         helpText("Choose pollutant"),
                         selectInput("pollutant", "Pollutants", 
                                     choices = c("None" = "None", "PM2.5" = "pm", "Nitrogen Dioxide (NO2)" = "no2", 
                                                 "Nitric Oxide (NO)" = "no", "Black Carbon" = "bc"),
                                     selected = "None"))),
              # column(width = 3,
              #        box(width = NULL,
              #            helpText("Click to zoom in which borough you want to see!"),
              #            uiOutput("boroSelect2"))),
             
              column(width = 3,
                     box(width = NULL,
                         helpText("Measure the trees"),
                         selectInput("count", "Measure", 
                                     choices = c("Disable", "Total number of trees" = "n",
                                                 "Number of trees per KM2" = "density"),
                                     selected = "Disable")),
                     box(width = NULL,
                         actionButton("reset", "Clean All Filters")))
                     
        
      )
    ),
    
    tabItem("analysis",
      fluidRow(

        column(width = 4,
               box(width = NULL,
                   plotOutput("boro_size_plot"))),
        column(width = 4,
               box(width = NULL,
                   plotOutput("health_plot"))),
        column(width = 4,
               box(width = NULL,
                   plotOutput("alive_tree_health_plot"))),
        column(width = 4,
               box(width  = NULL,
                   plotOutput("tree_health_regions_plot"))),
        column(width = 4,
               box(width = NULL,
                   plotOutput("sidewalk_damaged_plot"))),
        column(width = 4,
               box(width = NULL,
                   plotOutput("sidewalk_damaged_plot2"))),
        column(width = 4,
               box(width  = NULL,
                   plotOutput("add1"))),
        column(width = 4,
               box(width = NULL,
                   plotOutput("add2"))),
        column(width = 4,
               box(width = NULL,
                   plotOutput("add3"))),
        column(width = 4,
               box(width = NULL,
                   plotOutput("add4"))),
        column(width = 5,
               box(width = NULL,
                   plotOutput("add5")))
        
      )
    ),
    
    tabItem("analysis2",
            fluidRow(
              tabBox(
                # Title can include an icon
                title = tagList(shiny::icon("user"), "TOP10 Largest Population"),
                tabPanel("2000",
                         box(width = NULL,
                             plotOutput("population1"))
                ),
                tabPanel("2010",
                         box(width = NULL,
                             plotOutput("population2")))),
              # column(width = 6,
              #        box(width = NULL,
              #            plotOutput("population1"))),
              # column(width = 6,
              #        box(width = NULL,
              #            plotOutput("population2"))),
              tabBox(
                # Title can include an icon
                title = tagList(shiny::icon("chart-line"), "Population Growth"),
                tabPanel("Boroughs",
                         box(width = NULL,
                             plotOutput("population3"))),
                tabPanel("Neighborhoods",
                         box(width = NULL,
                             plotOutput("population4")))),
            # fluidRow(
            #   tabBox(
            #     # Title can include an icon
            #     title = tagList(shiny::icon("user"), "TOP10 Largest Population"),
            #     tabPanel("2000",
            #              box(width = NULL,
            #                  plotOutput("population4"))
            #     )),
              # column(width = 6,
              #        box(width = NULL,
              #            plotOutput("population1"))),
              # column(width = 6,
              #        box(width = NULL,
              #            plotOutput("population2"))),
              tabBox(
                # Title can include an icon
                title = tagList(shiny::icon("balance-scale"), "Population Growth Vs Neighborhood"),
                tabPanel("2000 - 2010",
                         box(width = NULL,
                             plotOutput("population5"))
                ))
            )),
    
              
            #   column(width = 6,
            #          box(width = NULL,
            #              plotOutput("population4"))),
            #   column(width = 6,
            #          box(width = NULL,
            #              plotOutput("population5")))
            # )),
    
    tabItem("treeupdate",
            fluidRow(
              column(width = 6,
                    box(width = NULL,
                        leafletOutput("treemap2", height = 750))),
              column(width = 3,
                     box(width = NULL,
                         textInput("latitude", "Latitude", placeholder = "Enter latitude..")),
                     box(width = NULL,
                         textInput("longitude", "Longitude", placeholder = "Enter longitude.."))),
              column(width = 3,
                     box(width = NULL,
                         textInput("spc_common", "Tree Type", placeholder = "Enter tree type..")),
                     box(width = NULL,
                         selectInput("status", "Status", choices = c("Alive", "Stump", "Dead")))),
              column(width = 3,
                     box(width = NULL,
                         textInput("address", "Address", placeholder = "Enter the address for the tree.. ")),
                     box(width = NULL,
                         selectInput("borough","Borough", choices = c("Manhattan", "Brooklyn", "Queens",
                                                                      "Staten Island", "Bronx"))),
                     box(width = NULL,
                         textInput("postcode","Zipcode", placeholder = "Enter the zipcode for the tree.."))),
              column(width = 3,
                     box(width = NULL,
                         selectInput("health","Health Info", choices = c("Fair","Good","Poor"), selected ="Fair")),
                     box(width = NULL,
                         selectInput("curb_loc","Curb Location",choices = c("OnCurb","OffsetFromCurb"),selected = "OnCurb")),
                     box(width = NULL,
                         selectInput('sidewalk', "Side Walk Status", choices = c("NoDamage","Damage"), selected = "NoDamage"
                                      ))),
              column(width = 2,
                     box(width = NULL,
                     actionButton("update", "Update"))),
              column(width = 2,
                     box(width = NULL,
                     actionButton("delete","Delete"))),
              column(width = 2,
                     box(width = NULL,
                         actionButton("add","Add"))),
              column(width = 12,
                     box(width = NULL,
                         DT::dataTableOutput("test_table")))
            ))
    
  ))
)
