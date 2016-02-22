library(shinydashboard)

dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Plot", tabName = "dashboard", icon = icon("line-chart")),
            selectInput("distName", label = "Distribution Name" ,choices = as.character(c(distrib_name,"")), selected = ""),
            menuItem("ReadMe", tabName = "readme", icon = icon("th")),
            menuItem("About", tabName = "about", icon = icon("question"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "dashboard",
                # Boxes need to be put in a row (or column)
                fluidRow(
                    box(
                        title = "Probability Density Function", status = "primary", solidHeader = TRUE,
                        plotOutput("plot1", height = 250)
                    ),
                    box(
                        title = "Cumulative Distribution Function", status = "primary", solidHeader = TRUE,
                        plotOutput("plot2", height = 250)
                    )
                    

                ),
                
                fluidRow(
                    box(
                        title = "Parameters Controlers Red",status = "primary", solidHeader = TRUE,
                        uiOutput("sliderPar1_1"),
                        uiOutput("sliderPar2_1"),
                        uiOutput("sliderPar3_1")
                    ),
                    box(
                        title = "Parameters Controlers Blue",status = "primary", solidHeader = TRUE,
                        uiOutput("sliderPar1_2"),
                        uiOutput("sliderPar2_2"),
                        uiOutput("sliderPar3_2")
                    )
                ),
                fluidRow(
                    infoBoxOutput("infoMean1"),
                    infoBoxOutput("infoMean2")
                ),
                fluidRow(
                    infoBoxOutput("infoVarience1"),
                    infoBoxOutput("infoVarience2")
                )
            ),
            tabItem(
                tabName = "readme",
                h1("dsfgas")
            ),
            tabItem(
                tabName = "about",
                "arahman.isrt@gmail.com"
            )
        )
        
    )
)