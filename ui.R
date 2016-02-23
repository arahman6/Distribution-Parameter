library(shinydashboard)

dashboardPage(
    dashboardHeader(title = "Distribution dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Plot", tabName = "dashboard", icon = icon("line-chart")),
            selectInput("distName", label = "Distribution Name" ,choices = as.character(c(distrib_name,"")), selected = ""),
            menuItem("Codes",  icon = icon("file-text-o"),
                     menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                     menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))
            ),
            menuItem("ReadMe", tabName = "readme", icon = icon("mortar-board")),
            menuItem("Author", tabName = "author", icon = icon("user"),
                     menuSubItem("LinkedIn", href = "https://bd.linkedin.com/in/md-arif-rahman-46934378", icon = icon("linkedin-square")),
                     menuSubItem("GitHub", href = "https://github.com/arahman6/Distribution-Parameter", icon = icon("github-square"))
            )
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
            tabItem(tabName = "ui",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title = "ui.R",
                         downloadButton('downloadData2', 'Download'),
                         br(),br(),
                         pre(includeText("ui.R"))
                    )
            ),
            tabItem(tabName = "server",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title = "server.R",
                         downloadButton('downloadData3', 'Download'),
                         br(),br(),
                         pre(includeText("server.R"))
                    )
            ),
            tabItem(tabName = "readme",
                    withMathJax(),
                    includeMarkdown("README.md")
            )
        )
    )
)