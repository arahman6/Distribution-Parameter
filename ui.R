library(shinydashboard)
library(markdown)
reg_data_name <- c("mtcars","")

dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Distribution dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Regression", tabName = "regression", icon = icon("line-chart")
      ),
      menuItem("Plot", tabName = "dashboard", icon = icon("line-chart")),
      selectInput(
        "distName", label = "Distribution Name" ,choices = as.character(c(distrib_name,"")), selected = ""
      ),
      menuItem(
        "Codes",  icon = icon("file-text-o"),
        menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
        menuSubItem(
          "server.R", tabName = "server", icon = icon("angle-right")
        )
      ),
      menuItem(
        "ReadMe", tabName = "readme", icon = icon("mortar-board")
      ),
      menuItem(
        "Author", tabName = "author", icon = icon("user"),
        menuSubItem(
          "LinkedIn", href = "https://bd.linkedin.com/in/md-arif-rahman-46934378", icon = icon("linkedin-square")
        ),
        menuSubItem("GitHub", href = "https://github.com/arahman6/Distribution-Parameter", icon = icon("github-square"))
      )
    )
  ),
  dashboardBody(tabItems(
    tabItem(tabName = "regression",
            tabsetPanel(
              tabPanel(
                "Simple Linear Regression",
                h3("Simple Linear Regression Model"),
                fluidRow(
                  column(width = 3,
                    selectInput(
                      inputId = "regression_dat",
                      label = "Data",
                      choices = reg_data_name,
                      selected = ""
                    )
                  )
                  renderTable(),
                  column(width = 3,
                     uiOutput("s_reg_depen_var_ui")
                  ),
                  column(width = 3,
                     uiOutput("s_reg_indepen_var_ui")
                  ),
                  column(width = 3,
                    checkboxInput(
                      inputId = "load_data_sreg",
                      label = "Load Data",
                      value = FALSE
                    )
                  ),
                  conditionalPanel(
                    "input.load_data_sreg",
                    fileInput(
                      'file1', 'Choose file to upload',
                      accept = c('text/csv', 'text/comma-separated-values', 'text/tab-separated-values',
                        'text/plain', '.csv','.tsv','.sav','.xpt', '.dta','xlsx','xlx')
                    ),
                    p('Load Text, CSV, Excel, SPSS, SAS, STATA File with general settings')
                  )
                  ),
                  fluidRow(
                    box(
                      title = "Regression Parameter", status = "primary", solidHeader = TRUE,
                      uiOutput("s_regression_par1"),
                      uiOutput("s_regression_par2")
                    ),
                    box(
                      title = "Regression", status = "primary", solidHeader = TRUE,
                      plotOutput("s_regression_main", height = 300)
                    )
                  )
              ),
              tabPanel(
                "Multiple Linear Regression",
                h1("Multiple Linear Regression Model"),
                fluidRow(
                  box(
                    title = "Regression Parameter", status = "primary", solidHeader = TRUE,
                    uiOutput("m_regression_par1"),
                    uiOutput("m_regression_par2")
                  ),
                  box(
                    title = "Regression", status = "primary", solidHeader = TRUE,
                    plotOutput("m_regression_main", height = 300)
                  )
                )
              ),
              tabPanel(
                "Generalized Linear Model",
                h1("Generalized Linear Regression Model"),
                fluidRow(
                  box(
                    title = "Regression Parameter", status = "primary", solidHeader = TRUE,
                    uiOutput("g_regression_par1"),
                    uiOutput("g_regression_par2")
                  ),
                  box(
                    title = "Regression", status = "primary", solidHeader = TRUE,
                    plotOutput("g_regression_main", height = 300)
                  )
                )
              )
            )),
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
      fluidRow(infoBoxOutput("infoMean1"),
               infoBoxOutput("infoMean2")),
      fluidRow(
        infoBoxOutput("infoVarience1"),
        infoBoxOutput("infoVarience2")
      )
    ),
    tabItem(
      tabName = "ui",
      box(
        width = NULL, status = "primary", solidHeader = TRUE, title = "ui.R",
        downloadButton('downloadData2', 'Download'),
        br(),br(),
        pre(includeText("ui.R"))
      )
    ),
    tabItem(
      tabName = "server",
      box(
        width = NULL, status = "primary", solidHeader = TRUE, title = "server.R",
        downloadButton('downloadData3', 'Download'),
        br(),br(),
        pre(includeText("server.R"))
      )
    ),
    tabItem(tabName = "readme",
            withMathJax(),
            includeMarkdown("README.md"))
  ))
)