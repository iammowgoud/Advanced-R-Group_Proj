# function for installing and loading packages
load_reqs <- function(reqs) {
  for(pkg in reqs) {
    if (!(pkg %in% installed.packages())) { install.packages(pkg) }
    suppressPackageStartupMessages(library(pkg, character.only = T))
  }
}

# add libraries
load_reqs(c("shiny","shinydashboard","shinyjs","DT","shinycssloaders","tidyr", "dplyr", "data.table", "purrr", "ggplot2", "corrplot", 
            "gridExtra", "grid", "cowplot", "doParallel", "caret", "MLmetrics", "DMwR",
            "ROSE", "ranger", "xgboost", "deepboost", "highcharter", "plotly"))


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Campaign Dashboard", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(width = 300,
      menuItem("Know the Data", tabName = "data", icon = icon("line-chart"),
               menuSubItem('Variables',
                           tabName = 'vars',
                           icon = icon('line-chart')),
               menuSubItem('Correlation',
                           tabName = 'corr',
                           icon = icon('th'))
               ),
      
      menuItem("Data Modeling", tabName = "modelling", icon = icon("puzzle-piece")),
      menuItem("Predictions", tabName = "input", icon = icon("play-circle"))
    )
  ),
  dashboardBody(
    tags$head(tags$script('
        $(document).on("shiny:sessioninitialized", function(event) {
                          $(\'a[data-value="vars"]\').tab("show");
                          $(\'ul.treeview-menu\').show();
                          $(\'li.treeview\').toggleClass("active");
                          });
                          ')),
    tags$head(tags$style(HTML('
                              .sidebar-menu li>a>.fa-angle-left {
                               transform: rotate(180deg)
                              }
                              .sidebar-menu li.active>a>.fa-angle-left {
                                 top: 10%;
                                right: 0px;
                              }
                              .sidebar > ul > li> a i{
                              width: 45px !important;
                              font-size: 18px;
                              }
                              .sidebar > ul > li> a span{
                              font-size: 14px;
                              white-space: normal;
                              }
                              .shiny-text-output {
                                margin: 20px auto;
                                font-size: 18px;
                                color: #e08e0b;
                                text-align: center;
                              }
                              .main-header .logo {
                                font-weight: bold;
                              }
                              '))),
    tabItems(
      # Know the Data
      # Variables tab content
      tabItem(tabName = "vars",
              fluidRow(
                box(
                  width = 8,
                  htmlOutput("desc")
                ),
                
                box(
                  width = 4,
                  title = "Variable",
                  selectInput(inputId= "var", "Select the Variable",
                              choices=c("age", "job", "marital", "education", 
                                        "default", "balance", "housing", 
                                        "loan", "contact", "day", "month", "duration", 
                                        "campaign", "pdays", "previous", "poutcome"),
                              selected="age")
                ),
                box(plotlyOutput("histogram")),
                box(highchartOutput("highchart"))
              )
              ),
      
      # correlation tab content
      tabItem(tabName = "corr",
              fluidRow(
              )
      ),
      
      # Data Modelling tab
      tabItem(tabName = "modelling",
              fluidRow(
              box(
                  width = 4,
                  title = "Choose the model",
                  radioButtons(inputId= "modelradio","Select the Model",
                               choices=c("Logistic Regression", "Random Forest"), 
                               selected = "Random Forest")
                ),
              # Dynamic valueBoxes
              valueBoxOutput("accbox", width = 4),
              valueBoxOutput("recallbox", width = 4)
              # box(width = 12,
              #     title = "Model Summary",
              #     verbatimTextOutput("summary")
              # )
              
              ),
              fluidRow(
                box(width = 7,
                    title = "Variable Importance",
                    plotOutput("varplot") 
                ),
                box(width = 5,
                    title = "Confusion Matrix",
                    plotOutput("cmplot")) 
              )
      ),
      # Data input trial tab contents
      tabItem(tabName = "input",
              fluidRow(
                box(
                  width = 6,
                  # Input: Select a file ----
                  fileInput("file2", "Choose New Data CSV File",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  tableOutput("contentspast")
                  
                  )
              ))
      
    )
    )
    )


