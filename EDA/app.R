#
#

library(shiny)
train <- read.csv('../data/BankCamp_train.csv')
numeric_vars <- unlist(lapply(train, is.numeric)) 
cat_vars <- unlist(lapply(train, is.factor)) 


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("EDA"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("select_variable", "Select the variable", colnames(train))
         #selectInput("select2", "Y - axis", colnames(train[numeric_vars]), selected = colnames(train[numeric_vars])[2])
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("histogram"),
        plotlyOutput("densityPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #submission <- read.csv('../data/BankCamp_test.csv')
   output$densityPlot <- renderPlotly({
    
     xvar <- input$select_variable
     if(xvar %in% names(numeric_vars[numeric_vars == TRUE])){
     ggplotly(
       ggdensity(train, x = xvar, add = "mean",
                 color = "y", fill = "y"#,
                 #palette = c("#0073C2FF", "#FC4E07")
                 )
     )
   }
     
   })
   
   output$histogram <- renderPlotly({
     
     xvar <- input$select_variable
     if(xvar %in% names(numeric_vars[numeric_vars == TRUE])){
     ggplotly(
       ggplot(
         train,
         aes_string(x=xvar,fill=train$y)) +
         geom_histogram(bins = 35) +
         theme_bw(base_size = 13)+
         labs(fill = "Target")
     )
     }
     else {
       ggplot(
         train[cat_vars],
         aes_string(xvar,fill=train$y)) +
         geom_bar() +
         theme_bw(base_size = 13)+
         labs(fill = "Target")
     }
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

