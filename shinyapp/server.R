# shiny server
source("helpers.R") # data import and most of the preparation is performed in a separate helpers file


function (input, output) {   # session is included for using shinyjs functionalities
  
  # Tab 1 
  
  # Variable descriptions
  
  output$desc<-renderText({
    
    varDesc <- as.data.table( read.csv("ui-helper/var_desc.csv", sep = "|", quote = '"'))
    
    paste("<p></p>",
          "<h3>",
          as.character (varDesc[varDesc$var == input$var]$title),
          "</h3>",
          "<p>",
          as.character (varDesc[varDesc$var == input$var]$desc),
          "</p>"
    )
    
  })
  
  ## Histograms
  
  output$histogram <- renderPlotly({
    
    xvar <- input$var
    
    l <- list(
      font = list(
        family = "sans-serif",
        size = 14,
        color = "#000"),
      bgcolor = "transparent",
      bordercolor = "#FFFFFF",
      borderwidth = 0)
    
    if(xvar %in% names(numeric_vars[numeric_vars == TRUE])){
      ggplotly(
        ggplot(
          eda_data,
          aes_string(x=xvar,fill=eda_data$label_edit)) +
          geom_histogram(bins = 35) +
          theme_bw(base_size = 13)+
          labs(fill = "Target") +
          ggtitle(paste("Histogram for ", xvar, sep= "")) + 
          theme(legend.key = element_rect(colour = "transparent", fill = "white"))
      ) %>% layout(plot_bgcolor='transparent') %>% 
        layout(paper_bgcolor='transparent') %>%
        layout(legend = l)
    }
    else {
      ggplotly(
        ggplot(
          eda_data,
          aes_string(xvar,fill=eda_data$label_edit)) +
          geom_bar() +
          theme_bw(base_size = 13)+
          labs(fill = "Target") + 
          ggtitle(paste("Bar chart for ", xvar, sep= "")) 
      ) %>% layout(plot_bgcolor='transparent') %>% 
        layout(paper_bgcolor='transparent') %>%
        layout(legend = l)
    }
    
  })
  
  ## Highcharter
  
  output$highchart <- renderHighchart({
    xvar <- input$var
    string <- paste("plot_", xvar, sep = "")
    get(string)
  })
  
  
  
  #   # Tab 2 - show model results
  output$accbox <- renderValueBox({
    if (input$modelradio == "Logistic Regression") {
      valueBox(
        
        paste0(round(log_up_r$results[2],4)*100,"%"), "Accuracy", icon = icon("list"),
        color = "purple"
      )
      
    } else if (input$modelradio == "Random Forest") {
      valueBox(
        
        paste0(round(ranger_r$results[1,4],4)*100,"%"), "Accuracy", icon = icon("list"),
        color = "purple"
      )
      
    }
  })
  
  output$recallbox <- renderValueBox({
    if (input$modelradio == "Logistic Regression") {
      valueBox(
        
        paste0(round(log_up_r$results[3],4)*100,"%"), "Recall", icon = icon("list"),
        color = "purple"
      )
      
    } else if (input$modelradio == "Random Forest") {
      valueBox(
        
        paste0(round(ranger_r$results[1,5],4)*100,"%"), "Recall", icon = icon("list"),
        color = "purple"
      )
      
    }
  })
  
  output$varplot <- renderPlot({
    if (input$modelradio == "Logistic Regression") {
      plot(varImp(log_up_r))
    } else if (input$modelradio == "Random Forest") {
      plot(varImp(ranger_r))
    }
  })
  
  output$cmplot <- renderPlot({
    if (input$modelradio == "Logistic Regression") {
      cmplotout
    } else if (input$modelradio == "Random Forest") {
      cmrfplotout
    }
  })
  
  
  # Tab 3 - Predictions
  
  output$contentsnew <- renderTable({
    req(input$file2)
    
    dfnew <- read.csv(input$file2$datapath,
                      header = TRUE
    )
    return(head(dfnew,2))
    
  })
  
  
  # observeEvent(input$tabs, {
  #   if(input$tabs=="data"){
  #     show("parm")
  #     hide("yearmap")
  #   }})
  # observeEvent(input$tabs, {
  #   if(input$tabs=="station"){
  #     show("yearmap")
  #     hide("parm")
  #   }})
  # observeEvent(input$tabs, {
  #   if(input$tabs=="eda"){
  #     hide("parm")
  #     hide("yearmon")
  #   }})
  # observeEvent(input$tabs, {
  #   if(input$tabs=="model"){
  #     hide("parm")
  #     hide("yearmap")
  #   }})
  
}
