# shiny server
source("helpers.R") # data import and most of the preparation is performed in a separate helpers file
source("ui-helper/predict.R") 

function (input, output) {   # session is included for using shinyjs functionalities
  
  # Tab 1 A
  
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
  
  # Tab 1 B
  
  # Correlation matrix
  output$corrMixed <- renderPlot({
    train_num <- dplyr::select_if(train, is.numeric)
    res <- cor(train_num)
    
    corrplot.mixed(
      res,
      upper="circle",
      lower="number",
      tl.col = "black",
      number.cex = .7,
      tl.cex=.7)
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
        
        paste0(round(ranger_r$results[7,5],4)*100,"%"), "Recall", icon = icon("list"),
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
  
  options(shiny.maxRequestSize = 800*1024^2)   # This is a number which specifies the maximum web request size, 
  # which serves as a size limit for file uploads. 
  # If unset, the maximum request size defaults to 5MB.
  # The value I have put here is 80MB
  
  
  output$sample_input_data_heading = renderUI({   # show only if data has been uploaded
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample data')
    }
  })
  
  output$sample_input_data = renderTable({    # show sample of uploaded data
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
    
      head(input_data)
    }
  })
  
  
  
  predictions<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
        
        prediction = predict_data(input_data)

        prediction
        
      })
    }
  })
  
  
  output$sample_prediction_heading = renderUI({  # show only if data has been uploaded
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample predictions')
    }
  })
  
  output$sample_predictions = renderTable({   # the last 6 rows to show
    pred <- predictions()
    head(pred)
    
  })
  

  
  
  # Downloadable csv of predictions ----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("input_data_with_predictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(predictions(), file, row.names = FALSE)
    })
  
  
}
