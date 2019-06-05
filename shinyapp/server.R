# shiny server
source("helpers.R") # data import and most of the preparation is performed in a separate helpers file

function (input, output) {   # session is included for using shinyjs functionalities
  
  # Tab 1 
  
  # Variable descriptions
  
  output$desc<-renderText({
    if (input$var == "age") {
      paste("<p></p>",
            "<h3>Age</h3>",
            "<p>This variable describes the age of the customer.
            It is recorded as a continuous variable and it takes values from 18 to 95.</p>"
      )
    }
    
    else if (input$var == "job") {
      paste0("<p></p>",
             "<h3>Job</h3>",
             "<p>This variable describes the job of the customer.
             It takes 12 different values, such as entrepreneur, technician, housemaid,
             blue-collar, management, student, services, admin, retired, unemployed,
             self-employed, and unknown.</p>"
      )
    }
    
    else if (input$var == "marital") {
      paste("<p></p>",
            "<h3>Marital status</h3>",
            "<p>This variable desrbies the marital status of the customer.
            It has three levels: married, single, or divorced.</p>"
      )
    }
    
    else if (input$var == "education") {
      paste("<p></p>",
            "<h3>Education level</h3>",
            "<p>This variable desrbies the education level of the customer.
            It has four levels: primary, secondary, tertiary, and unknown.</p>"
      )
    }
    
    else if (input$var == "default") {
      paste("<p></p>",
            "<h3>Default status</h3>",
            "<p>This variable desrbies if the customer has credit in default..
            It takes two values, either yes or no.</p>"
      )
    }
    
    else if (input$var == "balance") {
      paste("<p></p>",
            "<h3>Account balance</h3>",
            "<p>This variable desrbies how much money the customer has on his account.
            It is a continuous variable, and it takes both negative values (up to -8019), 
            and positive values (up to 98417).</p>"
      )
    }
    
    else if (input$var == "housing") {
      paste("<p></p>",
            "<h3>Housing loan</h3>",
            "<p>This variable desrbies if the customer has an outstaiding house loan.
            It takes two values, either yes or no..</p>"
      )
    }
    
    else if (input$var == "loan") {
      paste("<p></p>",
            "<h3>Personal loan</h3>",
            "<p>This variable desrbies if the customer has an outstaiding personal loan.
            It takes two values, either yes or no.</p>"
      )
    }
    
    else if (input$var == "contact") {
      paste("<p></p>",
            "<h3>Contact information</h3>",
            "<p>This variable desrbies the type of communication contact the bank has on the customer.
            It takes values cellular, telephone, or unknown.</p>"
      )
    }
    
    else if (input$var == "day") {
      paste("<p></p>",
            "<h3>Day of the month</h3>",
            "<p>This variable desrbies the day of the month on which the customer was last contacted.
            It takes values from 1 to 31.</p>"
      )
    }
    
    else if (input$var == "month") {
      paste("<p></p>",
            "<h3>Month of the year</h3>",
            "<p>This variable desrbies the month of the year on which the customer was last contacted.
            It takes 12 values, one for each month.</p>"
      )
    }
    
    else if (input$var == "duration") {
      paste("<p></p>",
            "<h3>Duration of the call</h3>",
            "<p>This variable desrbies the duration (in secods) of the last call.
            It takes values from 0 (meaning the customer has not picked up the phone), to 4918.</p>"
      )
    }
    
    else if (input$var == "campaign") {
      paste("<p></p>",
            "<h3>Number of contacts</h3>",
            "<p>This variable describes the number of contacts performed during this campaign and for this client.
            It takes continuous values from 1 to 63.</p>"
      )
    }
    
    else if (input$var == "pdays") {
      paste("<p></p>",
            "<h3>Days passed</h3>",
            "<p>This variable describes the number of days that passed by after the client was last contacted from a previous campaign.
            It takes continuous values from -1 (meaning that the client was not previously contacted) to 854.</p>"
      )
    }
    
    else if (input$var == "previous") {
      paste("<p></p>",
            "<h3>Number of contacts</h3>",
            "<p>This variable describes the number of contacts performed before this campaign and for this client.
            It takes continuous values from 0 to 58.</p>"
      )
    }
    
    else {
      paste("<p></p>",
            "<h3>Outcome</h3>",
            "<p>This variable describes the outcome of the previous marketing campaign.
            It takes values success, failure, other, and unknown.</p>"
      )
    }
    
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
#   # Logistic
  output$accbox <- renderValueBox({
    if (input$modelradio == "Logistic Regression") {
      valueBox(
        
          paste0(round(log_up_r$results[2],4)*100,"%"), "Accuracy", icon = icon("list"),
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

    }
  })

  output$varplot <- renderPlot({
    if (input$modelradio == "Logistic Regression") {
    plot(varImp(log_up_r))
    }
  })

  output$cmplot <- renderPlot({
    if (input$modelradio == "Logistic Regression") {
      cmplotout
    }
  })
  
  # Random Forest
  
  output$accbox <- renderValueBox({
    if (input$modelradio == "Random Forest") {
      valueBox(
        
          paste0(round(ranger_r$results[1,4],4)*100,"%"), "Accuracy", icon = icon("list"),
        color = "purple"
      )

    }
  })

  output$recallbox <- renderValueBox({
    if (input$modelradio == "Random Forest") {
      valueBox(
        
          paste0(round(ranger_r$results[1,5],4)*100,"%"), "Recall", icon = icon("list"),
        color = "purple"
      )

    }
  })

  output$varplot <- renderPlot({
    if (input$modelradio == "Random Forest") {
      plot(varImp(ranger_r))
    }
  })

  output$cmplot <- renderPlot({
    if (input$modelradio == "Random Forest") {
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
