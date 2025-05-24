library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)

ui <- page_sidebar(
  theme = bs_theme(preset = "darkly"),
  title = "Home Value Growth Calculator",
  sidebar = sidebar(
    width = 300,
    titlePanel("Input Parameters"),
    numericInput("principal", "Principal Amount ($)", value = 315000, min = 1000, step = 1000),
    numericInput("years", "Time Period (Years)", value = 15, min = 1, max = 50, step = 1),
    numericInput("rate1", "Interest Rate 1 (%)", value = 1, min = 0, max = 20, step = 0.1),
    numericInput("rate2", "Interest Rate 2 (%)", value = 2, min = 0, max = 20, step = 0.1),
    numericInput("rate3", "Interest Rate 3 (%)", value = 3, min = 0, max = 20, step = 0.1),
    numericInput("downpayment_contrib", "Down payment per investor ($)", value = 0, min = 1000, step = 100),
    numericInput("monthly_contrib", "Monthly payment per investor ($)", value = 0, min = 1, max = 5000, step = 1),
    actionButton("calculate", "Calculate Future Value")
  ),
  
  layout_columns(
    card(
      card_header("Future Value Estimates"),
      dataTableOutput("futureValueTable"),
      textOutput("lifetime_invested")
    ),
    card(
      card_header("Value Growth Over Time"),
      plotlyOutput("growthPlot", height = "400px")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to calculate future values
  futureValues <- eventReactive(input$calculate, {
    principal <- input$principal
    years <- input$years
    rates <- c(input$rate1, input$rate2, input$rate3) / 100
    
    # Create a data frame to store results
    results <- data.frame(
      Rate = paste0(rates * 100, "%"),
      FutureValue = sapply(rates, function(r) {
        principal * (1 + r)^years
      })
    )
    
    # Rename the column for display
    names(results)[2] <- "Future Value ($)"
    
    return(results)
  })
  
  # Reactive expression to generate data for the plot
  plotData <- eventReactive(input$calculate, {
    principal <- input$principal
    max_years <- input$years
    rates <- c(input$rate1, input$rate2, input$rate3) / 100
    
    # Create a data frame for plotting
    years_seq <- 0:max_years
    plot_data <- data.frame()
    
    for (r in rates) {
      rate_data <- data.frame(
        Year = years_seq,
        Value = principal * (1 + r)^years_seq,
        Rate = paste0(r * 100, "%")
      )
      plot_data <- rbind(plot_data, rate_data)
    }
    
    return(plot_data)
  })

  # Output the table of future values
  output$futureValueTable <- renderDataTable({
    data <- futureValues()
    # Format the numbers with commas for display only, not for calculations
    datatable(data) %>% formatCurrency('Future Value ($)', 
                            currency = "$", 
                            interval = 3, 
                            mark = ",")
  })
  
  output$lifetime_invested <- renderText({
    paste("Lifetime amount invested per investor ($):", input$downpayment_contrib + input$monthly_contrib*12*input$years)
  })
  
  # Output the growth plot
  output$growthPlot <- renderPlotly({
    data <- plotData()
    
    ggplotly(
    ggplot(data, aes(x = Year, y = Value, color = Rate)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(
        title = "Projected Home Value Over Time",
        x = "Years",
        y = "Estimated Value ($)",
        color = "Interest Rate"
      ) +
      scale_color_viridis_d(option = "turbo", direction = -1) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
    )
  })
}

shinyApp(ui, server)