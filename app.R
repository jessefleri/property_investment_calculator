library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)

ui <- page_sidebar(
  theme = bs_theme(preset = "darkly"),
  title = "Multi-Investor Property Calculator",
  sidebar = sidebar(
    width = 300,
    titlePanel("Property Parameters"),
    numericInput("principal", "Property Price ($)", value = 315000, min = 1000, step = 1000),
    numericInput("down_payment_pct", "Down Payment (%)", value = 20, min = 0, max = 100, step = 1),
    numericInput("mortgage_rate", "Mortgage Interest Rate (%/year)", value = 6.5, min = 0, max = 20, step = 0.1),
    numericInput("years", "Mortgage Term (Years)", value = 15, min = 1, max = 50, step = 1),
    numericInput("hoa_fee", "HOA Fee ($/month)", value = 0, min = 0, max = 5000, step = 25),
    hr(),
    h5("Property Growth Scenarios"),
    numericInput("rate1", "Growth Rate 1 (%/year)", value = 1, min = 0, max = 20, step = 0.1),
    numericInput("rate2", "Growth Rate 2 (%/year)", value = 2, min = 0, max = 20, step = 0.1),
    numericInput("rate3", "Growth Rate 3 (%/year)", value = 3, min = 0, max = 20, step = 0.1),
    hr(),
    h5("Add Investors"),
    textInput("investor_name", "Investor Name", value = ""),
    numericInput("investor_downpayment", "Down Payment Contribution ($)", value = 10000, min = 0, step = 100),
    numericInput("investor_monthly", "Monthly Payment Contribution ($)", value = 500, min = 0, max = 10000, step = 50),
    actionButton("add_investor", "Add Investor", class = "btn-success"),
    hr(),
    actionButton("calculate", "Calculate Future Value", class = "btn-primary")
  ),
  
  layout_columns(
    card(
      card_header("Mortgage Requirements"),
      uiOutput("mortgageInfo"),
      uiOutput("coverageStatus")
    ),
    card(
      card_header("Current Investors"),
      dataTableOutput("investorsTable"),
      actionButton("clear_investors", "Clear All Investors", class = "btn-warning")
    )
  ),
  layout_columns(
    card(
      card_header("Future Value Estimates"),
      dataTableOutput("futureValueTable")
    ),
    card(
      card_header("Value Growth Over Time"),
      plotlyOutput("growthPlot", height = "400px")
    )
  ),
  layout_columns(
    card(
      card_header("Investor Returns by Scenario"),
      dataTableOutput("investorReturnsTable")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store investors
  investors <- reactiveVal(data.frame(
    Name = character(),
    DownPayment = numeric(),
    MonthlyPayment = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Calculate mortgage requirements
  mortgageCalcs <- reactive({
    property_price <- input$principal
    down_pct <- input$down_payment_pct / 100
    mortgage_rate <- input$mortgage_rate / 100
    years <- input$years
    
    # Calculate required down payment
    required_down <- property_price * down_pct
    
    # Calculate loan amount
    loan_amount <- property_price - required_down
    
    # Calculate monthly mortgage payment using amortization formula
    if (loan_amount > 0 && mortgage_rate > 0) {
      monthly_rate <- mortgage_rate / 12
      n_payments <- years * 12
      monthly_payment <- loan_amount * (monthly_rate * (1 + monthly_rate)^n_payments) / 
        ((1 + monthly_rate)^n_payments - 1)
    } else if (loan_amount > 0) {
      monthly_payment <- loan_amount / (years * 12)
    } else {
      monthly_payment <- 0
    }
    
    # Add HOA fee to monthly payment
    hoa_fee <- input$hoa_fee
    total_monthly_payment <- monthly_payment + hoa_fee
    
    list(
      required_down = required_down,
      loan_amount = loan_amount,
      monthly_payment = monthly_payment,
      mortgage_rate_monthly = if (mortgage_rate > 0) mortgage_rate / 12 else 0,
      hoa_fee = hoa_fee,
      total_monthly_payment = total_monthly_payment,
      total_payments = total_monthly_payment * years * 12,
      total_cost = required_down + total_monthly_payment * years * 12
    )
  })
  
  # Calculate when mortgage is paid off with investor contributions
  payoffCalcs <- reactive({
    mort <- mortgageCalcs()
    totals <- investorTotals()
    
    if (nrow(investors()) == 0 || mort$loan_amount == 0) {
      return(list(
        months_to_payoff = 0,
        years_to_payoff = 0,
        is_early_payoff = FALSE
      ))
    }
    
    # Calculate actual months to pay off mortgage with investor contributions
    remaining_balance <- mort$loan_amount
    monthly_rate <- mort$mortgage_rate_monthly
    investor_monthly <- totals$total_monthly
    mortgage_only <- mort$monthly_payment
    hoa_fee <- mort$hoa_fee
    
    # Amount going to principal each month (investor payment - HOA - interest)
    months <- 0
    max_months <- input$years * 12
    
    while (remaining_balance > 0 && months < max_months) {
      months <- months + 1
      
      # Interest charged this month
      interest_payment <- remaining_balance * monthly_rate
      
      # Principal payment (investor payment minus HOA minus interest)
      principal_payment <- investor_monthly - hoa_fee - interest_payment
      
      # If principal payment exceeds remaining balance, we're done
      if (principal_payment >= remaining_balance) {
        remaining_balance <- 0
        break
      }
      
      remaining_balance <- remaining_balance - principal_payment
    }
    
    years_to_payoff <- months / 12
    is_early <- months < max_months
    
    list(
      months_to_payoff = months,
      years_to_payoff = years_to_payoff,
      is_early_payoff = is_early
    )
  })
  investorTotals <- reactive({
    inv_data <- investors()
    
    if (nrow(inv_data) == 0) {
      return(list(
        total_down = 0,
        total_monthly = 0,
        total_investment = 0
      ))
    }
    
    years <- input$years
    
    list(
      total_down = sum(inv_data$DownPayment),
      total_monthly = sum(inv_data$MonthlyPayment),
      total_investment = sum(inv_data$DownPayment) + sum(inv_data$MonthlyPayment) * 12 * years
    )
  })
  
  # Display mortgage information
  output$mortgageInfo <- renderUI({
    mort <- mortgageCalcs()
    payoff <- payoffCalcs()
    
    payoff_text <- if (payoff$is_early_payoff && nrow(investors()) > 0) {
      tags$p(style = "color: #28a745; font-weight: bold;",
             icon("bolt"), 
             sprintf(" Early Payoff: %.1f years (%.0f months)", 
                     payoff$years_to_payoff, payoff$months_to_payoff))
    } else if (nrow(investors()) > 0) {
      tags$p(sprintf("Payoff: %d years (scheduled term)", input$years))
    } else {
      NULL
    }
    
    tagList(
      p(strong("Required Down Payment: "), 
        sprintf("$%s", format(round(mort$required_down), big.mark = ",", scientific = FALSE))),
      p(strong("Loan Amount: "), 
        sprintf("$%s", format(round(mort$loan_amount), big.mark = ",", scientific = FALSE))),
      p(strong("Monthly Mortgage Payment: "), 
        sprintf("$%s", format(round(mort$monthly_payment, 2), big.mark = ",", scientific = FALSE))),
      p(strong("Monthly HOA Fee: "), 
        sprintf("$%s", format(round(mort$hoa_fee, 2), big.mark = ",", scientific = FALSE))),
      p(strong("Total Monthly Payment: "), 
        sprintf("$%s", format(round(mort$total_monthly_payment, 2), big.mark = ",", scientific = FALSE))),
      payoff_text,
      p(strong("Total to be Paid: "), 
        sprintf("$%s", format(round(mort$total_cost), big.mark = ",", scientific = FALSE)))
    )
  })
  
  # Display coverage status
  output$coverageStatus <- renderUI({
    mort <- mortgageCalcs()
    totals <- investorTotals()
    
    down_covered <- totals$total_down >= mort$required_down
    monthly_covered <- totals$total_monthly >= mort$total_monthly_payment
    
    down_status <- if (down_covered) {
      tags$p(style = "color: #28a745; font-weight: bold;",
             icon("check-circle"), " Down payment: COVERED",
             sprintf(" ($%s / $%s)", 
                     format(round(totals$total_down), big.mark = ",", scientific = FALSE),
                     format(round(mort$required_down), big.mark = ",", scientific = FALSE)))
    } else {
      tags$p(style = "color: #dc3545; font-weight: bold;",
             icon("exclamation-circle"), " Down payment: SHORT",
             sprintf(" ($%s / $%s needed)", 
                     format(round(totals$total_down), big.mark = ",", scientific = FALSE),
                     format(round(mort$required_down), big.mark = ",", scientific = FALSE)))
    }
    
    monthly_status <- if (monthly_covered) {
      tags$p(style = "color: #28a745; font-weight: bold;",
             icon("check-circle"), " Monthly payments: COVERED",
             sprintf(" ($%s / $%s)", 
                     format(round(totals$total_monthly, 2), big.mark = ",", scientific = FALSE),
                     format(round(mort$total_monthly_payment, 2), big.mark = ",", scientific = FALSE)))
    } else {
      tags$p(style = "color: #dc3545; font-weight: bold;",
             icon("exclamation-circle"), " Monthly payments: SHORT",
             sprintf(" ($%s / $%s needed)", 
                     format(round(totals$total_monthly, 2), big.mark = ",", scientific = FALSE),
                     format(round(mort$total_monthly_payment, 2), big.mark = ",", scientific = FALSE)))
    }
    
    tagList(
      hr(),
      h5("Coverage Status"),
      down_status,
      monthly_status
    )
  })
  
  # Add investor
  observeEvent(input$add_investor, {
    req(input$investor_name != "")
    
    current_investors <- investors()
    
    # Check if investor name already exists
    if (input$investor_name %in% current_investors$Name) {
      showNotification("Investor name already exists!", type = "error")
      return()
    }
    
    new_investor <- data.frame(
      Name = input$investor_name,
      DownPayment = input$investor_downpayment,
      MonthlyPayment = input$investor_monthly,
      stringsAsFactors = FALSE
    )
    
    investors(rbind(current_investors, new_investor))
    
    # Clear inputs
    updateTextInput(session, "investor_name", value = "")
    updateNumericInput(session, "investor_downpayment", value = 10000)
    updateNumericInput(session, "investor_monthly", value = 500)
    
    showNotification("Investor added successfully!", type = "message")
  })
  
  # Clear all investors
  observeEvent(input$clear_investors, {
    investors(data.frame(
      Name = character(),
      DownPayment = numeric(),
      MonthlyPayment = numeric(),
      stringsAsFactors = FALSE
    ))
    showNotification("All investors cleared!", type = "message")
  })
  
  # Validate before calculating
  observeEvent(input$calculate, {
    mort <- mortgageCalcs()
    totals <- investorTotals()
    
    if (nrow(investors()) == 0) {
      showNotification("Please add at least one investor before calculating!", type = "error")
      return()
    }
    
    if (totals$total_down < mort$required_down) {
      showNotification(
        sprintf("Insufficient down payment! Need $%s more.", 
                format(round(mort$required_down - totals$total_down), big.mark = ",", scientific = FALSE)),
        type = "error",
        duration = 5
      )
      return()
    }
    
    if (totals$total_monthly < mort$total_monthly_payment) {
      showNotification(
        sprintf("Insufficient monthly payments! Need $%s more per month.", 
                format(round(mort$total_monthly_payment - totals$total_monthly, 2), big.mark = ",", scientific = FALSE)),
        type = "error",
        duration = 5
      )
      return()
    }
  })
  
  # Display current investors
  output$investorsTable <- renderDataTable({
    inv_data <- investors()
    
    if (nrow(inv_data) == 0) {
      return(data.frame(Message = "No investors added yet"))
    }
    
    payoff <- payoffCalcs()
    months_paying <- payoff$months_to_payoff
    
    # Calculate total investment for each investor (only until mortgage is paid off)
    inv_data$TotalInvestment <- inv_data$DownPayment + 
      (inv_data$MonthlyPayment * months_paying)
    
    # Calculate ownership percentage
    total_down <- sum(inv_data$DownPayment)
    total_monthly <- sum(inv_data$MonthlyPayment)
    
    inv_data$OwnershipPct <- if (total_down + total_monthly > 0) {
      ((inv_data$DownPayment + inv_data$MonthlyPayment * months_paying) / 
         (total_down + total_monthly * months_paying)) * 100
    } else {
      0
    }
    
    display_data <- inv_data[, c("Name", "DownPayment", "MonthlyPayment", 
                                 "TotalInvestment", "OwnershipPct")]
    names(display_data) <- c("Name", "Down Payment ($)", "Monthly ($)", 
                             "Total Investment ($)", "Ownership (%)")
    
    datatable(display_data, options = list(pageLength = 10)) %>%
      formatCurrency(c('Down Payment ($)', 'Monthly ($)', 'Total Investment ($)'), 
                     currency = "$", interval = 3, mark = ",") %>%
      formatRound('Ownership (%)', digits = 2)
  })
  
  # Reactive expression to calculate future values
  futureValues <- eventReactive(input$calculate, {
    # Validation check
    mort <- mortgageCalcs()
    totals <- investorTotals()
    
    req(nrow(investors()) > 0)
    req(totals$total_down >= mort$required_down)
    req(totals$total_monthly >= mort$total_monthly_payment)
    
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
    # Validation check
    mort <- mortgageCalcs()
    totals <- investorTotals()
    
    req(nrow(investors()) > 0)
    req(totals$total_down >= mort$required_down)
    req(totals$total_monthly >= mort$total_monthly_payment)
    
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
    datatable(data, options = list(pageLength = 5)) %>% 
      formatCurrency('Future Value ($)', 
                     currency = "$", 
                     interval = 3, 
                     mark = ",")
  })
  
  # Output investor returns for each scenario
  output$investorReturnsTable <- renderDataTable({
    futureValues()  # Trigger dependency
    inv_data <- investors()
    
    if (nrow(inv_data) == 0) {
      return(data.frame(Message = "Add investors to see returns"))
    }
    
    years <- input$years
    rates <- c(input$rate1, input$rate2, input$rate3) / 100
    principal <- input$principal
    payoff <- payoffCalcs()
    
    # Calculate total months of mortgage payments (actual payoff time)
    months_paying <- payoff$months_to_payoff
    
    # Calculate actual investment per investor (only pay monthly until mortgage is paid off)
    inv_data$TotalInvestment <- inv_data$DownPayment + 
      (inv_data$MonthlyPayment * months_paying)
    
    # Calculate ownership percentages based on actual total investment
    total_down <- sum(inv_data$DownPayment)
    total_monthly <- sum(inv_data$MonthlyPayment)
    total_invested <- total_down + total_monthly * months_paying
    
    inv_data$OwnershipPct <- if (total_invested > 0) {
      (inv_data$TotalInvestment / total_invested) * 100
    } else {
      0
    }
    
    # Calculate returns for each rate scenario
    returns_data <- data.frame(Name = inv_data$Name)
    returns_data$Investment <- inv_data$TotalInvestment
    returns_data$Ownership <- inv_data$OwnershipPct
    
    for (i in 1:length(rates)) {
      future_value <- principal * (1 + rates[i])^years
      col_name <- paste0("Return_", rates[i] * 100, "%")
      returns_data[[col_name]] <- (inv_data$OwnershipPct / 100) * future_value
      
      roi_col_name <- paste0("ROI_", rates[i] * 100, "%")
      returns_data[[roi_col_name]] <- ((returns_data[[col_name]] - inv_data$TotalInvestment) / 
                                         inv_data$TotalInvestment) * 100
    }
    
    # Format column names for display
    display_names <- c("Investor", "Total Investment ($)", "Ownership (%)")
    for (i in 1:length(rates)) {
      display_names <- c(display_names, 
                         paste0("Return @ ", rates[i] * 100, "% ($)"),
                         paste0("ROI @ ", rates[i] * 100, "% (%)"))
    }
    names(returns_data) <- display_names
    
    # Format the datatable
    dt <- datatable(returns_data, options = list(pageLength = 10, scrollX = TRUE))
    
    # Format currency columns
    currency_cols <- grep("Return.*\\(\\$\\)", names(returns_data))
    for (col in currency_cols) {
      dt <- dt %>% formatCurrency(col, currency = "$", interval = 3, mark = ",")
    }
    
    # Format percentage columns
    pct_cols <- grep("(%)", names(returns_data), fixed = TRUE)
    for (col in pct_cols) {
      dt <- dt %>% formatRound(col, digits = 2)
    }
    
    # Format investment column
    dt <- dt %>% formatCurrency('Total Investment ($)', 
                                currency = "$", interval = 3, mark = ",")
    
    return(dt)
  })
  
  # Output the growth plot
  output$growthPlot <- renderPlotly({
    data <- plotData()
    payoff <- payoffCalcs()
    
    p <- ggplot(data, aes(x = Year, y = Value, color = Rate)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(
        title = "Projected Property Value Over Time",
        x = "Years",
        y = "Estimated Value ($)",
        color = "Growth Rate"
      ) +
      scale_color_viridis_d(option = "turbo", direction = -1) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
    
    # Add vertical line for mortgage payoff if early
    if (payoff$is_early_payoff && nrow(investors()) > 0) {
      p <- p + 
        geom_vline(xintercept = payoff$years_to_payoff, 
                   linetype = "dashed", 
                   color = "#28a745", 
                   linewidth = 1) +
        annotate("text", 
                 x = payoff$years_to_payoff, 
                 y = max(data$Value) * 0.95,
                 label = sprintf("Mortgage Paid Off\n(%.1f years)", payoff$years_to_payoff),
                 color = "#28a745",
                 fontface = "bold",
                 hjust = -0.1,
                 size = 3.5)
    }
    
    ggplotly(p)
  })
}

shinyApp(ui, server)