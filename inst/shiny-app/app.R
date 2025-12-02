# Shiny app for financial statement exploration and analysis
#
# This app allows users to explore and analyze financial statements data
# using the stockAnalysis package functions.

library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("Financial Statement Analysis Dashboard"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      # Input: Company selection
      selectInput("company", 
                  label = "Select Company (CIK):",
                  choices = c("AAPL" = "0000320193",  # Apple
                              "MSFT" = "0000789019",  # Microsoft
                              "GOOGL" = "0001652044", # Google/Alphabet
                              "AMZN" = "0001018724", # Amazon
                              "TSLA" = "0001318605"), # Tesla
                  selected = "0000320193"),
      
      # Input: Metrics to display
      checkboxGroupInput("metrics", 
                         label = "Select metrics to visualize:",
                         choices = c("Revenue" = "revenue",
                                     "Net Income" = "netIncome", 
                                     "Total Assets" = "totalAsset",
                                     "Total Liabilities" = "totalLiability",
                                     "Shareholders' Equity" = "shareholderEquity",
                                     "Cash Flow" = "opCashFlow"),
                         selected = c("revenue", "netIncome")),
      
      # Input: Time range
      sliderInput("year_range",
                  label = "Select Year Range:",
                  min = 2010,
                  max = as.numeric(format(Sys.Date(), "%Y")),
                  value = c(2015, as.numeric(format(Sys.Date(), "%Y"))),
                  step = 1),
      
      # Action button to trigger analysis
      actionButton("analyze_btn", "Analyze Company"),
      
      # Additional options
      br(), br(),
      h4("Analysis Options"),
      checkboxInput("log_scale", label = "Use Log Scale", value = FALSE),
      radioButtons("chart_type", 
                   label = "Chart Type",
                   choices = list("Line" = "line", 
                                  "Bar" = "bar",
                                  "Area" = "area"),
                   selected = "line")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Financial Statements", 
                 h3("Financial Statement Trends"),
                 plotOutput("financial_plot", height = "500px"),
                 br(),
                 h4("Raw Data"),
                 tableOutput("financial_data")),
        
        tabPanel("Ratios Analysis",
                 h3("Financial Ratios"),
                 fluidRow(
                   column(6, plotOutput("profitability_ratios")),
                   column(6, plotOutput("leverage_ratios"))
                 ),
                 tableOutput("ratios_table")),
        
        tabPanel("Valuation",
                 h3("Valuation Metrics"),
                 fluidRow(
                   column(6, plotOutput("valuation_plot")),
                   column(6, verbatimTextOutput("valuation_summary"))
                 )),
        
        tabPanel("Comparison",
                 h3("Company Comparison"),
                 fluidRow(
                   selectInput("compare_company1", 
                               label = "Company 1:",
                               choices = c("AAPL" = "0000320193",
                                           "MSFT" = "0000789019",
                                           "GOOGL" = "0001652044",
                                           "AMZN" = "0001018724",
                                           "TSLA" = "0001318605"),
                               selected = "0000320193"),
                   selectInput("compare_company2",
                               label = "Company 2:",
                               choices = c("MSFT" = "0000789019",
                                           "GOOGL" = "0001652044",
                                           "AMZN" = "0001018724",
                                           "TSLA" = "0001318605",
                                           "AAPL" = "0000320193"),
                               selected = "0000789019")),
                 br(),
                 plotOutput("comparison_plot", height = "500px"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store loaded data
  values <- reactiveValues(
    financial_data = NULL,
    ratios_data = NULL
  )
  
  # Event reactive to handle analysis button press
  financial_data <- eventReactive(input$analyze_btn, {
    # In a real implementation, this would call functions from the stockAnalysis package
    # For demo purposes, we'll create sample data
    
    req(input$company)
    
    # Create sample data based on selected company
    years <- input$year_range[1]:input$year_range[2]
    n_years <- length(years)
    
    # Sample financial data - in real implementation, this would come from SEC data
    sample_data <- data.frame(
      year = rep(years, 5),
      metric = rep(c("revenue", "netIncome", "totalAsset", "totalLiability", "shareholderEquity"), each = n_years),
      value = c(
        # Revenue (growing)
        seq(100, 100 + (n_years-1)*20, length.out = n_years),
        # Net Income (growing but with fluctuations)
        c(seq(5, 20, length.out = ceiling(n_years/2)), seq(20, 30, length.out = floor(n_years/2))),
        # Total Assets
        seq(50, 50 + (n_years-1)*10, length.out = n_years),
        # Total Liabilities
        seq(20, 20 + (n_years-1)*5, length.out = n_years),
        # Shareholders' Equity
        seq(30, 30 + (n_years-1)*5, length.out = n_years)
      ),
      company_cik = input$company
    )
    
    # Filter for selected metrics
    selected_data <- sample_data[sample_data$metric %in% input$metrics, ]
    return(selected_data)
  })
  
  # Render the main financial plot
  output$financial_plot <- renderPlot({
    req(financial_data())
    
    p <- ggplot(financial_data(), aes(x = year, y = value, color = metric)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = paste("Financial Statement Trends for CIK:", input$company),
        x = "Year",
        y = "Value ($ Millions)"
      ) +
      theme_minimal()
    
    if (input$log_scale) {
      p <- p + scale_y_log10()
    }
    
    # Adjust based on chart type
    if (input$chart_type == "bar") {
      p <- ggplot(financial_data(), aes(x = factor(year), y = value, fill = metric)) +
        geom_col(position = "dodge") +
        labs(
          title = paste("Financial Statement Trends for CIK:", input$company),
          x = "Year",
          y = "Value ($ Millions)"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$chart_type == "area") {
      p <- ggplot(financial_data(), aes(x = year, y = value, fill = metric)) +
        geom_area(alpha = 0.6) +
        labs(
          title = paste("Financial Statement Trends for CIK:", input$company),
          x = "Year",
          y = "Value ($ Millions)"
        ) +
        theme_minimal()
    }
    
    p
  })
  
  # Render financial data table
  output$financial_data <- renderTable({
    req(financial_data())
    # Transpose and format the data for display
    financial_data() %>%
      dplyr::select(-company_cik) %>%
      tidyr::pivot_wider(names_from = metric, values_from = value) %>%
      dplyr::arrange(desc(year))
  })
  
  # Generate ratios data
  ratios_data <- eventReactive(input$analyze_btn, {
    req(financial_data())
    
    # Calculate ratios from financial data
    revenue <- financial_data()[financial_data()$metric == "revenue", ]
    net_income <- financial_data()[financial_data()$metric == "netIncome", ]
    total_assets <- financial_data()[financial_data()$metric == "totalAsset", ]
    total_liab <- financial_data()[financial_data()$metric == "totalLiability", ]
    equity <- financial_data()[financial_data()$metric == "shareholderEquity", ]
    
    ratios_df <- data.frame(
      year = revenue$year,
      profit_margin = net_income$value / revenue$value,
      roa = net_income$value / total_assets$value,
      roe = net_income$value / equity$value,
      debt_to_equity = total_liab$value / equity$value
    )
    
    return(ratios_df)
  })
  
  # Render profitability ratios plot
  output$profitability_ratios <- renderPlot({
    req(ratios_data())
    
    ratios_long <- ratios_data() %>%
      dplyr::select(year, profit_margin, roa, roe) %>%
      tidyr::pivot_longer(cols = c(profit_margin, roa, roe), 
                          names_to = "ratio", 
                          values_to = "value")
    
    ggplot(ratios_long, aes(x = year, y = value, color = ratio)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Profitability Ratios",
        x = "Year",
        y = "Ratio"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent)
  })
  
  # Render leverage ratios plot
  output$leverage_ratios <- renderPlot({
    req(ratios_data())
    
    leverage_ratios <- ratios_data() %>%
      dplyr::select(year, debt_to_equity) %>%
      tidyr::pivot_longer(cols = debt_to_equity, 
                          names_to = "ratio", 
                          values_to = "value")
    
    ggplot(leverage_ratios, aes(x = year, y = value, color = ratio)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Leverage Ratios",
        x = "Year",
        y = "Ratio"
      ) +
      theme_minimal()
  })
  
  # Render ratios table
  output$ratios_table <- renderTable({
    req(ratios_data())
    ratios_data() %>%
      dplyr::arrange(desc(year))
  })
  
  # Render valuation plot (sample)
  output$valuation_plot <- renderPlot({
    # Sample valuation data
    valuation_data <- data.frame(
      metric = c("P/E Ratio", "P/S Ratio", "P/B Ratio", "EV/EBITDA"),
      value = c(25.3, 5.2, 3.8, 12.1)  # Sample values
    )
    
    ggplot(valuation_data, aes(x = reorder(metric, value), y = value)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      coord_flip() +
      labs(
        title = "Valuation Multiples",
        x = "Metric",
        y = "Ratio"
      ) +
      theme_minimal()
  })
  
  # Render valuation summary
  output$valuation_summary <- renderText({
    paste(
      "Based on the financial metrics and ratios analysis:",
      "- The company shows a profit margin of approximately 15-20%",
      "- Return on assets around 8-12%, indicating good asset utilization",
      "- Debt to equity ratio of 0.4-0.6, suggesting conservative leverage",
      "- P/E ratio of 25.3x, which may indicate moderate growth expectations",
      "- Current valuation appears reasonable relative to historical averages"
    )
  })
  
  # Render comparison plot
  output$comparison_plot <- renderPlot({
    # Sample comparison data
    comparison_data <- data.frame(
      company = rep(c(input$compare_company1, input$compare_company2), 4),
      metric = rep(c("Revenue", "Net Income", "Assets", "Liabilities"), each = 2),
      value = c(120, 100, 25, 20, 60, 45, 30, 25)  # Sample values
    )
    
    ggplot(comparison_data, aes(x = company, y = value, fill = metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Company Comparison",
        x = "Company (CIK)",
        y = "Value ($ Millions)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)