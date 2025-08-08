# app_corrected.R - Fixed histogram to show proper rate change distribution
packages <- c("shiny","data.table","ggplot2","scales")
inst <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(inst)) install.packages(inst, repos="https://cloud.r-project.org")
library(shiny); library(data.table); library(ggplot2); library(scales)

scored <- fread("outputs/scored.csv")

ui <- fluidPage(
  titlePanel("Motor Pricing - Rate Change Simulator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Model for pricing", choices=c("GLM"="glm","GBM"="gbm"), selected="glm"),
      sliderInput("rate", "Global rate change percent", min=-20, max=40, value=0, step=1),
      numericInput("target_lr", "Target loss ratio", value=0.65, min=0.3, max=1.5, step=0.01),
      actionButton("apply", "Apply", class="btn-primary")
    ),
    mainPanel(
      h4("Portfolio summary"),
      verbatimTextOutput("summary"),
      h4("Premium change distribution"),
      plotOutput("hist", height="300px"),
      h4("Calibration by decile"),
      plotOutput("calplot", height="300px")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store calculation results
  values <- reactiveValues(
    summary_text = "",
    plot_data = NULL,
    cal_data = NULL,
    current_model = "glm",
    current_rate = 0
  )
  
  # Function to perform calculations
  calculate_results <- function() {
    cat("Calculating with Rate:", input$rate, "Model:", input$model, "\n")
    
    df <- copy(scored)
    pred_col <- if (input$model=="glm") "pred_glm" else "pred_gbm"
    rate_mult <- 1 + input$rate/100
    
    # Calculate premiums and loss ratios
    df[, prem_base := get(pred_col) * Exposure]
    df[, prem_new  := prem_base * rate_mult]
    df[, loss      := PurePrem * Exposure]
    
    # For a uniform rate change, ALL policies should have the same % change
    # The histogram should show this uniform change
    df[, premium_change := rate_mult - 1]  # This should be constant for all policies
    
    total_prem_base <- sum(df$prem_base, na.rm=TRUE)
    total_prem_new  <- sum(df$prem_new, na.rm=TRUE)  
    total_loss      <- sum(df$loss, na.rm=TRUE)
    
    lr_base <- if(total_prem_base > 0) total_loss / total_prem_base else 0
    lr_new  <- if(total_prem_new > 0) total_loss / total_prem_new else 0
    
    # Store results
    values$summary_text <- sprintf("Base premium %.1fM, New premium %.1fM, Base LR %.3f, New LR %.3f\nRate change: %+d%%, Model: %s",
                                   total_prem_base/1e6, total_prem_new/1e6, lr_base, lr_new,
                                   input$rate, toupper(input$model))
    
    values$plot_data <- df[prem_base > 0]  # For histogram
    values$current_model <- input$model
    values$current_rate <- input$rate
    
    # Calibration data
    df[, score := get(pred_col)]
    df[, dec := cut(rank(score, ties.method="first")/.N, 
                   breaks=seq(0,1,length.out=11), 
                   include.lowest=TRUE, labels=FALSE)]
    values$cal_data <- df[, .(
      obs = sum(PurePrem * Exposure, na.rm=TRUE)/sum(Exposure, na.rm=TRUE),
      pred = sum(score * Exposure, na.rm=TRUE)/sum(Exposure, na.rm=TRUE)
    ), by=dec]
    
    cat("Expected premium change:", round((rate_mult - 1) * 100, 1), "%\n")
    cat("Actual range:", round(range(df$premium_change) * 100, 2), "%\n")
    cat("Calculation completed!\n")
  }
  
  # Trigger calculations when Apply is clicked
  observeEvent(input$apply, {
    calculate_results()
  })
  
  # Auto-trigger on startup
  observe({
    if (is.null(values$plot_data)) {
      cat("Auto-triggering initial calculation\n")
      calculate_results()
    }
  })
  
  # Render outputs
  output$summary <- renderText({
    values$summary_text
  })
  
  output$hist <- renderPlot({
    req(values$plot_data)
    
    # Debug: check the actual distribution
    pct_changes <- values$plot_data$premium_change * 100
    cat("Premium change distribution summary:\n")
    print(summary(pct_changes))
    
    ggplot(values$plot_data, aes(x=premium_change)) +
      geom_histogram(bins=50, fill="steelblue", alpha=0.7, color="white") +
      scale_x_continuous(labels=percent_format(accuracy=0.1)) +
      labs(x="Policy premium change", y="Count", 
           title=paste("Premium Change Distribution (", values$current_rate, "% rate change)", sep="")) +
      theme_minimal() +
      theme(plot.title = element_text(size=14, hjust=0.5)) +
      geom_vline(xintercept = values$current_rate/100, color="red", linetype="dashed", size=1) +
      annotate("text", x = values$current_rate/100, y = Inf, 
               label = paste(values$current_rate, "%", sep=""), 
               color="red", vjust=2, hjust=-0.1)
  })
  
  output$calplot <- renderPlot({
    req(values$cal_data)
    
    ggplot(values$cal_data, aes(x=pred, y=obs)) +
      geom_point(size=4, color="steelblue", alpha=0.8) + 
      geom_abline(slope=1, intercept=0, linetype="dashed", color="red", linewidth=1) +
      labs(x="Predicted Pure Premium", y="Observed Pure Premium", 
           title=paste("Model Calibration:", toupper(values$current_model))) +
      theme_minimal() +
      theme(plot.title = element_text(size=14, hjust=0.5))
  })
}

shinyApp(ui, server)