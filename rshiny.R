install.packages("shiny")
###########################################################
# COMPLETE R SHINY APP FOR COMPOUND POISSON-EXPONENTIAL
###########################################################

library(shiny)

# Function to simulate S(t)
simulate_S_t <- function(lambda, mu, t, n_sim = 5000) {
  S_t <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    N <- rpois(1, lambda * t)
    if (N > 0) {
      S_t[i] <- sum(rexp(N, rate = mu))
    } else {
      S_t[i] <- 0
    }
  }
  return(S_t)
}

###########################################################
# UI
###########################################################

ui <- fluidPage(
  titlePanel("Compound Poisson–Exponential Process: S(t)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parameters"),
      
      sliderInput("lambda",
                  "Interarrival rate λ:",
                  min = 0.1, max = 2, value = 0.5, step = 0.1),
      
      sliderInput("mu",
                  "Rate of Xᵢ (μ):",
                  min = 0.1, max = 2, value = 1, step = 0.1),
      
      sliderInput("time",
                  "Time t:",
                  min = 1, max = 10000, value = 10, step = 1),
      
      numericInput("n_sim",
                   "Number of simulations:",
                   value = 5000, min = 1000, max = 50000),
      
      actionButton("simulate", "Run Simulation", class = "btn-primary")
    ),
    
    mainPanel(
      h3("Histogram of S(t)"),
      plotOutput("histPlot"),
      hr(),
      h3("Summary Statistics"),
      verbatimTextOutput("summaryStats"),
      hr(),
      h3("Compare S(t) at Fixed Times"),
      plotOutput("multiHist")
    )
  )
)

###########################################################
# SERVER
###########################################################

server <- function(input, output) {
  
  # Run simulation when button is clicked
  data_sim <- eventReactive(input$simulate, {
    simulate_S_t(input$lambda, input$mu, input$time, input$n_sim)
  })
  
  # Histogram for chosen t
  output$histPlot <- renderPlot({
    req(data_sim())
    hist(data_sim(),
         breaks = 50,
         main = paste("Histogram of S(t) at t =", input$time),
         xlab = "S(t)",
         col = "skyblue",
         border = "white",
         freq = TRUE)
  })
  
  # Summary
  output$summaryStats <- renderPrint({
    s <- data_sim()
    cat("Mean of S(t):", mean(s), "\n")
    cat("Variance of S(t):", var(s), "\n")
    cat("\nSummary:\n")
    print(summary(s))
  })
  
  # Comparison histograms (t = 10, 100, 1000, 10000)
  output$multiHist <- renderPlot({
    par(mfrow = c(2,2))
    times <- c(10,100,1000,10000)
    
    for (t in times) {
      S_t <- simulate_S_t(input$lambda, input$mu, t, 3000)
      hist(S_t, breaks = 40,
           main = paste("S(t) at t =", t),
           xlab = "S(t)", col = "red", border = "white")
    }
  })
}

###########################################################
# Run Shiny App
###########################################################

shinyApp(ui, server)





































































































