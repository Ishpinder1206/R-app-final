library(shiny)
library(ggplot2)

# Creating a function for interest
interest_func <- function(cr_score) {
  if(cr_score < 693) {
    return(NULL)  # Cut-off credit score is 693, below 693, not eligible for loan
  } else if (cr_score <= 742) {
    return(10)  # Rate of interest for a score less than or equal to 742 will be 10
  } else if(cr_score <= 789) {
    return(9.5)  # Rate of interest for a score less than or equal to 789 will be 9.5
  } else {
    return(9)  # ROI for a credit score >789 will be 9
  }
}

# Calculation of EMI (equated monthly installments)
emi_func <- function(loan_amount, monthly_income, ROI, loan_period) {
  monthly_ROI <- ROI / 12 / 100 # monthly rate of interest
  emi <- loan_amount * monthly_ROI * ((1 + monthly_ROI) ^ loan_period) / ((1 + monthly_ROI) ^ loan_period - 1)
  return(emi)
}

# Function to assess credit risk on credit score and EMI/NMI ratio
cr_risk <- function(cr_score, loan_amount, monthly_income, loan_period) {
  ROI <- interest_func(cr_score)
  if(is.null(ROI)) {
    return("No loan can be sanctioned due to poor credit score.")
  }
  emi <- emi_func(loan_amount, monthly_income, ROI, loan_period)
  nmi_ratio <- (emi / monthly_income) * 100  # emi nmi ratio
  
  # Credit risk assessment according to emi/nmi and credit score
  if(nmi_ratio > 50) {
    return("Loan amount cannot be given due to high emi/nmi. Check for any existing borrowings of the customer")
  } else if(cr_score <= 742) {
    return("High chances of default. Necessary due diligence is required before sanctioning the loan. Check for any written-off or settled loans")
  } else if(cr_score <= 789) {
    return("Medium chances of default. Necessary due diligence is required before sanctioning the loan. Check for any written-off or settled loans")
  } else { 
    return("Low chances of default. Please do the necessary due diligence and check for any written-off or settled loans")
  }
}

#UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap'); 
      body {
        background-color: Lightblue;
        color: Darkblue;
      }
      h2 {
        font-family: 'Times New Roman', Times;
      }
      .shiny-input-container {
        color: #333333;
      }
      .shiny-text-output {
        font-size: 18px;
        margin-bottom: 20px;
      }
    "))
  ), #page layout
  titlePanel("Credit Risk Assessment"), 
  sidebarLayout(
    sidebarPanel(
      numericInput("cr_score", "Credit Score:", value = 700, min = 300, max = 900), # credit score
      numericInput("monthly_income", "Monthly Income after deductions ($):", value = 2000, min = 0),
      numericInput("loan_amount", "Loan Amount ($):", value = 20000, min = 0),
      numericInput("loan_period", "Loan Period (Months):", value = 12, min = 1, max = 360),
      actionButton("assess", "Assess Risk")
    ),
    mainPanel(
      textOutput("risk_assessment"), # The risk assessment text
      plotOutput("credit_score_bar") #scatterplot
    )
  )
)


server <- function(input, output) {
  observeEvent(input$assess, {
    risk <- cr_risk(input$cr_score, input$loan_amount, input$monthly_income, input$loan_period)
    output$risk_assessment <- renderText({
      risk
    })
    output$credit_score_bar <- renderPlot({
      credit_data <- data.frame(
        Category = factor(c("Total Score", "Cut-off Score", "Score of the Customer")),
        Score = c(900, 693, input$cr_score),
        Labels = c(900, 693, paste("Customer:", input$cr_score))
      )
      
      ggplot(credit_data, aes(x = Category, y = Score)) +
        geom_line(group = 1, color = "darkblue") +
        geom_point(size = 5, color = "darkblue") +
        geom_text(aes(label = Labels), nudge_y = 15, color = "darkblue", size = 6) +
        labs(title = "Credit Score Comparison", x = "", y = "Credit Score") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.text.y = element_text(color = "darkblue", size = 14),
          axis.title.y = element_text(size = 14)
        )
    }, height = 250)
  })
}


shinyApp(ui = ui, server = server)
