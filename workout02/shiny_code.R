source('settings_and_functions.R')

### Input widget ###
# Initial Amount : 0$ ~ 100,000$, in steps of 500$. Default value of 1,000$
Initial_Amount <- sliderInput("Initial_Amount", "Initial Amount",
                              min = 0, max = 100000,
                              value = 1000, step = 500,
                              pre = '$', sep = ",")

# Annual Contribution : 0$ ~ 50,000$, in steps of 500$. Default value of 2,000$
Annual_Contribution <- sliderInput("Annual_Contribution", "Annual Contribution",
                                   min = 0, max = 50000,
                                   value = 2000, step = 500,
                                   pre = '$', sep = ",")

# Return Rate : 0% ~ 20%, in steps of 0.1%. Default value of 5%
Return_Rate <- sliderInput("Return_Rate", "Return Rate (In %)",
                           min = 0, max = 20,
                           value = 5, step = 0.1)

# Growth Rate : 0% ~ 20%, in steps of 0.1%. Default value of 2%
Growth_Rate <- sliderInput("Growth_Rate", "Growth Rate (In %)",
                           min = 0, max = 20,
                           value = 2, step = 0.1)

# Years : 0 ~ 50, in steps of 1. Default value of 20
Years <- sliderInput("Years", "Years",
                      min = 0, max = 50,
                      value = 20, step = 1)

# Facet: 'No' , 'Yes'
Facet <- selectInput('Facet', 'Facet?',
                     choices = list('Yes' = 'Yes' , 'No' = 'No'),
                     selected = 'No')

### ui, server ###
ui <- fluidPage(
  titlePanel("Your Investment Planner"),
  fluidRow(
    column(3, Initial_Amount, Annual_Contribution),
    column(4, Return_Rate, Growth_Rate),
    column(4, Years, Facet)
  ),
  mainPanel(
    h4('Timelines'),
    plotOutput('timelines',width = 850),
    
    h4('Balances'),
    tableOutput('balances')
  )
)
  

server <- function(input, output){
  output$balances <- renderTable({
    get_balances_timelines(Initial_Amount = input$Initial_Amount ,
                           Annual_Contribution = input$Annual_Contribution,
                           Return_Rate = input$Return_Rate,
                           Growth_Rate = input$Growth_Rate,
                           Years = input$Years,
                           Facet = input$Facet)[[1]]
  })
    
  output$timelines <- renderPlot({
    get_balances_timelines(Initial_Amount = input$Initial_Amount ,
                           Annual_Contribution = input$Annual_Contribution,
                           Return_Rate = input$Return_Rate,
                           Growth_Rate = input$Growth_Rate,
                           Years = input$Years,
                           Facet = input$Facet)[[2]]
  })
  
}

shinyApp(ui, server)

rsconnect::deployApp('shiny_code.Rmd')
