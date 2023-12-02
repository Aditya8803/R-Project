library(RColorBrewer)
library(shiny)
library(readr)
library(dplyr)
library(caret)
library(xgboost)
library(shinythemes)

# Load Bootstrap CSS
shinythemes::themeSelector()
theme <- shinythemes::shinytheme("flatly")  # You can choose a different Bootstrap theme

df = read_csv("C:\\Users\\ayush\\Downloads\\ds_salaries.csv")
org_df = df
org_df
df$company_size = replace(df$company_size, df$company_size == 'L', 1)
df$company_size = replace(df$company_size, df$company_size == 'M', 2)
df$company_size = replace(df$company_size, df$company_size == 'S', 3)
df = select(df, -experience_level, -employment_type, -job_title, -salary, -salary_currency, -employee_residence, -remote_ratio, -company_location)
df

set.seed(123)
train_proportion <- 0.7
train_indices <- createDataPartition(df$salary_in_usd, p = train_proportion, list = FALSE)
training_data <- df[train_indices, ]
testing_data <- df[-train_indices, ]
model <- lm(salary_in_usd ~ work_year + company_size, data = training_data)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        #salary_plot {
          border: 1px solid #ddd;
          padding: 10px;
          border-radius: 8px;
        }
        "
      )
    )
  ),
  titlePanel("Data Science Jobs Analysis"),
  tags$h4(HTML("Aditya Parashar - 21ESKCX004")),
  tags$h4(HTML("Devansh Pancholi - 21ESKCX014")),
  tags$h4(HTML("Submitted to - Sumit Kumar Mathur")),
  titlePanel("Salary Prediction"),
  theme = theme,
  sidebarLayout(
    sidebarPanel(
      selectInput("work_year", "Select Work Year", choices = c(2023, 2024, 2025, 2026, 2027)),
      selectInput("company_size", "Select Company Size", choices = c("Large (1)", "Medium (2)", "Small (3)")),
      actionButton("predict_button", "Predict Salary", class = "btn-primary"),
      br(),
      textOutput("predicted_salary")
    ),
    mainPanel(
      plotOutput("salary_plot", height = "400px"),
      titlePanel("Data Visualization In Shiny"),
      plotOutput("plot2", height = "300px"),
      plotOutput("plot3", height = "300px"),
      plotOutput("plot4", height = "300px"),
      plotOutput("plot5", height = "300px"),
      plotOutput("plot6", height = "300px"),
    )
  )
)

server <- function(input, output) {
  output$plot2 <- renderPlot({
    df2 <- org_df %>%
      group_by(experience_level) %>%
      summarise(total_count = n()) %>%
      as.data.frame()
    colors <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")  # Specify colors for each category
    pie(df2$total_count, labels = c("Entry", "Middle", "Senior", "Executive"), main = "Experience_level vs Total Count", radius = 1, col = colors)
  })
  
  output$plot3 <- renderPlot({
    df3 <- org_df %>%
      group_by(remote_ratio) %>%
      summarise(total_count = n()) %>%
      as.data.frame()
    colors <- rainbow(length(df3$total_count))  # Use rainbow colors
    pie(df3$total_count, labels = df3$remote_ratio, main = "Percentage of work done remotely", radius = 1, col = colors)
  })
  
  output$plot4 <- renderPlot({
    df4 <- org_df %>%
      group_by(company_size) %>%
      summarise(total_count = n()) %>%
      as.data.frame()
    cmp <- c("Large", "Medium", "Small")
    colors <- c("#1f78b4", "#33a02c", "#e31a1c")  # Specify colors for each bar
    barplot(df4$total_count, names.arg = cmp, main = "Scalability vs Total Count", xlab = "Company Size", ylab = "Count", col = colors)
  })
  
  output$plot5 <- renderPlot({
    df5 <- org_df %>%
      group_by(employment_type) %>%
      summarise(total_count = n()) %>%
      as.data.frame()
    emp <- c("Contract", "Freelance", "FullTime", "PartTime")
    colors <- terrain.colors(length(df5$total_count))  # Use terrain colors
    barplot(df5$total_count, names.arg = emp, main = "Employment Type vs Total Count", xlab = "Employment Type", ylab = "Count", col = colors)
  })
  
  output$plot6 <- renderPlot({
    df6 <- org_df %>%
      group_by(work_year) %>%
      summarise(total_count = n()) %>%
      as.data.frame()
    colors <- brewer.pal(length(df6$total_count), "Set3")  # Use color palette from RColorBrewer
    barplot(df6$total_count, names.arg = c("2020", "2021", "2022"), main = "Count Of Work Year", xlab = "Work Year", ylab = "Count of Employees", col = colors)
  })
  
  # Define a function to convert company_size input to numerical value
  company_size_to_numeric <- function(company_size) {
    if (company_size == "Large (1)") return(1)
    if (company_size == "Medium (2)") return(2)
    if (company_size == "Small (3)") return(3)
    return(NA)  # Return NA for unknown values
  }
  
  observeEvent(input$predict_button, {
    # Prepare input data for prediction
    new_data <- data.frame(
      work_year = as.numeric(input$work_year),
      company_size = factor(company_size_to_numeric(input$company_size), levels = c(1, 2, 3))
    )
    
    # Make the prediction using the model
    predicted_salary <- predict(model, new_data)
    
    output$predicted_salary <- renderText({
      paste("Predicted Salary: $", round(predicted_salary, 2))
    })
    
    output$salary_plot <- renderPlot({
      # Create a plot if needed
      # This is an example, you can customize the plot as per your requirements
      hist(predicted_salary, main = "Predicted Salary Distribution", xlab = "Salary in USD", col = "#4CAF50", border = "black")
    })
  })
}

shinyApp(ui, server)
