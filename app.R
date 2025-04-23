# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

# Load dataset
data <- read.csv("C:/Users/Vikash/Downloads/Salary_Data_Based_country_and_race.csv")
colnames(data) <- make.names(colnames(data))
data <- data %>% filter(!is.na(Salary), !is.na(Years.of.Experience), !is.na(Age))

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "position: relative; width: 100%;",
      tags$div("SALARY DASHBOARD",
               style = "position: absolute; left: 50%; transform: translateX(-50%); font-weight: bold; font-size: 22px; color: white;"),
      tags$div("by Akhilesh Prajapati",
               style = "position: absolute; right: 10px; top: 0; font-size: 14px; color: white;")
    ),
    titleWidth = "100%"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      selectInput("country", "Country:", choices = c("All", unique(data$Country))),
      selectInput("race", "Race:", choices = c("All", unique(data$Race))),
      selectInput("gender", "Gender:", choices = c("All", unique(data$Gender))),
      selectInput("education", "Education Level:", choices = c("All", unique(data$Education.Level))),
      sliderInput("ageRange", "Age Range:", 
                  min = min(data$Age), max = max(data$Age), value = range(data$Age)),
      sliderInput("expRange", "Experience (Years):",
                  min = min(data$Years.of.Experience), max = max(data$Years.of.Experience),
                  value = range(data$Years.of.Experience)),
      downloadButton("downloadData", "Download Filtered Data")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper {
          background: url('https://images.unsplash.com/photo-1504384308090-c894fdcc538d') no-repeat center center fixed !important;
          background-size: cover !important;
        }
        .box, .value-box {
          background-color: rgba(255, 255, 255, 0.85) !important;
          border-radius: 10px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        .main-header, .main-sidebar {
          background-color: #222d32 !important;
        }
        .skin-blue .main-header .logo {
          background-color: #222d32 !important;
        }
      "))
    ),
    fluidRow(
      valueBoxOutput("avgSalaryBox"),
      valueBoxOutput("expBox"),
      valueBoxOutput("countBox")
    ),
    fluidRow(
      box(width = 6, plotlyOutput("salaryPlot", height = "300px"), title = "Avg Salary by Job Category"),
      box(width = 6, plotlyOutput("educationPlot", height = "300px"), title = "Avg Salary by Education Level")
    ),
    fluidRow(
      box(width = 6, plotlyOutput("boxPlot", height = "300px"), title = "Salary by Race"),
      box(width = 6, plotlyOutput("genderPie", height = "300px"), title = "Gender Distribution")
    ),
    fluidRow(
      box(width = 12, DTOutput("dataTable"), title = "Filtered Data Table")
    )
  )
)

# Server
server <- function(input, output) {
  
  filteredData <- reactive({
    df <- data
    if (input$country != "All") df <- df[df$Country == input$country, ]
    if (input$race != "All") df <- df[df$Race == input$race, ]
    if (input$gender != "All") df <- df[df$Gender == input$gender, ]
    if (input$education != "All") df <- df[df$Education.Level == input$education, ]
    df <- df[df$Age >= input$ageRange[1] & df$Age <= input$ageRange[2], ]
    df <- df[df$Years.of.Experience >= input$expRange[1] & df$Years.of.Experience <= input$expRange[2], ]
    df
  })
  
  output$avgSalaryBox <- renderValueBox({
    avg <- mean(filteredData()$Salary, na.rm = TRUE)
    valueBox(paste0("$", round(avg, 2)), "Average Salary", icon = icon("dollar-sign"), color = "green")
  })
  
  output$expBox <- renderValueBox({
    avgExp <- mean(filteredData()$Years.of.Experience, na.rm = TRUE)
    valueBox(round(avgExp, 1), "Avg. Experience (Years)", icon = icon("briefcase"), color = "purple")
  })
  
  output$countBox <- renderValueBox({
    count <- nrow(filteredData())
    valueBox(count, "Total Records", icon = icon("users"), color = "blue")
  })
  
  output$salaryPlot <- renderPlotly({
    df <- filteredData()
    df$JobCategory <- case_when(
      str_detect(df$Job.Title, regex("data\\s*scientist", ignore_case = TRUE)) ~ "Data Scientist",
      str_detect(df$Job.Title, regex("data\\s*analyst", ignore_case = TRUE)) ~ "Data Analyst",
      str_detect(df$Job.Title, regex("software|developer|programmer", ignore_case = TRUE)) ~ "Software Developer",
      str_detect(df$Job.Title, regex("manager", ignore_case = TRUE)) ~ "Manager",
      str_detect(df$Job.Title, regex("engineer", ignore_case = TRUE)) ~ "Engineer",
      str_detect(df$Job.Title, regex("analyst", ignore_case = TRUE)) ~ "Analyst",
      str_detect(df$Job.Title, regex("support|technician|it", ignore_case = TRUE)) ~ "IT Support",
      TRUE ~ "Other"
    )
    
    plotData <- df %>% 
      group_by(JobCategory) %>% 
      summarise(Average_Salary = mean(Salary, na.rm = TRUE)) %>% 
      arrange(desc(Average_Salary))
    
    p <- ggplot(plotData, aes(x = reorder(JobCategory, Average_Salary), y = Average_Salary)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(x = "Job Category", y = "Average Salary")
    
    ggplotly(p, height = 300)
  })
  
  output$educationPlot <- renderPlotly({
    df <- filteredData() %>%
      filter(!is.na(Education.Level), Education.Level != "") %>%
      mutate(Education.Level = str_replace_all(Education.Level, c(
        "Bachelor's Degree" = "Bachelor",
        "Master's Degree" = "Master",
        "Doctorate Degree" = "PhD",
        "High School Diploma" = "High School",
        "Some College" = "College"
      ))) %>%
      group_by(Education.Level) %>%
      summarise(Average_Salary = mean(Salary, na.rm = TRUE)) %>%
      arrange(desc(Average_Salary))
    
    df$Education.Level <- factor(df$Education.Level, levels = df$Education.Level)
    
    p <- ggplot(df, aes(x = Education.Level, y = Average_Salary)) +
      geom_col(fill = "#FFA07A") +
      labs(x = "Education Level", y = "Average Salary") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, height = 300)
  })
  
  output$boxPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Race, y = Salary, fill = Race)) +
      geom_boxplot() +
      labs(x = "Race", y = "Salary") +
      theme_minimal()
    ggplotly(p, height = 300)
  })
  
  output$genderPie <- renderPlotly({
    pieData <- filteredData() %>%
      group_by(Gender) %>%
      summarise(Count = n())
    
    plot_ly(pieData, labels = ~Gender, values = ~Count, type = "pie") %>%
      layout(title = "Gender Distribution")
  })
  
  output$dataTable <- renderDT({
    datatable(filteredData(), options = list(pageLength = 10))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_salary_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
}

# Run the App
shinyApp(ui = ui, server = server)
