# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(shiny)
library(shinydashboard)
dashboardPage(
  dashboardHeader(title = "Finding Your Best Data Science Freelancing Job", titleWidth = 450),
  dashboardSidebar(
    width = 450,
    fluidPage(
      textInput(inputId = "profile_text", label = "Your Profile Text"),
      selectInput("profile_skills", "Your skills", popular_job_skills$skill_name, selected = NULL, multiple = TRUE),
      tags$div(class = "form-group shiny-input-container", actionButton("skillProfButton", "Submit")),
      selectInput("highest_degree", "Your Highest Degree", levels(unique(ds_job_history$highest_degree)), selected = NULL),
      fluidRow(
        column(6,
               selectInput("duration",
                           "Duration:",
                           c("All",
                             "Less than 1 week", "Less than 1 month", "1 to 3 months", "3 to 6 months", 
                             "More than 6 months"))
        ),
        column(6,
               selectInput("job_type",
                           "Job Type:",
                           c("All",
                            unique(as.character(jobs$job_type))))
        ),
        column(6, 
               numericInput('feedback', 'Min. Feedback Score:', 0,
                            min = 0, max = 5, step = .1)
        ),
        column(6, 
               numericInput('past_hires', 'Min. Number of Past Hires:', 0,
                            min = 0, max = 500, step = 1)
        ),
        column(6,
               selectInput("payment_verification_status",
                           "Payment Verified:",
                           c("All",
                             "VERIFIED"))
        ),
        column(6,
               selectInput("workload",
                           "Workload:",
                           c("All",
                             unique(as.character(jobs$workload))))
        )
      ))
    
  ),
  dashboardBody(
    # Create a new row for the table.
    fluidRow(
      DT::dataTableOutput("table")
    )
  )
)




