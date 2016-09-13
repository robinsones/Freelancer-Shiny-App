# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(shiny)
library(shinydashboard)
dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    width = 400,
    fluidPage(
      h4("Enter Your Profile Text Here"),
      tags$style(type="text/css", "textarea {width:100%}"),
      tags$textarea(id = 'profile_text', placeholder = 'Type here', rows = 8, ""),
      # Create a new Row in the UI for selectInputs
      selectInput("profile_skills", "Your skills", popular_job_skills$skills_fixed, selected = NULL, multiple = TRUE),
      fluidRow(
        column(6,
               selectInput("duration",
                           "duration:",
                           c("All",
                             unique(as.character(jobs$duration))))
        ),
        column(6,
               selectInput("job_type",
                           "job_type:",
                           c("All",
                             unique(as.character(jobs$job_type))))
        ),
        column(6, 
               numericInput('feedback', 'Minimum Feedback Score', 0,
                            min = 0, max = 5, step = .1)
        ),
        column(6, 
               numericInput('past_hires', 'Minimum Number of Past hires', 0,
                            min = 0, max = 500, step = 1)
        ),
        column(6,
               selectInput("payment_verification_status",
                           "payment_verification_status:",
                           c("All",
                             "VERIFIED"))
        ),
        column(6,
               selectInput("workload",
                           "workload:",
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




