# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(shiny)

fluidPage(
  titlePanel("Basic DataTable"),
  h4("Enter Your Profile Text Here"),
  tags$style(type="text/css", "textarea {width:100%}"),
  tags$textarea(id = 'input_text', placeholder = 'Type here', rows = 8, ""),
  verbatimTextOutput("output_profile_text"),
  textInput("caption", "Enter Your Skills Here as a List", "c(C++, Java)"),
  verbatimTextOutput("skill_list"),
  # Create a new Row in the UI for selectInputs
  selectizeInput("profile_skills", "Your skills", popular_job_skills$skills_fixed, selected = NULL, multiple = TRUE,
                                        options = NULL),
  fluidRow(
    column(4,
           selectInput("duration",
                       "duration:",
                       c("All",
                         unique(as.character(jobs$duration))))
    ),
    column(4,
           selectInput("job_type",
                       "job_type:",
                       c("All",
                         unique(as.character(jobs$job_type))))
    ),
    column(4, 
           numericInput('feedback', 'Minimum Feedback Score', 0,
                 min = 0, max = 5, step = .1)
    ),
    column(4, 
           numericInput('past_hires', 'Minimum Number of Past hires', 0,
                        min = 0, max = 500, step = 1)
    ),
    column(4,
           selectInput("payment_verification_status",
                       "payment_verification_status:",
                       c("All",
                         "VERIFIED"))
    ),
    column(4,
           selectInput("workload",
                       "workload:",
                       c("All",
                         unique(as.character(jobs$workload))))
    )
  ),
  # Create a new row for the table.
  fluidRow(
    DT::dataTableOutput("table")
  )
)
