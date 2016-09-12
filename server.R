library(ggplot2)
library(shiny)

# make skill_matching function
skill_match <- function(person_skills, job_skills = list()){
  if (length(job_skills) == 0){
    return (.2)
  }
  per_skill_have = length(intersect(person_skills, job_skills))/length(job_skills)
  return (per_skill_have)
}

function(input, output, session) {
  updateSelectizeInput(session, 'profile_skills', server = TRUE, choices = popular_job_skills$skills_fixed)
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- jobs
    popular_job_skills <- popular_job_skills
    if (input$duration != "All") {
      data <- data[data$duration == input$duration,]
    }
    # get skill match
    data <- data %>%
      mutate(skill_match = skill_match(input$profiles_skills, skills_fixed))
    # delete first column
    data <- data %>% select(-1)
    # only want job descriptions of certain length 
    data <- data %>%
      mutate(snippet_length = nchar(snippet)) %>%
      filter(snippet_length > 100)
    if (input$job_type != "All") {
      data <- data[data$job_type == input$job_type,]
    }
    if (input$workload != "All") {
      data <- data[data$workload == input$workload,]
    }
    if (input$past_hires >= 0) {
      data <- data[data$past_hires >= input$past_hires,]
    }
    if (input$feedback >= 0) {
      data <- data[data$feedback >= input$feedback,]
    }
    if (input$payment_verification_status != "All") {
      data <- data[data$payment_verification_status == input$payment_verification_status,]
    }
    data
  }))
  
}
