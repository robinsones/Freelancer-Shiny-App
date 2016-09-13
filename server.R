library(ggplot2)
library(shiny)
library(tidyverse)
# make skill_matching function
skill_match <- function(person_skills, job_skills = list()){
  if (length(job_skills) == 0){
    return (.2)
  }
  per_skill_have = length(intersect(person_skills, job_skills))/length(job_skills)
  return (per_skill_have)
}

# make function for getting cosine similarities 
cosine_similarities <- function(profile_text, jobs){
  # add profile text to job words
  job_words <- jobs %>%
    select(snippet, id) %>%
    add_row(snippet = profile_text, id = "profile_id", .before = 1) %>% 
    unnest_tokens(word, snippet) %>% count(id, word, sort = TRUE) %>% ungroup()
  # select words appearing less than 6 times and no stop words
  rare_words <- job_words %>% count(word) %>% filter(nn < 6)
  total_words <- job_words %>%  group_by(id) %>%summarize(total = sum(n))
  job_words <- left_join(job_words, total_words)
  data("stop_words")
  job_words <- job_words %>% anti_join(stop_words) %>% anti_join(rare_words)
  # get tfidf vectors
  tfidf_vector <- job_words %>% bind_tf_idf(word, id, n) %>% 
    select(word, id, tf_idf) %>% spread(word, tf_idf)
  # extract profile_tfidf
  profile_tfidf = tfidf_vector[1, -1]
  profile_tfidf <- as.numeric(profile_tfidf)
  profile_tfidf[is.na(profile_tfidf)] <- 0
  similarity_df <- frame_data(
    ~id, ~similarity, 
    "", 0)
  for (i in 2:nrow(tfidf_vector)){
    # make into vectors and get rid of NAs
    vectorized = as.numeric(tfidf_vector[i, -1])
    vectorized[is.na(vectorized)] <- 0
    similarity_score <- lsa::cosine(vectorized, profile_tfidf)
    similarity_df <- similarity_df %>%
      add_row(id = tfidf_vector[[i, 1]], similarity = similarity_score[[1, 1]])
  }
  return(similarity_df)}

function(input, output, session) {
  values <- reactiveValues()
  values$df <- data.frame()
  observe({
    # Filter data based on selections
    data <- jobs
    popular_job_skills <- popular_job_skills
    if (input$duration != "All") {
      data <- data[data$duration == input$duration,]
    }
    # round numbers
    data <- data %>%
      mutate(feedback = round(feedback, 2))
    # get skill match
    profile_skills <- input$profile_skills
    data <- data %>%
      mutate(skill_match = map_dbl(skills_fixed, ~skill_match(profile_skills, .)))
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
    data <- data %>% 
      select(title, snippet, workload, duration, country, feedback, payment_verification_status, 
             skills_fixed, skill_match) %>%
      mutate(snippet = paste0(str_sub(snippet, 1, 300), "..."))
    values$df <- data
  })
  output$table <- DT::renderDataTable(DT::datatable(values$df, colnames = c("Title", "Job Description Excerpt", "Workload", "Duration", 
                                                                            "Country", "Average Feedback", "Payment Verified", 
                                                                            "Skills", "Skill Match")))
}

