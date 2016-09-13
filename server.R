library(ggplot2)
library(shiny)
library(tidyverse)
library(readr)

jobs_url <- jobs_url %>% 
  select(id, url) 

### Make tfidf matrix

job_words <- jobs %>% 
  select(snippet, id) %>%
  unnest_tokens(word, snippet) %>% 
  count(id, word, sort = TRUE) %>% 
  ungroup()

# select words appearing less than 6 times and no stop words
rare_words <- job_words %>% 
  count(word) %>% 
  filter(nn < 6)

total_words <- job_words %>%  
  group_by(id) %>% 
  summarize(total = sum(n))

job_words <- job_words %>% 
  left_join(total_words) %>%
  anti_join(stop_words) %>% 
  anti_join(rare_words)

# get tfidf vectors
tfidf <- job_words %>% 
  bind_tf_idf(word, id, n) %>% 
  group_by(id) %>%
  mutate(tf_idf_norm = tf_idf / sqrt(sum(tf_idf ^ 2))) %>%
  ungroup()

word_idf <- tfidf %>%
  distinct(word, idf)

### Make skill_matching function
skill_match <- function(person_skills, job_skills = list()){
  if (length(job_skills) == 0){
    return (.2)
  }
  per_skill_have = length(intersect(person_skills, job_skills))/length(job_skills)
  return (per_skill_have)
}

# delete first column, round feedback, only look at jobs with more than 100 characters
pre_processing_data <- function(jobs_data){
  jobs_data_final <- jobs_data %>%
    mutate(skills_fixed = (gsub("\\[|\\]|'", "", skills))) %>%
    mutate(skills_fixed = as.list(strsplit(skills_fixed, ","))) %>%
    select(-1) %>%
    mutate(feedback = round(feedback, 2)) %>%
    mutate(snippet_length = nchar(snippet)) %>%
    filter(snippet_length > 100) %>% 
    mutate(overall_match = 0) %>%
    mutate(snippet = paste0(str_sub(snippet, 1, 300), "...")) %>%
    left_join(jobs_url)
  return(jobs_data_final)
}

# make a function to normalize a column 
normalit<-function(m){
  (m - min(m))/(max(m)-min(m))
}

function(input, output, session) {
  values <- reactiveValues()
  values$df <- data.frame()
  observe({
    # Filter data based on selectionss
    data <- jobs
    data <- pre_processing_data(data)
    # get skill match
    profile_skills <- input$profile_skills
    
    data <- data %>%
      mutate(skill_match = map_dbl(skills_fixed, ~skill_match(profile_skills, .)))
    
    if (input$job_type != "All") {
      data <- data[data$job_type == input$job_type,]
    }
    if (input$duration != "All") {
      data <- data[data$duration == input$duration,]
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
      select(title, snippet, overall_match, url, workload, duration, country, feedback, payment_verification_status, 
             skills_fixed, skill_match, id)
    values$df <- data
  })
  observeEvent(input$goButton, {
    # get similarity data and join
    final_data <- values$df
    
    updateTextInput(session, "profile_text", value = input$profile_text)
    
    # get tfidf of entered profile text
    tfidf_profile <- data_frame(profile = input$profile_text) %>%
      unnest_tokens(word, profile) %>%
      count(word) %>%
      inner_join(word_idf) %>%
      mutate(tf = n/sum(n)) %>%
      mutate(tf_idf = tf * idf) %>%
      mutate(tf_idf_norm = tf_idf / sqrt(sum(tf_idf ^ 2)))
    
    # left join with table, replace_na with zero 
    document_cosine_similarity <- tfidf %>%
      select(id, word, document_tfidf = tf_idf_norm) %>%
      inner_join(tfidf_profile, by = "word") %>%
      select(id, word, profile_tfidf = tf_idf_norm, document_tfidf) %>%
      group_by(id) %>%
      summarize(cosine_similarity = sum(profile_tfidf * document_tfidf))

    # normalize similarity and skill_match
    final_data <- final_data %>%
      left_join(document_cosine_similarity) %>%
      mutate(cosine_similarity = ifelse(is.na(cosine_similarity), 0, cosine_similarity)) %>%
      mutate(normalized_skill_match = normalit(skill_match)) %>%
      mutate(normalized_similarity = normalit(cosine_similarity)) %>%
      mutate(overall_match = normalized_similarity + .5*normalized_skill_match)
    
    final_data <- final_data %>% 
      select(title, snippet, url, overall_match, workload, duration, country, feedback, payment_verification_status, 
             skills_fixed, skill_match, id) %>%
      arrange(desc(overall_match))
    
    values$df <- final_data
  })
  output$table <- DT::renderDataTable(DT::datatable(values$df, colnames = c("Order", "Title", "Job Description Excerpt",
                                                                            "Overall Match", ""Url", Workload", "Duration", 
                                                                            "Country", "Average Feedback", "Payment Verified", 
                                                                            "Skills", "Skill Match", "ID")))
}

