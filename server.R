library(ggplot2)
library(shiny)
library(tidyverse)
library(readr)
library(stringr)
library(DT)

jobs_url <- jobs_url %>% 
  select(id, url) 

#### tfidf on jobs

all_titles <- ds_job_history %>%
  distinct(title)

# add id
all_titles <- all_titles %>%
  mutate(id = rownames(all_titles))

# add id back to ds_job_history
ds_job_history<- ds_job_history %>% 
  left_join(all_titles)

title_words <- all_titles %>% 
  unnest_tokens(word, title) %>% 
  count(id, word, sort = TRUE) %>% 
  ungroup()

# select words appearing less than 6 times and no stop words
rare_title_words <- title_words %>% 
  count(word) %>% 
  filter(nn < 100)

total_title_words <- title_words %>%  
  group_by(id) %>% 
  summarize(total = sum(n))

title_words <- title_words %>% 
  left_join(total_title_words) %>%
  anti_join(stop_words) %>% 
  anti_join(rare_title_words)

# get tfidf vectors
tfidf_titles <- title_words %>% 
  bind_tf_idf(word, id, n) %>% 
  group_by(id) %>%
  mutate(tf_idf_norm = tf_idf / sqrt(sum(tf_idf ^ 2))) %>%
  ungroup()

# we need to keep mapping word, idf
word_title_idf <- tfidf_titles %>%
  distinct(word, idf)

#### Now that we have skills, pick the job history and education

job_counts <- ds_job_history %>%
  distinct(skill_name, id) %>%
  group_by(id) %>%
  summarize(job_total = n())

ds_job_history <- ds_job_history %>%
  left_join(job_counts)

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

trim_leading <- function (x)  sub("^\\s+", "", x)

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
    mutate(skill_match = 0) %>%
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
    if (input$job_type != "All") {
      data <- data %>%
       filter(job_type == input$job_type)
    }
    if (input$duration != "All") {
      data <- data %>% 
        filter(duration == input$duration)
    }
    if (input$workload != "All") {
      data <- data %>%
        filter(workload == input$workload)
    }
    if (input$past_hires >= 0) {
      data <- data %>%
        filter(past_hires >= input$past_hires)
    }
    if (input$feedback >= 0) {
      data <- data %>%
        filter(feedback >= input$feedback)
    }
    if (input$payment_verification_status != "All") {
      data <- data %>%
        filter(payment_verification_status == input$payment_verification_status) 
    }
    data <- data %>% 
      select(title, snippet, overall_match, url, workload, duration, country, feedback, payment_verification_status, 
             skill_match, skills_fixed, id)
    values$df <- data
  })
  observeEvent(input$skillButton, {
    final_data <- values$df
    
    final_data <- final_data %>%
      mutate(skills_for_match = lapply(skills_fixed, trim_leading)) %>%
      mutate(skill_match = map_dbl(skills_for_match, ~skill_match(input$profile_skills, .))) %>%
      select(title, snippet, overall_match, url, workload, duration, country, feedback, payment_verification_status, 
             skill_match, skills_fixed, id)
    values$df <- final_data
  })
  observeEvent(input$profButton, {
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
      select(title, snippet, overall_match, url, workload, duration, country, feedback, payment_verification_status, 
             skill_match, skills_fixed, id) %>%
      arrange(desc(overall_match))
    values$df <- final_data
  })
  output$table <- DT::renderDataTable(DT::datatable(values$df, colnames = c("Order", "Title", "Job Description Excerpt",
                                                                            "Overall Match", "Url","Workload", "Duration", 
                                                                            "Country", "Average Feedback", "Payment Verified", 
                                                                            "Skill Match","Skills", "ID")))
}

