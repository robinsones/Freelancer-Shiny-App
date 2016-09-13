library(tidyverse)

# load and slightly clean data
ds_job_titles <- read_csv("~/Dropbox/Final_Project_Shiny_App/ds_job_titles.csv") 
highest_degree <- read_csv("~/Dropbox/Final_Project_Shiny_App/highest_degree.csv")
skills <- read_csv("~/Dropbox/Final_Project_Shiny_App/skills.csv")
skill_correlations <- read_csv("~/Dropbox/Final_Project_Shiny_App/skill_correlations.csv")
popular_job_skills <- read_csv("~/Dropbox/Final_Project_Shiny_App/popular_job_skills.csv")

skills <- skills %>% select(ciphertext, skill_name)
highest_degree <- highest_degree %>% select(ciphertext, highest_degree = degrees_ranked)
ds_job_titles <- ds_job_titles %>% select(ciphertext, title = as_opening_title)
popular_job_skills <- popular_job_skills %>% select(skill_name = skills_fixed)
pop_skills <- skills %>%
  inner_join(popular_job_skills)

# get job history that we want
ds_job_history <- ds_job_titles %>%
  left_join(pop_skills) %>%
  left_join(highest_degree) %>%
  filter(highest_degree != "other" & is.na(highest_degree) == FALSE)

# let's say if over .15 they're correlated
# let's look for jobs where our education level matches, 
# 

# if a job has a skill, has the ducation level keep it, 

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

### filter down job history 
filtered_jobs <- ds_job_history %>%
  filter(highest_degree == "Bachelors") %>%
  # at least one skill is in that job
  filter(skill_name %in% c("java", "python", "r", "data-entry", "machine-learning"))

# have at 20% of the skills
"filtered_jobs <- filtered_jobs %>%
  count(id) %>%
  mutate(percent_have = job_total) %>%
  filter(percent_have >= .2)
"  

tfidf_titles_filter <- tfidf_titles %>%
  filter(id %in% filtered_jobs$id)

# get mean tf_idf for these jobs 

mean_tfidf <- tfidf_titles_filter %>%
  group_by(word) %>%
  summarize(mean = mean(tf_idf_norm))

#### get tfidf representation of current jobs

jobs_and_title <- jobs %>%
  select(title, id)

current_jobs_tfidf <- jobs_and_title %>%
  unnest_tokens(word, title) %>%
  group_by(id) %>%
  count(word) %>%
  inner_join(word_title_idf) %>%
  mutate(tf = n/sum(n)) %>%
  mutate(tf_idf = tf * idf) %>%
  mutate(tf_idf_norm = tf_idf / sqrt(sum(tf_idf ^ 2))) %>%
  ungroup()

#### Finally, combine tfidf of current jobs with old one

title_cosine_similarity <- current_jobs_tfidf %>%
  select(id, word, document_tfidf = tf_idf_norm) %>%
  inner_join(mean_tfidf, by = "word") %>%
  select(id, word, old_jobs_tfidf = mean, document_tfidf) %>%
  group_by(id) %>%
  summarize(cosine_similarity = sum(old_jobs_tfidf * document_tfidf))



### get tf_idf of every job

  ds_job_history %>% group_by(title) %>% summarize(skill_name)




# have 711 PhDs
ds_job_history %>%
  filter(highest_degree == "PhD") %>%
  distinct(ciphertext)

# have 4,862 Masters
ds_job_history %>%
  filter(highest_degree == "Masters") %>%
  distinct(ciphertext)

# get level of highest_degree in right order for ui 
unique(ds_job_history$highest_degree)
ds_job_history$highest_degree <- as.factor(ds_job_history$highest_degree)
sizes <- factor(sizes, levels = c("small", "medium", "large"))
levels(ds_job_history$highest_degree)
ds_job_history$highest_degree <- factor(ds_job_history$highest_degree, 
                    levels = c("High School", "Associates", "Bachelors", "Masters", "MBA", "JD", "MD", "PhD"))

my_skills <- c("java", "c++", "python", "r")
  
ds_job_history %>%
  distinct(skill_name, id) %>%
  filter(skill_name %in% my_skills) %>%
  count(id) %>%
  inner_join(job_counts)

# how many of the job skills do I have, these are 14,000 jobs that overlap


### extra
jobs %>% 
  mutate(feedback = ifelse(reviews_count == 0, NA, feedback)) %>% 
  arrange(reviews_count) %>% select(reviews_count, feedback)

# if review count zero change feedback to NA
mutate(feedback = ifelse(reviews_count == 0, NA, feedback)) 
