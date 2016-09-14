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

