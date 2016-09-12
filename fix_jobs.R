library(dplyr)

library(readr)
jobs <- read_csv("~/Dropbox/Final_Project_Shiny_App/jobs.csv")

jobs <- jobs %>%
  mutate(skills_fixed = (gsub("\\[|\\]|'", "", skills))) %>%
  mutate(skills_fixed = as.list(strsplit(skills_fixed, ",")))

job_skill_count <- jobs %>%
  select(skills_fixed, id)

trim_leading <- function (x)  sub("^\\s+", "", x)

job_skill_count <- job_skill_count %>% 
  unnest(skills_fixed) %>% 
  mutate(skills_fixed = trim_leading(skills_fixed)) %>%
  count(skills_fixed) %>%
  filter(n >= 10)

write.csv(job_skill_count, "popular_job_skills.csv")
popular_job_skills.csv <- job_skill_count

unique(unlist(jobs$skills_fixed))

# remove quotation marks and brackets from skills column 
kk <- gsub("\\[|\\]", "", jobs$skills[1])
kk <- gsub("'", "", kk)

as.list(strsplit(jobs$skills[3], ",")[[1]])
        
jobs$skills[1]

"[u'excel-vba', u'javascript', u'microsoft-excel']"

jobs %>%
  mutate(skills_fixed = as.list(strsplit(skills, ",")[[1]]))