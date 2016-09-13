

library(tidyverse)
library(lsa)
library(Matrix)

# jobs_url <- read_csv("~/Dropbox/Final_Project_Shiny_App/jobs_url.csv")

job_skill_count <- jobs %>%
  select(skills_fixed, id)

trim_leading <- function (x)  sub("^\\s+", "", x)

job_skill_count <- job_skill_count %>% 
  unnest(skills_fixed) %>% 
  mutate(skills_fixed = trim_leading(skills_fixed)) %>%
  count(skills_fixed) %>%
  filter(n >= 10)

write.csv(job_skill_count, "popular_job_skills.csv")
popular_job_skills <- job_skill_count

unique(unlist(jobs$skills_fixed))

# remove quotation marks and brackets from skills column 
kk <- gsub("\\[|\\]", "", jobs$skills[1])
kk <- gsub("'", "", kk)


library(tm)
library(tidytext)
library(janeaustenr)

job_words <- jobs %>%
  select(snippet, id)

job_words <- job_words %>%
  unnest_tokens(word, snippet) %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

# select words appearing less than 6 times
rare_words <- job_words %>%
  count(word) %>%
  filter(nn < 6)

word_count

total_words <- job_words %>% group_by(id) %>% summarize(total = sum(n))
job_words <- left_join(job_words, total_words)
job_words

data("stop_words")
job_words <- job_words %>%
  anti_join(stop_words) %>%
  anti_join(rare_words)

job_words <- job_words %>%
  bind_tf_idf(word, id, n)

# now job words has tf_idf, word, and id. We need to spread

job_words

tfidf_vector <- job_words %>%
  select(word, id, tf_idf) %>%
  spread(word, tf_idf)

tfidf_vector[is.na(tfidf_vector)] <- 0

cos.sim <- function(A, B) 
{
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   

# make into vectors and get rid of NAs
tfidfv_ex = as.numeric(tfidf_vector[2,])
tfidfv_ex1 = as.numeric(tfidf_vector[20,])

tfidfv_ex1[is.na(tfidfv_ex1)] <- 0
tfidfv_ex[is.na(tfidfv_ex)] <- 0

lsa::cosine(tfidfv_ex, tfidfv_ex1)


corpus <- tm_map(jobs$snippet, removeWords, stopwords("english"))

corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument, language="english")
#creating term matrix with TF-IDF weighting
terms <-DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))


as.list(strsplit(jobs$skills[3], ",")[[1]])
        
jobs$skills[1]

"[u'excel-vba', u'javascript', u'microsoft-excel']"

jobs %>%
  mutate(skills_fixed = as.list(strsplit(skills, ",")[[1]]))

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

profile_text_ex <- "I rarely take on jobs, but when I do I go all in!  Greetings,  I am a full-stack SEO, data-driven marketer and web developer with a Swiss degree in Marketing and Business Management, currently writing a thesis on Search Engines.   Past and present work includes SEO & SEM for Silicon Valley startups and videostreaming services with millions of *daily* visitors.  Why is my SEO successful? I am an actual developer, which separates me from SEO theorists who had never opened an IDE; and which makes me able to talk to programmers effectively.   Search engine optimisation:  ✓ On-site optimisation ✓ Off-site optimisation ✓ Link building ✓ Content outlining  Conversion optimisation:  ✓ Standards and business algorithms ✓ Goals ✓ Conversions and funnels ✓ Strategy ✓ Analytics ✓ Growth-hacking ✓ Project management ✓ Brand and reputation management  Team management:  ✓ Management of distributed teams with up to 20 people  Web development:  ✓ Ruby 1.9+ ✓ Ruby on Rails 4+ ✓ JavaScript and CoffeeScript ✓ HTML, CSS, HAML, SLIM, SASS/SCSS ✓ APIs ✓ Test-driven development (Rspec, Capybara)  It is high time for a change."

sd <- cosine_similarities(profile_text_ex, jobs)

jobs %>% left_join(sd) %>%
  select(job_type, similarity)

jobs <- read_csv("~/Dropbox/Final_Project_Shiny_App/jobs.csv")


#### 


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
    select(word, id, tf_idf) %>%
    group_by(id) %>%
    mutate(tf_idf_norm = tf_idf / sqrt(sum(tf_idf ^ 2))) %>%
    ungroup()

# we need to keep mapping word, idf
word_idf <- tfidf %>%
    distinct(word, idf)
  
tfidf_profile <- data_frame(profile = profile_text_ex) %>%
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

# dot product of two vectors after they've been normalized


tf_idf_matrix <- tfidf %>%
  cast_sparse(id, word, tf_idf_norm)

tfidf_profile_vec <- tfidf_profile$tf_idf_norm[match(colnames(tf_idf_matrix), tfidf_profile$word)]
tfidf_profile_vec[is.na(tfidf_profile_vec)] <- 0
as.numeric(t(tfidf_profile_vec) %*% t(tf_idf_matrix))

# now have two matrices
# change to be in same order and position
tf_idf_matrix %*% t(tf_idf_matrix) # now have a document to document matrix of similarities 


### old cosine function
# make function for getting cosine similarities 
cosine_similarities <- function(profile_text, jobs){
  # add profile text to job words
  job_words <- jobs %>%
    select(snippet, id) %>%
    add_row(snippet = profile_text, id = "profile_id", .before = 1) %>% 
    unnest_tokens(word, snippet) %>% count(id, word, sort = TRUE) %>% ungroup()
  # select words appearing less than 6 times and no stop words
  rare_words <- job_words %>% count(word) %>% filter(nn < 6)
  total_words <- job_words %>%  group_by(id) %>% summarize(total = sum(n))
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

calculate_tfidf <- function(jobs){
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
    left_join(job_words, total_words) %>%
    anti_join(stop_words) %>% 
    anti_join(rare_words)
  
  # get tfidf vectors
  tfidf_vector <- job_words %>% 
    bind_tf_idf(word, id, n) %>% 
    select(word, id, tf_idf) %>% 
    spread(word, tf_idf)
  return(tfidf_vector)
}




