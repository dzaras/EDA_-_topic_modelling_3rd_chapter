# 3rd Chapter - amateur film reviews text analysis
# Dimitri Zaras - 12/26/2021

# in this script I will replicate the process for extracting topics through stm that I followed for professional film reviews and apply
# it to amateur film reviews collected from imdb.com

# I will do that initially for all of them together, and then break it down by year - which I'll have to do for the pro reviews too
# In this script, I analyze only amateur film reviews that were posted in '98

# load packages
require(tidyverse); require(stm)
p <- c("RCurl", "gdata", "zoo", "tidyverse", "tidytext", "stm", "quanteda", "plm", "broom", "RColorBrewer", "stargazer", "psych", "rvest", "GGally", "gridExtra", "ggpubr", "tesseract", "readxl", "readtext")
lapply(p, require, character.only = TRUE)
rm(p)
# -----------------------------------
setwd("C:/Users/dima/OneDrive - Emory University/3rd_chapter")

# import dataset that contains all of the film reviews from newspapers and imdb that I created on 12/26/21
data <- read.csv("~/OneDrive - Emory University/3rd_chapter/third_ch_data/pro_amat_reviews.csv")

# remember to convert categorical variables from char in the original csv file to factor variables
data$pub <- as.factor(data$pub)
data$pub_type <- as.factor(data$pub_type)
data$review_year <- as.factor(data$review_year)
data$actual_review_year <- as.factor(data$actual_review_year)
data$doc_type <- as.factor(data$doc_type)
data$rating2 <- as.factor(data$rating2)

summary(data)

# remove the words 'film' and 'movie' because those words are too common
data$review_text <- gsub("film", "", data$review_text) # remove 'film' bc this word is very common
data$review_text <- gsub("films", "", data$review_text) # remove 'films' bc this word is very common
data$review_text <- gsub("movie", "", data$review_text) # remove 'movie' bc this word is very common
data$review_text <- gsub("movies", "", data$review_text) # remove 'movies' bc this word is very common


processed <- textProcessor(data$review_text, metadata= data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <-  out$documents
vocab <- out$vocab
meta <- out$meta

# stm

# stm using k = 0 algorithm to let computer ID number of topics
topic_model <- stm(out$documents, out$vocab, K = 0, 
                   #prevalence = ~ pub.type2,
                   data = out$meta, 
                   verbose = TRUE, 
                   seed = 2000, 
                   init.type = "Spectral")

# returned k = 112
#save(topic_model, file = "C:/Users/dima/OneDrive - Emory University/3rd_chapter/analysis/stm_text12.Rdata")

# using the searchK function to identify "good" number of topics. warning: time intensive (10+ min)
findingk <- searchK(out$documents, out$vocab, 
                    K = c(10, 15, 20, 25), 
                    prevalence = ~ pub.type2,
                    data = out$meta, 
                    verbose=TRUE,
                    seed = 2000,
                    init.type = "Spectral")

findK_results1 <- findingk$results
save(findK_results1, file = "C:/Users/dima/OneDrive - Emory University/3rd_chapter/analysis/20210210/findK_results1.RData")
findK_results1_plot <- plot(findingk)
# held-out likelihood is lowest at 25, sem. coherence lowest at 20, residuals lowest at 25

# find trade-off between exclusivity and semantic coherence on a graph
# the following chuck of code is from: https://juliasilge.com/blog/evaluating-stm/
findK_results1 %>%
  select(K, exclus, semcoh) %>%
  filter(K %in% c(10, 15, 20, 25)) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semcoh, exclus, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

# run higher number of topics
findingk <- searchK(out$documents, out$vocab, 
                    K = c(20, 55, 80, 90), 
                    prevalence = ~ pub.type2,
                    data = out$meta, 
                    verbose=TRUE,
                    seed = 2000,
                    init.type = "Spectral")

findK_results2 <- findingk$results
save(findK_results2, file = "C:/Users/dima/OneDrive - Emory University/3rd_chapter/analysis/20210309/findK_results2.RData")
findK_results2_plot <- plot(findingk)
# probably 25 topics seems like a good option?

findK_results2 %>%
  select(K, exclus, semcoh) %>%
  filter(K %in% c(20, 55, 80, 90)) %>%
  unnest(cols = c(K, exclus, semcoh)) %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semcoh, exclus, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

# stm final / working model k = 80
topic_model <- stm(out$documents, out$vocab, K = 80, 
                   #prevalence = ~ pub.type2,
                   data = out$meta, 
                   verbose = TRUE, 
                   seed = 2000, 
                   init.type = "Spectral")

save(topic_model, file = "C:/Users/dima/OneDrive - Emory University/3rd_chapter/analysis/20211217/amateur98_topicmodel_k80.Rdata")

# Save beta lists
td_beta <- tidy(topic_model, matrix = "beta") # extract betas
td_beta2 <- td_beta %>% 
  arrange(topic, desc(beta)) %>% 
  group_by(topic) %>% 
  mutate(rank = order(beta, decreasing = T)) %>% 
  ungroup() %>% 
  filter(rank < 31) %>% 
  mutate(topic = paste0("topic_", topic)) %>% 
  select(-beta) %>% 
  spread(topic, term)

head(td_beta2)
write_csv(td_beta2, file = "C:/Users/dima/OneDrive - Emory University/3rd_chapter/analysis/20211217/amateur98_beta_80.csv")


# extracting betas - a dataset of the top words for each topic
td_beta <- tidy(topic_model, matrix = "beta")
# plotting each topic as a bar-graph of the top 15 words
Rp1 <- td_beta %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")
Rp1

# Regression estimates of topic prevalence - this might not be relevant for amateur reviews (it was for pro reviews due to pub_type(?))
prep <- estimateEffect(1:80 ~ pub.type2, 
                       stmobj = topic_model,
                       meta = out$meta,
                       uncertainty = "Global")
summary(prep)
# most of the topics are stat. significant

# extracting gamma matrix
td_gamma <- tidy(topic_model, matrix = "gamma") %>% # extract gammas
  spread(topic, gamma)
names(td_gamma)
names(td_gamma)[2:81] <- paste0("topic_", names(td_gamma)[2:81])
names(td_gamma)
meta$document <- 1:nrow(meta)

# join with metadata
together <- inner_join(meta, td_gamma)
names(together)
save(together, file = "C:/Users/dima/OneDrive - Emory University/3rd_chapter/analysis/20211217/amateur98_together_text1_80.RData") 

# prepare for plotting
together <- gather(together, topic, value, topic_1:topic_80)
#together$publication_year <- as.Date(together$year, format = "%Y")
#together$month <- str_sub(together$publication_date, 6, 7)

# here i am trying the same with only 30 topics to check if it works differently because we are dealing with amateur reviews
# stm final / working model k = 30
topic_model <- stm(out$documents, out$vocab, K = 30, 
                   prevalence = ~ pub_type,
                   data = out$meta, 
                   verbose = TRUE, 
                   seed = 2000, 
                   init.type = "Spectral")

save(topic_model, file = "C:/Users/dima/OneDrive - Emory University/3rd_chapter/analysis/pro_and_amat_topicmodel_k30.Rdata")

# Save beta lists
td_beta <- tidy(topic_model, matrix = "beta") # extract betas
td_beta2 <- td_beta %>% 
  arrange(topic, desc(beta)) %>% 
  group_by(topic) %>% 
  mutate(rank = order(beta, decreasing = T)) %>% 
  ungroup() %>% 
  filter(rank < 31) %>% 
  mutate(topic = paste0("topic_", topic)) %>% 
  select(-beta) %>% 
  spread(topic, term)

head(td_beta2)
write_csv(td_beta2, file = "C:/Users/dima/OneDrive - Emory University/3rd_chapter/analysis/pro_and_amat_beta_30.csv")


# extracting betas - a dataset of the top words for each topic
td_beta <- tidy(topic_model, matrix = "beta")
# plotting each topic as a bar-graph of the top 15 words
Rp1 <- td_beta %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")
Rp1

# Regression estimates of topic prevalence - this might not be relevant for amateur reviews (it was for pro reviews due to pub_type(?))
prep <- estimateEffect(1:30 ~ pub_type + review_year, 
                       stmobj = topic_model,
                       meta = out$meta,
                       uncertainty = "Global")
summary(prep)
# most of the topics are stat. significant

# extracting gamma matrix
td_gamma <- tidy(topic_model, matrix = "gamma") %>% # extract gammas
  spread(topic, gamma)
names(td_gamma)
names(td_gamma)[2:31] <- paste0("topic_", names(td_gamma)[2:31])
names(td_gamma)
meta$document <- 1:nrow(meta)

# join with metadata
together <- inner_join(meta, td_gamma)
names(together)
save(together, file = "C:/Users/dima/OneDrive - Emory University/3rd_chapter/analysis/pro_and_amat_gamma_and_meta_k30.RData") 

# prepare for plotting
together <- gather(together, topic, value, topic_1:topic_30)
together$review_year <- as.Date(together$review_year, format = "%Y")
#together$month <- str_sub(together$publication_date, 6, 7)


# create smaller lists for ease of plot-readability - select appropriate number of lists based on number of k
first <- c("topic_1", "topic_2", "topic_3", "topic_4", "topic_5", "topic_6", "topic_7", "topic_8", "topic_9", "topic_10")

second <- c("topic_11", "topic_12", "topic_13", "topic_14", "topic_15", "topic_16", "topic_17", "topic_18", "topic_19", "topic_20")

third <- c("topic_21", "topic_22", "topic_23", "topic_24", "topic_25", "topic_26", "topic_27", "topic_28", "topic_29", "topic_30")

fourth <- c("topic_31", "topic_32", "topic_33", "topic_34", "topic_35", "topic_36", "topic_37", "topic_38", "topic_39", "topic_40")

fifth <- c( "topic_41", "topic_42", "topic_43", "topic_44", "topic_45", "topic_46", "topic_47", "topic_48", "topic_49", "topic_50")

sixth <- c( "topic_51", "topic_52", "topic_53", "topic_54", "topic_55", "topic_56", "topic_57", "topic_58", "topic_59", "topic_60")

seventh <- c("topic_61", "topic_62", "topic_63", "topic_64", "topic_65", "topic_66", "topic_67", "topic_68", "topic_69", "topic_70" )

eighth <- c( "topic_71", "topic_72", "topic_73", "topic_74", "topic_75", "topic_76", "topic_77", "topic_78", "topic_79", "topic_80" )


# plot three - I like the idea of grouping by rating the topic avg topic prevalence (?)
a <- together %>% 
  filter(pub_type != "imdb") %>% 
  filter(topic %in% first) %>% 
  group_by(review_year, topic) %>% 
  summarize(avg = mean(value)) %>%
  ungroup() %>% 
  ggplot(aes(review_year, avg)) +
  geom_line(aes(group = topic, color = topic)) + 
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.20))

b <- together %>% 
  #filter(month != "NA") %>% 
  filter(topic %in% second) %>% 
  group_by(rating, topic) %>% 
  summarize(avg = mean(value)) %>%
  ungroup() %>% 
  ggplot(aes(rating, avg)) +
  geom_line(aes(group = topic, color = topic)) + 
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.20))

c <- together %>% 
  #filter(month != "NA") %>% 
  filter(topic %in% third) %>% 
  group_by(rating, topic) %>% 
  summarize(avg = mean(value)) %>%
  ungroup() %>% 
  ggplot(aes(rating, avg)) +
  geom_line(aes(group = topic, color = topic)) + 
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.20))

# join together
grid.arrange(a, b, c, nrow = 1)
# (screenshot)










# ----------------------------------------------------------------
# extracting gamma - the proportion of each topic in each document
td_gamma <- tidy(topic_model, matrix = "gamma") %>%
  spread(topic, gamma) # the spread function is confusing but essentially reshaping data

# renaming variables
names(td_gamma)[2:21] <- paste0("topic_", names(td_gamma)[2:21])

# joining with metadata
meta$document <- 1:nrow(meta)
together <- inner_join(meta, td_gamma)
names(together)

# plotting topic proportions over time
together <- gather(together, topic, value, topic_1:topic_20) # reshaping
together$year <- as.Date(together$year, format = "%Y")
#together$month <- str_sub(together$publication_date, 6, 7) # identifying month as variable

# plotting
together %>% 
  group_by(year, topic) %>% 
  summarize(avg = mean(value)) %>%
  ungroup() %>% 
  ggplot(aes(year, avg)) +
  geom_line(aes(group = topic, color = topic)) + 
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.2))
#facet_wrap(~topic, scales = "free")

# stm using k = 0 algorithm to let computer ID number of topics
topic_model <- stm(out$documents, out$vocab, K = 0, 
                   #prevalence = ~ source,
                   data = out$meta, 
                   verbose = TRUE, 
                   seed = 2000, 
                   init.type = "Spectral")







