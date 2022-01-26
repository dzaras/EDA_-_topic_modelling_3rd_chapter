# 3rd Chapter - film reviews text analysis
# Dimitri Zaras - 04/08/2021

# load packages
require(tidyverse); require(stm)
p <- c("RCurl", "gdata", "zoo", "tidyverse", "tidytext", "stm", "quanteda", "plm", "broom", "RColorBrewer", "stargazer", "psych", "rvest", "GGally", "gridExtra", "ggpubr", "tesseract", "readxl", "readtext")
lapply(p, require, character.only = TRUE)
rm(p)
# -----------------------------------
setwd("C:/Users/dima/OneDrive - Emory University/3rd_chapter")

# import dataset that contains all of the film reviews from newspapers and online sources
data <- read.csv("~/OneDrive - Emory University/3rd_chapter/imdb_data_realSent_text1.csv")

# remove the words 'film' and 'movie' because those words are too common
data$text <- gsub("film", "", data$text) # remove 'film' bc this word is very common
data$text <- gsub("films", "", data$text) # remove 'films' bc this word is very common
data$text <- gsub("movie", "", data$text) # remove 'movie' bc this word is very common
data$text <- gsub("movies", "", data$text) # remove 'movies' bc this word is very common


processed <- textProcessor(data$text, metadata= data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <-  out$documents
vocab <- out$vocab
meta <- out$meta

# stm

# stm using k = 0 algorithm to let computer ID number of topics
topic_model <- stm(out$documents, out$vocab, K = 0, 
                   prevalence = ~ pub.type2,
                   data = out$meta, 
                   verbose = TRUE, 
                   seed = 2000, 
                   init.type = "Spectral")

# returned k = 83
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
                   prevalence = ~ pub.type2,
                   data = out$meta, 
                   verbose = TRUE, 
                   seed = 2000, 
                   init.type = "Spectral")

save(topic_model, file = "C:/Users/dima/OneDrive - Emory University/3rd_chapter/analysis/20210408/text1_80.Rdata")

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
write_csv(td_beta2, path = "C:/Users/dima/OneDrive - Emory University/3rd_chapter/analysis/20210408/beta_80.csv")


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

# Regression estimates of topic prevalence
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
save(together, file = "C:/Users/dima/OneDrive - Emory University/3rd_chapter/analysis/20210408/together_text1_80.RData") # 3,744 obs. X 157 vars

# prepare for plotting
together <- gather(together, topic, value, topic_1:topic_80)
together$publication_year <- as.Date(together$year, format = "%Y")
#together$month <- str_sub(together$publication_date, 6, 7)

# create smaller lists for ease of plot-readability
first <- c("topic_1", "topic_2", "topic_3", "topic_4", "topic_5", "topic_6", "topic_7", "topic_8", "topic_9", "topic_10")

second <- c("topic_11", "topic_12", "topic_13", "topic_14", "topic_15", "topic_16", "topic_17", "topic_18", "topic_19", "topic_20")

third <- c("topic_21", "topic_22", "topic_23", "topic_24", "topic_25", "topic_26", "topic_27", "topic_28", "topic_29", "topic_30")

fourth <- c("topic_31", "topic_32", "topic_33", "topic_34", "topic_35", "topic_36", "topic_37", "topic_38", "topic_39", "topic_40")
            
fifth <- c( "topic_41", "topic_42", "topic_43", "topic_44", "topic_45", "topic_46", "topic_47", "topic_48", "topic_49", "topic_50")

sixth <- c( "topic_51", "topic_52", "topic_53", "topic_54", "topic_55", "topic_56", "topic_57", "topic_58", "topic_59", "topic_60")

seventh <- c("topic_61", "topic_62", "topic_63", "topic_64", "topic_65", "topic_66", "topic_67", "topic_68", "topic_69", "topic_70" )

eighth <- c( "topic_71", "topic_72", "topic_73", "topic_74", "topic_75", "topic_76", "topic_77", "topic_78", "topic_79", "topic_80" )


# plot three 
a <- together %>% 
  #filter(month != "NA") %>% 
  filter(topic %in% first) %>% 
  group_by(publication_year, topic) %>% 
  summarize(avg = mean(value)) %>%
  ungroup() %>% 
  ggplot(aes(publication_year, avg)) +
  geom_line(aes(group = topic, color = topic)) + 
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.20))

b <- together %>% 
  #filter(month != "NA") %>% 
  filter(topic %in% second) %>% 
  group_by(publication_year, topic) %>% 
  summarize(avg = mean(value)) %>%
  ungroup() %>% 
  ggplot(aes(publication_year, avg)) +
  geom_line(aes(group = topic, color = topic)) + 
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.20))

c <- together %>% 
  #filter(month != "NA") %>% 
  filter(topic %in% third) %>% 
  group_by(publication_year, topic) %>% 
  summarize(avg = mean(value)) %>%
  ungroup() %>% 
  ggplot(aes(publication_year, avg)) +
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







