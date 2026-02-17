#PREPARE
library(tidyverse)
library(tidytext)
library(SnowballC)
library(topicmodels)
library(stm)
library(ldatuning)
library(knitr)
library(LDAvis)
raw <- read.csv("data/master.csv")
raw <- raw |>
  filter(modality == "forum essay" | modality == "zoom essay" | modality == "discord essay") |>
  select(c("modality",
           "sender",
           "message"))
#WRANGLE
#tokenize
tokens <- 
  unnest_tokens(raw, 
                output = word,
                input = message)
#tidy
tokens <-
  anti_join(tokens,
            stop_words,
            by = "word")
tokens_frequency <- count(tokens, word, sort = TRUE)
my_stopwords <- c("2", "42", "pp", "64758","31035")
tokens <-
  tokens |>
  filter(!word %in% my_stopwords)
#ANALYZE
#cast DTMs
DTM_forum <- tokens |>
  filter(modality == "forum essay") |>
  count(sender, word) |>
  cast_dtm(sender, word, n)
DTM_zoom <- tokens |>
  filter(modality == "zoom essay") |>
  count(sender, word) |>
  cast_dtm(sender, word, n)
DTM_discord <- tokens |>
  filter(modality == "discord essay") |>
  count(sender, word) |>
  cast_dtm(sender, word, n)
#(K has already been found)
#LDA
#forum 
LDA_forum <- LDA(DTM_forum, 
                  k = 20, 
                  control = list(seed = 524))
LDA_forum_tidy <- tidy(LDA_forum)
LDA_forum_top <- LDA_forum_tidy |>
  group_by(topic) |>
  slice_max(beta, n = 5, with_ties = FALSE) |>
  ungroup() |>
  arrange(topic, -beta)
#viz
LDA_forum_top |>
  mutate(term = reorder_within(term, beta, topic)) |>
  group_by(topic, term) |>    
  arrange(desc(beta)) |>  
  ungroup() |>
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 5 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
#zoom
LDA_zoom <- LDA(DTM_zoom, 
                 k = 20, 
                 control = list(seed = 524))
LDA_zoom_tidy <- tidy(LDA_zoom)
LDA_zoom_top <- LDA_zoom_tidy |>
  group_by(topic) |>
  slice_max(beta, n = 5, with_ties = FALSE) |>
  ungroup() |>
  arrange(topic, -beta)
#viz
LDA_zoom_top |>
  mutate(term = reorder_within(term, beta, topic)) |>
  group_by(topic, term) |>    
  arrange(desc(beta)) |>  
  ungroup() |>
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 5 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
#discord
LDA_discord <- LDA(DTM_discord, 
                 k = 20, 
                 control = list(seed = 524))
LDA_zoom_tidy <- tidy(LDA_zoom)
LDA_zoom_top <- LDA_zoom_tidy |>
  group_by(topic) |>
  slice_max(beta, n = 5, with_ties = FALSE) |>
  ungroup() |>
  arrange(topic, -beta)
#viz
LDA_zoom_top |>
  mutate(term = reorder_within(term, beta, topic)) |>
  group_by(topic, term) |>    
  arrange(desc(beta)) |>  
  ungroup() |>
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 5 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")