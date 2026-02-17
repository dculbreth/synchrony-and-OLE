#PREPARE
install.packages(c("dplyr","readr","tidyr","writexl","readxl","tidytext","textdata","ggplot2","scales","wordcloud2"))
library(dplyr)
library(readr)
library(tidyr)
library(writexl)
library(tidytext)
library(textdata)
library(ggplot2)
library(scales)
library(wordcloud2)
raw <- read.csv("data/master.csv")
raw <-raw |>
  filter(modality == "Async" | modality == "Sync" | modality == "Quasi") |>
  select(c("modality", 
           "sender",
           "session",
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
token_frequency <- count(tokens, word, sort = TRUE)
token_frequency
my_stopwords <- c("2", "42", "pp", "31035", "60407")
tokens <- tokens |>
  filter(!word %in% my_stopwords)
rm(my_stopwords)
#ANALYZE
#AFINN
afinn <- get_sentiments("afinn")
AFINN_global <- inner_join(tokens, afinn, by = "word")
AFINN_Async <- AFINN_global |>
  filter(modality == "Async") |>
  summarize(sentiment = sum(value))
AFINN_Sync <- AFINN_global |>
  filter(modality == "Sync") |>
  summarize(sentiment = sum(value))
AFINN_Quasi <- AFINN_global |>
  filter(modality == "Quasi") |>
  summarize(sentiment = sum(value))
#BING
bing <- get_sentiments("bing")
BING_global <- inner_join(tokens, bing, by = "word")
BING_Async <- BING_global |>
  filter(modality == "Async") |>
  count(sentiment, sort = TRUE)
BING_Sync <- BING_global |>
  filter(modality == "Sync") |>
  count(sentiment, sort = TRUE)
BING_Quasi <- BING_global |>
  filter(modality == "Quasi") |>
  count(sentiment, sort = TRUE)
#NRC
nrc <- get_sentiments("nrc")
NRC_global <- inner_join(tokens, nrc, by = "word")
NRC_Async <- NRC_global |>
  filter(modality == "Async") |>
  count(sentiment, sort = TRUE)
NRC_Sync <- NRC_global |>
  filter(modality == "Sync") |>
  count(sentiment, sort = TRUE)
NRC_Quasi <- NRC_global |>
  filter(modality == "Quasi") |>
  count(sentiment, sort = TRUE)
#VISUALIZE
#token frequency (table)
freq_Async <- tokens |> #Async
  select(c("modality","word")) |>
  filter(modality == "Async") |>
  group_by(word) |>
  count(sort = TRUE) |>
  ungroup() |>
  slice_max(order_by = n, n = 25)
write_csv(freq_Async, "freq_Async.csv")
freq_Sync <- tokens |> #Sync
  select(c("modality","word")) |>
  filter(modality == "Sync") |>
  group_by(word) |>
  count(sort = TRUE) |>
  ungroup() |>
  slice_max(order_by = n, n = 25)
write_csv(freq_Sync, "freq_Sync.csv")
freq_Quasi <- tokens |> #Quasi
  select(c("modality","word")) |>
  filter(modality == "Quasi") |>
  group_by(word) |>
  count(sort = TRUE) |>
  ungroup() |>
  slice_max(order_by = n, n = 25)
write_csv(freq_Quasi, "freq_Quasi.csv")
#word cloud
cloud_Async <- tokens |>
  select(c("modality","word")) |>
  filter(modality == "Async") |>
  group_by(word) |>
  count(sort = TRUE) |>
  ungroup() |>
  slice_max(order_by = n, n = 100)
wordcloud2(cloud_Async)
cloud_Sync <- tokens |>
  select(c("modality","word")) |>
  filter(modality == "Sync") |>
  group_by(word) |>
  count(sort = TRUE) |>
  ungroup() |>
  slice_max(order_by = n, n = 100)
wordcloud2(cloud_Sync)
cloud_Quasi <- tokens |>
  select(c("modality","word")) |>
  filter(modality == "Quasi") |>
  group_by(word) |>
  count(sort = TRUE) |>
  ungroup() |>
  slice_max(order_by = n, n = 100)
wordcloud2(cloud_Quasi)
#verbosity
verb <- tokens |>
  group_by(modality) |>
  count(sort = TRUE)
ggplot(verb,
       aes(modality, n, color = modality, fill = modality)) +
  geom_col() +
  xlab(label = "Modality") +
  ylab(label = "Number of Tokens") +
  theme(legend.position="none")
#AFINN
AFINN_viz <- data.frame( #I was tired so I just made a dataframe for this
  modality = c("Async","Sync","Quasi"),
  sentiment = c(353,153,249))
ggplot(AFINN_viz, aes(modality, sentiment, color = modality, fill = modality)) +
  geom_col() +
  xlab(label = "Modality") +
  ylab(label = "Net Sentiment") +
  theme(legend.position="none")
#BING
BING_viz <- data.frame( #this has beene edited for drafting new tables
  modality = c("Async","Async", "Sync", "Sync","Quasi", "Quasi"),
  Sentiment = c("positive","negative", "positive","negative","positive","negative"),
  frequency = c(505, 421, 110, 84, 188, 110))
ggplot(BING_viz, 
       aes(modality, frequency, fill = Sentiment)) +
  geom_col() +
  xlab(label = "Modality") +
  ylab(label = "Number of Tokens")
#NRC
#bar graph
NRC_bar <- inner_join(NRC_Async, NRC_Sync, by = "sentiment")
NRC_bar <- NRC_bar |>
  inner_join(NRC_Quasi, by = "sentiment") |>
  rename(Async = n.x, Sync = n.y, Quasi = n) |>
  gather(modality, n, -sentiment) #wide to long format
ggplot(NRC_bar) +
  geom_bar(aes(x = sentiment,
               y = n,
               fill = modality,
               color = modality),
           stat = "identity",
           position = position_dodge()) +
  xlab(label = "Sentiment") +
  ylab(label = "Number of Tokens") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
#pie chart
NRC_pie_Async <- NRC_global |>
  filter(modality == "Async") |>
  select(c("word","sentiment")) |>
  group_by(sentiment) |>
  count() |>
  ungroup() |> 
  mutate(per=`n`/sum(`n`)) |> #this adds percent calculations
  arrange(desc(sentiment))
NRC_pie_Async$label <- scales::percent(NRC_pie_Async$per) #this changes the data type? 
ggplot(NRC_pie_Async,
       aes("", n, fill = sentiment)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(x=1, y = cumsum(n) - n/2, label=label)) #this attaches the label to the slices
NRC_pie_Sync <- NRC_global |>
  filter(modality == "Sync") |>
  select(c("word","sentiment")) |>
  group_by(sentiment) |>
  count() |>
  ungroup() |> 
  mutate(per=`n`/sum(`n`)) |> #this adds percent calculations
  arrange(desc(sentiment))
NRC_pie_Sync$label <- scales::percent(NRC_pie_Sync$per) #this changes the data type? 
ggplot(NRC_pie_Sync,
       aes("", n, fill = sentiment)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(x=1, y = cumsum(n) - n/2, label=label)) #this attaches the label to the slices
NRC_pie_Quasi <- NRC_global |>
  filter(modality == "Quasi") |>
  select(c("word","sentiment")) |>
  group_by(sentiment) |>
  count() |>
  ungroup() |> 
  mutate(per=`n`/sum(`n`)) |> #this adds percent calculations
  arrange(desc(sentiment))
NRC_pie_Quasi$label <- scales::percent(NRC_pie_Quasi$per) #this changes the data type? 
ggplot(NRC_pie_Quasi,
       aes("", n, fill = sentiment)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(x=1, y = cumsum(n) - n/2, label=label)) #this attaches the label to the slices
#########
#separate bar graphs template for NRC
ggplot(NRC_Async, aes(sentiment, n)) +
  geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Async", 
       subtitle="NRC Categorization") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))