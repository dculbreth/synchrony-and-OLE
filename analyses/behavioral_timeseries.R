#PREPARE, WRANGLE
install.packages("gridExtra")
library(tidyverse)
library(gridExtra)
raw <- read.csv("data/master.csv")
raw <- raw |>
  select(c("modality",
           "session",
           "date",
           "time",
           "sender")) |>
  filter(sender !="1")
raw <- raw |>
  filter(modality != "Async essay") |>
  filter(modality != "Async modality") |>
  filter(modality != "Sync essay") |>
  filter(modality != "Sync modality") |>
  filter(modality != "Quasi essay") |>
  filter(modality != "Quasi modality")
raw <- raw |>
  mutate(date = as.Date(date,format='%m/%d/%Y')) |>
  mutate(raw, event = paste(date, time)) |>
  select(c("modality","session","event"))
raw <- raw |>
  mutate(event = ymd_hms(event)) |> #if all fail to parse, it's because of the dumb yyyy/mm/dd thing again. if some fail, it's blanks from double replies
  filter(!is.na(event)) #removes the NAs created by ymd_hms failing to parse a blank
#VISUALIZE
raw |> #Async
  filter(modality == "Async") |>
  ggplot(aes(x=event)) +
  geom_histogram(color = "red", fill = "red") +
  xlab(label = "Time") +
  ylab(label = "Number of Interactions") +
  facet_wrap(~session)
Sync1 <- raw |> #Sync (session 1)
  filter(modality == "Sync") |>
  filter(session == "1") |>
  ggplot(aes(x=event)) +
  geom_histogram(color = "red", fill = "red") +
  xlab(label = "Time") +
  ylab(label = "Number of Interactions") +
  ggtitle("1")
Sync2 <- raw |> #Sync (session 2)
  filter(modality == "Sync") |>
  filter(session == "2") |>
  ggplot(aes(x=event)) +
  geom_histogram(color = "red", fill = "red") +
  xlab(label = "Time") +
  ylab(label = "Number of Interactions") +
  ggtitle("2")
grid.arrange(Sync1, Sync2, nrow = 1)
raw |> #Quasi
  filter(modality == "Quasi") |>
  ggplot(aes(x=event)) +
  geom_histogram(color = "red", fill = "red") +
  xlab(label = "Time") +
  ylab(label = "Number of Interactions") +
  facet_wrap(~session)
#####
#old histogram
Async1 <- raw |>
  filter(modality == "Async") |>
  filter(session == "1")|>
  select("event")
Async2 <- raw |>
  filter(modality == "Async") |>
  filter(session == "2")|>
  select("event")
hist(Async1$event, breaks = "hours", #(from https://stackoverflow.com/questions/42368826/histogram-of-timestamp)
     col="red", main = "Async 1",  
     xlab = "Timestamp", ylab = "Frequency", freq = TRUE)
hist(Async2$event, breaks = "hours", 
     col="red", main = "Async 2",  
     xlab = "Timestamp", ylab = "Frequency", freq = TRUE)
Sync1 <- raw |>
  filter(modality == "Sync") |>
  filter(session == "1")|>
  select("event")
Sync2 <- raw |>
  filter(modality == "Sync") |>
  filter(session == "2")|>
  select("event")
hist(Sync1$event, breaks = 120, 
     col="red", main = "Sync 1",  
     xlab = "Timestamp", ylab = "Frequency", freq = TRUE)
hist(Sync2$event, breaks = 120, 
     col="red", main = "Sync 2",  
     xlab = "Timestamp", ylab = "Frequency", freq = TRUE)
Quasi1 <- raw |>
  filter(modality == "Quasi") |>
  filter(session == "1")|>
  select("event")
Quasi2 <- raw |>
  filter(modality == "Quasi") |>
  filter(session == "2")|>
  select("event")
hist(Quasi1$event, breaks = "hours", 
     col="red", main = "Quasi 1",  
     xlab = "Timestamp", ylab = "Frequency", freq = TRUE)
hist(Quasi2$event, breaks = "hours", 
     col="red", main = "Quasi 2",  
     xlab = "Timestamp", ylab = "Frequency", freq = TRUE)