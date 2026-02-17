#PREPARE
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(skimr)
library(janitor)
#WRANGLE
raw <- read.csv("data/master.csv")
raw <-raw |>
  filter(modality == "Async" | modality == "Sync" | modality == "Quasi") |>
  select(c("modality",
           "session",
           "sender",
           "receiver")) |>
  mutate(sender = as.character(sender)) |>
  mutate(receiver = as.character(receiver)) |>
  filter(sender != "1")
#edge lists
Async_edges <- raw |>
  filter(modality == "Async") |>
  select(c("sender",
           "receiver"))
Sync_edges <- raw |>
  filter(modality == "Sync") |>
  select(c("sender",
           "receiver"))
Quasi_edges <- raw |>
  filter(modality == "Quasi") |>
  select(c("sender",
           "receiver"))
#node lists
OP <- data.frame(1, "both")
names(OP) <- c("sender","session")
Async_nodes <- raw |>
  filter(modality == "Async") |>
  select(c("sender",
           "session")) |>
  distinct(sender, .keep_all = TRUE) |>
  rbind(OP) |>
  mutate(sender = as.character(sender)) |>
  mutate(session = as.character(session))
Sync_nodes <- raw |>
  filter(modality == "Sync") |>
  select(c("sender",
           "session")) |>
  distinct(sender, .keep_all = TRUE) |>
  rbind(OP) |>
  mutate(sender = as.character(sender)) |>
  mutate(session = as.character(session))
Quasi_nodes <- raw |>
  filter(modality == "Quasi") |>
  select(c("sender",
           "session")) |>
  distinct(sender, .keep_all = TRUE) |>
  rbind(OP) |>
  mutate(sender = as.character(sender)) |>
  mutate(session = as.character(session))
#ANALYZE
#summary stats
raw |> 
  count(sender) |>
  summarise(across(where(is.numeric), .fns = 
                     list(min = min,
                          median = median,
                          mean = mean,
                          stdev = sd,
                          q25 = ~quantile(., 0.25),
                          q75 = ~quantile(., 0.75),
                          max = max))) %>%
  pivot_longer(everything(), names_sep='_', names_to=c('variable', '.value'))
#number of engagements by student and modality
participation <- raw |>
  select(c("modality","sender")) |>
  count(modality, sender, sort = TRUE)
#Async
Async_network <- tbl_graph(edges = Async_edges,
                           nodes = Async_nodes,
                           node_key = "sender",
                           directed = TRUE)
autograph(Async_network)
gsize(Async_network) #total edges
gorder(Async_network) #total vertices
Async_edges |> #unique edges
  distinct(sender, receiver) |>
  count()
edge_density(Async_network) #graph density
reciprocity(Async_network)
transitivity(Async_network, type = "undirected")
centr_degree(Async_network, mode = "in") #centralization
centr_degree(Async_network, mode = "out") #centralization
#Sync
Sync_network <- tbl_graph(edges = Sync_edges,
                          nodes = Sync_nodes,
                          node_key = "sender",
                          directed = TRUE)
autograph(Sync_network)
gsize(Sync_network) #total edges
gorder(Sync_network) #total vertices
Sync_edges |> #unique edges
  distinct(sender, receiver) |>
  count()
edge_density(Sync_network) #graph density
reciprocity(Sync_network)
transitivity(Sync_network, type = "undirected")
centr_degree(Sync_network, mode = "in") #centralization
centr_degree(Sync_network, mode = "out") #centralization
#Quasi
Quasi_network <- tbl_graph(edges = Quasi_edges,
                             nodes = Quasi_nodes,
                             node_key = "sender",
                             directed = TRUE)
autograph(Quasi_network)
gsize(Quasi_network) #total edges
gorder(Quasi_network) #total vertices
Quasi_edges |> #unique edges
  distinct(sender, receiver) |>
  count()
edge_density(Quasi_network) #graph density
reciprocity(Quasi_network)
transitivity(Quasi_network, type = "undirected")
centr_degree(Quasi_network, mode = "in") #centralization
centr_degree(Quasi_network, mode = "out") #centralization
#VISUALIZE
#participation
raw |>
  count(sender) |>
  ggplot(aes(x = reorder(sender, +n), y = n)) +
  geom_bar(stat="identity", color='red',fill='red') +
  xlab(label = "Student") +
  ylab(label = "Interactions") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
raw |>
  count(sender) |>
  ggplot(aes(x = n)) +
  geom_histogram(color = "red", fill = "red") +
  xlab(label = "Total Interactions") +
  ylab(label = "Number of Students")
ggplot(participation) +
  geom_bar(aes(x = sender,
               y = n,
               fill = modality,
               color = modality),
           stat = "identity",
           position = position_dodge()) +
  xlab(label = "Student") +
  ylab(label = "Interactions") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
#SNA:Async
Async_network <- Async_network |>
  activate(edges) |>
  mutate(reciprocated = edge_is_mutual())
Async_network <- Async_network |>
  activate(nodes) |>
  mutate(degree = centrality_degree(mode = "all"))
ggraph(Async_network, layout = "fr") +
  geom_node_point(aes(size = degree, colour = session)) +
  geom_edge_link(aes(colour = reciprocated)) +
  theme_void()
#SNA:Sync
Sync_network <- Sync_network |>
  activate(edges) |>
  mutate(reciprocated = edge_is_mutual())
Sync_network <- Sync_network |>
  activate(nodes) |>
  mutate(degree = centrality_degree(mode = "all"))
ggraph(Sync_network, layout = "fr") +
  geom_node_point(aes(size = degree, colour = session)) +
  geom_edge_link(aes(colour = reciprocated)) +
  theme_void()
#SNA:Quasi
Quasi_network <- Quasi_network |>
  activate(edges) |>
  mutate(reciprocated = edge_is_mutual())
Quasi_network <- Quasi_network |>
  activate(nodes) |>
  mutate(degree = centrality_degree(mode = "all"))
ggraph(Async_network, layout = "fr") +
  geom_node_point(aes(size = degree, colour = session)) +
  geom_edge_link(aes(colour = reciprocated)) +
  theme_void()
#####
#labels for actors
ggraph(Async_network, layout = "fr") +
  geom_node_point(aes(size = degree, colour = session)) +
  geom_edge_link(aes(colour = reciprocated)) +
  geom_node_text(aes(label = sender), repel = TRUE) +
  theme_void()
#degree
Async_network <- Async_network |>
  activate(nodes) |>
  mutate(in_degree = centrality_degree(mode = "in"),
         out_degree = centrality_degree(mode = "out"))
Async_network |> 
  as_tibble() |>
  ggplot() +
  geom_histogram(aes(x = in_degree))
########
#alt time series plot?
#SCRAP#
ggplot(Asyncs1,
       aes(x=event)) +
  geom_histogram