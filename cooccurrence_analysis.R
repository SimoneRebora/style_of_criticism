# cooccurrence_analysis

# load libraries
library(udpipe)
library(igraph)
library(ggraph)
library(tidyverse)
library(gridExtra)

# define variables
my_lang <- "italian"
my_words <- c("libro")

upos_reduction <- T
upos_selection <- c("NOUN", "ADJ", "ADV", "VERB", "INTJ") 
# limited to open class words (https://universaldependencies.org/u/pos/)

my_term <- "lemma" # choose between "token" and "lemma"

# prepare or load annotated corpus
if(file.exists("all_reviews_udpipe.RData")){
  load("all_reviews_udpipe.RData")
}else{
  load("Delta_results.RData")
  anobii_df <- data.frame(doc_id = paste("anobii", 1:length(anobii_full)), text = anobii_full, stringsAsFactors = F)
  sole_df <- data.frame(doc_id = paste("sole", 1:length(sole_full)), text = sole_full, stringsAsFactors = F)
  my_df <- rbind(anobii_df, sole_df)
  reviews_annotated <- udpipe(my_df, object = my_lang)
  save(reviews_annotated, file = "all_reviews_udpipe.RData")
}

# create Group variable
reviews_annotated <- reviews_annotated %>%
  mutate(group = case_when(
    grepl("anobii", doc_id, ignore.case = TRUE) ~ "GroupA",
    grepl("sole", doc_id, ignore.case = TRUE) ~ "GroupB",
    TRUE ~ "Other"  # Optional: Assign "Other" if no match
  ))

all_texts <- reviews_annotated

# calculate co-occurrences
if(!upos_reduction)
  upos_selection <- unique(reviews_annotated$upos)

# for Group A
cooc_GroupA <- cooccurrence(x = subset(all_texts, 
                                       upos %in% upos_selection &
                                         group == "GroupA"), 
                            term = my_term, 
                            group = c("doc_id", 
                                      "paragraph_id",
                                      "sentence_id"))

# for Group B
cooc_GroupB <- cooccurrence(x = subset(all_texts, 
                                       upos %in% upos_selection &
                                         group == "GroupB"), 
                            term = my_term, 
                            group = c("doc_id", 
                                      "paragraph_id",
                                      "sentence_id"))

# let's focus on a group of words
# ...and filter the co-occurrences just for it
cooc_GroupA_filter <- cooc_GroupA[which(cooc_GroupA$term1 %in% my_words |
                                          cooc_GroupA$term2 %in% my_words),]
# make network graph
network_data <- cooc_GroupA_filter[1:100,]
wordnetwork <- graph_from_data_frame(network_data)
nodes <- V(wordnetwork)$name
colors <- rep("darkgreen", length(nodes))
colors[which(nodes %in% my_words)] <- "red"
p1 <- ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = colors, size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "aNobii")

# Show only the term that is not the one we chose
cooc_GroupA_filter$selected_term <- ifelse(cooc_GroupA_filter$term1 %in% my_words, cooc_GroupA_filter$term2, cooc_GroupA_filter$term1)
cooc_GroupA_filter <- data.frame(term = cooc_GroupA_filter$selected_term,
                                 cooc = cooc_GroupA_filter$cooc)

# sum scores for repeated terms
cooc_GroupA_filter <- cooc_GroupA_filter %>%
  group_by(term) %>%
  summarise(cooc = sum(cooc)) %>%
  arrange(desc(cooc)) %>%
  as.data.frame()

# filter also for group B
cooc_GroupB_filter <- cooc_GroupB[which(cooc_GroupB$term1 %in% my_words |
                                          cooc_GroupB$term2 %in% my_words),]

# make network graph
network_data <- cooc_GroupB_filter[1:100,]
wordnetwork <- graph_from_data_frame(network_data)
nodes <- V(wordnetwork)$name
colors <- rep("darkgreen", length(nodes))
colors[which(nodes %in% my_words)] <- "red"
p2 <- ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = colors, size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "La Domenica")

combined_plot <- grid.arrange(p1, p2)

ggsave(combined_plot, file = paste("Cooccurrence_graph_", paste(my_words, collapse = "-"), ".png", sep = ""), scale = 1.5)

# Show only the term that is not the one we chose
cooc_GroupB_filter$selected_term <- ifelse(cooc_GroupB_filter$term1 %in% my_words, cooc_GroupB_filter$term2, cooc_GroupB_filter$term1)
cooc_GroupB_filter <- data.frame(term = cooc_GroupB_filter$selected_term,
                                 cooc = cooc_GroupB_filter$cooc)

# sum scores for repeated terms
cooc_GroupB_filter <- cooc_GroupB_filter %>%
  group_by(term) %>%
  summarise(cooc = sum(cooc)) %>%
  arrange(desc(cooc)) %>%
  as.data.frame()

# Join the two together
cooc_df <- full_join(cooc_GroupA_filter, cooc_GroupB_filter, by = "term", suffix = c("_GroupA", "_GroupB"))

# convert NAs to zeros
cooc_df <- cooc_df %>%
  mutate(cooc_GroupA = coalesce(cooc_GroupA, 0),
         cooc_GroupB = coalesce(cooc_GroupB, 0))

# normalize by occurrence of focus terms
cooc_df$cooc_GroupA_norm <- cooc_df$cooc_GroupA/length(which(all_texts$group == "GroupA" & all_texts$lemma %in% my_words))
cooc_df$cooc_GroupB_norm <- cooc_df$cooc_GroupB/length(which(all_texts$group == "GroupB" & all_texts$lemma %in% my_words))

# select just the most different co-occurrences
cooc_df$score <- abs(cooc_df$cooc_GroupA_norm - cooc_df$cooc_GroupB_norm)
cooc_df <- cooc_df[order(-cooc_df$score),]

# make a plot
# Reshape the data for ggplot
df_long <- tidyr::gather(cooc_df[1:20,], key = "group", value = "value", cooc_GroupA_norm, cooc_GroupB_norm)

# rename columns in Ferrante
df_long$group[which(df_long$group == "cooc_GroupA_norm")] <- "aNobii"
df_long$group[which(df_long$group == "cooc_GroupB_norm")] <- "La Domenica"

# Create the ggplot
p1 <- ggplot(df_long, aes(x = term, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("red","green")) +
  labs(x = "Term",
       y = "Normalized Value",
       fill = "Reviews") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

p1

ggsave(p1, filename = paste("Cooccurrence_plot_", paste(my_words, collapse = "-"), ".png", sep = ""), width = 16, height = 9, scale = 0.7)
