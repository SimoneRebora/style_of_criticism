# word_deletion_analysis

# load libraries
library(stylo)
library(tidyverse)
library(mclust)

# load results of stylometric analysis
load("Delta_results.RData")

# define variables
normalize_distances <- F # TRUE in case of distance varying with no. of MFW

my_mfw <- mfw_max
my_distance_table <- results$distance.table

# Function to normalize values between 0 and 1
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to the entire matrix
if(normalize_distances)
  my_distance_table <- normalize(my_distance_table)

# get basic efficiency
my_efficiency <- 0
my_efficiency_l <- numeric()
for(i in 1:dim(my_distance_table)[1]){
  
  my_distances <- my_distance_table[i,]
  
  correct_attribution <- names(my_distances)[i]
  correct_attribution <- strsplit(correct_attribution, "_")
  correct_attribution <- sapply(correct_attribution, function(x) x[1])
  
  my_distances <- my_distances[-i]
  
  anobii_distance <- mean(my_distances[which(grepl("aNobii", names(my_distances)))])
  sole_distance <- mean(my_distances[which(grepl("LaDomenica", names(my_distances)))])
  
  if(correct_attribution == "aNobii"){
    my_efficiency_l[i] <- sole_distance - anobii_distance
  }else{
    my_efficiency_l[i] <- anobii_distance - sole_distance
  }
  
  if(anobii_distance < sole_distance){
    attribution <- "aNobii"
  }else{
    attribution <- "LaDomenica"
  }
  
  if(correct_attribution == attribution){
    my_efficiency <- my_efficiency+1
  }else{
    print(i)
  }
}

basic_efficiency <- my_efficiency/dim(my_distance_table)[1]

# get ARI

# Extract true class labels from row names
true_labels <- ifelse(grepl("^aNobii", rownames(my_distance_table)), "aNobii", "LaDomenica")

# convert to dist object
my_distance_table <- as.dist(my_distance_table)

# Perform hierarchical clustering
hc <- hclust(my_distance_table, method = "ward.D2")

# Assign clusters (choose the number of clusters `k`)
k <- 2  # Assuming two classes: "first" and "second"
predicted_clusters <- cutree(hc, k)

# Compute Adjusted Rand Index
ari <- adjustedRandIndex(true_labels, predicted_clusters)
print(ari)  # ARI ranges from -1 to 1 (higher is better)
basic_ari <- ari

# get analyzed words
my_words <- results$features.actually.used
my_words

# get all frequencies
all_freqs <- read.csv("table_with_frequencies.txt", sep = " ")
all_freqs <- all_freqs[1:my_mfw,]

# get evolution of distances with varying MFW
efficiency_word <- numeric()
efficiency_word_l <- list()
ari_word <- numeric()
for(my_word in 1:my_mfw){
  
  print(my_word)
  
  all_freqs_tmp <- all_freqs[-my_word,]
  
  stylo_results <- stylo(gui = F, 
                         frequencies = t(all_freqs_tmp), 
                         corpus.lang = "Italian",
                         mfw.min = my_mfw-1,
                         mfw.max = my_mfw-1,
                         distance.measure = my_distance
  )
  
  my_distance_table <- stylo_results$distance.table
  
  # Apply normalization to the entire matrix
  if(normalize_distances)
    my_distance_table <- normalize(my_distance_table)
  
  # get efficiency
  my_efficiency <- 0
  efficiency_word_l[[my_word]] <- numeric()
  for(i in 1:dim(my_distance_table)[1]){
    
    my_distances <- my_distance_table[i,]
    
    correct_attribution <- names(my_distances)[i]
    correct_attribution <- strsplit(correct_attribution, "_")
    correct_attribution <- sapply(correct_attribution, function(x) x[1])
    
    my_distances <- my_distances[-i]
    
    anobii_distance <- mean(my_distances[which(grepl("aNobii", names(my_distances)))])
    sole_distance <- mean(my_distances[which(grepl("LaDomenica", names(my_distances)))])
    
    if(correct_attribution == "aNobii"){
      efficiency_word_l[[my_word]][i] <- sole_distance - anobii_distance
    }else{
      efficiency_word_l[[my_word]][i] <- anobii_distance - sole_distance
    }
    
    if(anobii_distance < sole_distance){
      attribution <- "aNobii"
    }else{
      attribution <- "LaDomenica"
    }
    
    if(correct_attribution == attribution){
      my_efficiency <- my_efficiency+1
    }else{
      print(i)
    }
  }
  
  efficiency_word[my_word] <- my_efficiency/dim(my_distance_table)[1]
  
  
  # get ARI
  
  # Extract true class labels from row names
  true_labels <- ifelse(grepl("^aNobii", rownames(my_distance_table)), "aNobii", "LaDomenica")
  
  # convert to dist object
  my_distance_table <- as.dist(my_distance_table)
  
  # Perform hierarchical clustering
  hc <- hclust(my_distance_table, method = "ward.D2")
  
  # Assign clusters (choose the number of clusters `k`)
  k <- 2  # Assuming two classes: "first" and "second"
  predicted_clusters <- cutree(hc, k)
  
  # Compute Adjusted Rand Index
  ari <- adjustedRandIndex(true_labels, predicted_clusters)
  print(ari)  # ARI ranges from -1 to 1 (higher is better)
  ari_word[my_word] <- ari
  
  
}

unlink("*EDGES.csv")

# show basic efficiency
efficiency_word <- efficiency_word-basic_efficiency
names(efficiency_word) <- my_words[1:my_mfw]

# Convert to data frame
df <- data.frame(
  word = names(efficiency_word),
  value = as.numeric(efficiency_word)
)

# Select the words with the lowest values
df_top <- df[order(df$value), ]

# Create the barplot
ggplot(df_top, aes(x = reorder(word, value), y = value)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Top Words with Lowest Values",
    x = "Words",
    y = "Value"
  ) +
  theme_minimal()


# show ARI efficiency
ari_word <- ari_word-basic_ari
names(ari_word) <- my_words[1:my_mfw]

# Convert to data frame
df <- data.frame(
  word = names(ari_word),
  value = as.numeric(ari_word)
)

# Select the words with the lowest values
df_top <- df[order(df$value), ]

# Create the barplot
ggplot(df_top, aes(x = reorder(word, value), y = value)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Top Words with Lowest Values",
    x = "Words",
    y = "Value"
  ) +
  theme_minimal()

# Compute detailed efficiency
for(i in 1:my_mfw){
  
  efficiency_word_l[[i]] <- efficiency_word_l[[i]] - my_efficiency_l
  
}

efficiency_refined <- sapply(efficiency_word_l, mean)
names(efficiency_refined) <- my_words[1:my_mfw]

# Convert to data frame
df <- data.frame(
  word = names(efficiency_refined),
  value = as.numeric(efficiency_refined)
)

# Select the words with the lowest values
df_top <- df[order(df$value), ]

# Create the barplot
p1 <- ggplot(df_top, aes(x = reorder(word, value), y = value)) +
  geom_col() +
  coord_flip() +  # Flip coordinates for better readability
  labs(
    x = "Words",
    y = "Clustering strength change"
  ) +
  theme_minimal()

p1

ggsave(p1, filename = "Word_deletion_effect.png", width = 15, height = 20, scale = 0.35)
