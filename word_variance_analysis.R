# word_variance_analysis

# load libraries
library(stylo)
library(tidyverse)
library(reshape2)

# load results of stylometric analysis
load("Delta_results.RData")

# define variables
n_words <- 10
my_mfw <- mfw_max

# visualize z-scores for anobii texts
zeta_scores <- results$table.with.all.zscores
zeta_scores <- zeta_scores[which(grepl("aNobii", rownames(zeta_scores))),1:my_mfw]

# calulate variance for each set of zeta scores (per word)
my_variance <- numeric()

for(i in 1:dim(zeta_scores)[2]){
  
  my_variance[i] <- mean(abs(zeta_scores[,i] - mean(zeta_scores[,i])))
  
  
}

# reorder the table based on the variance
zeta_scores <- as.data.frame(t(zeta_scores))
zeta_scores$variance <- my_variance
zeta_scores <- zeta_scores[order(zeta_scores$variance),]

# join variance info to the words
zeta_scores$word <- paste(rownames(zeta_scores), " (variance ", round(zeta_scores$variance, 2), ")", sep = "")
zeta_scores$variance <- NULL

# prepare first visualization (words with least variance)
zeta_scores_m <- melt(zeta_scores[1:n_words,], id.vars = "word", value.name = "zeta_value", variable.name = "Style")
zeta_scores_m$word <- factor(zeta_scores_m$word, levels = zeta_scores$word[1:n_words])

p1 <- ggplot(zeta_scores_m, aes(x = Style, y = zeta_value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~word, nrow = 2) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  xlab("aNobii texts") + ylab("zeta scores")
p1

ggsave(p1, filename = "Zeta_anobii.png", width = 16, height = 10, scale = 0.45)


# visualize z-scores for sole texts
zeta_scores <- results$table.with.all.zscores
zeta_scores <- zeta_scores[which(grepl("LaDomenica", rownames(zeta_scores))),1:my_mfw]

# calulate variance for each set of zeta scores (per word)
my_variance <- numeric()

for(i in 1:dim(zeta_scores)[2]){
  
  my_variance[i] <- mean(abs(zeta_scores[,i] - mean(zeta_scores[,i])))
  
  
}

# reorder the table based on the variance
zeta_scores <- as.data.frame(t(zeta_scores))
zeta_scores$variance <- my_variance
zeta_scores <- zeta_scores[order(zeta_scores$variance),]

# join variance info to the words
zeta_scores$word <- paste(rownames(zeta_scores), " (variance ", round(zeta_scores$variance, 2), ")", sep = "")
zeta_scores$variance <- NULL

# prepare first visualization (words with least variance)
zeta_scores_m <- melt(zeta_scores[1:n_words,], id.vars = "word", value.name = "zeta_value", variable.name = "Style")
zeta_scores_m$word <- factor(zeta_scores_m$word, levels = zeta_scores$word[1:n_words])

p1 <- ggplot(zeta_scores_m, aes(x = Style, y = zeta_value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~word, nrow = 2) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  xlab("La Domenica texts") + ylab("zeta scores")
p1

ggsave(p1, filename = "Zeta_sole.png", width = 16, height = 10, scale = 0.45)



