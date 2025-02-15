# KWIC_analysis

# load libraries
library(quanteda)

# Define variables
keyword <- "ma"  # Change this to any keyword you want
window_size <- 10     # Context window size

# Load text corpus
load("Delta_results.RData")

# Create a quanteda corpus for aNobii
corpus <- tokens(anobii_full)

# Run KWIC
kwic_results <- kwic(corpus, pattern = keyword, window = window_size)

# Export the results to a CSV file
write.csv(kwic_results, paste0("kwic_results_aNobii_", keyword, ".csv"), row.names = FALSE)

# Repeat procedure for Sole
corpus <- tokens(sole_full)
kwic_results <- kwic(corpus, pattern = keyword, window = window_size)
write.csv(kwic_results, paste0("kwic_results_LaDomenica_", keyword, ".csv"), row.names = FALSE)
