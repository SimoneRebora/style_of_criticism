# stylo_analysis

# load libraries
library(stylo)
library(stringr)

# define variables
anobii_min_quartile <- 2 #remove first quartile of anobii lenghts (very short reviews)
split_length <- 5000
mfw_min <- 10
mfw_max <- 50
mfw_incr <- 20
my_distance <- "dist.wurzburg"

output_folder <- "corpus/"

# delete output folder if present
unlink(output_folder, recursive = T)
# re-create it
dir.create(output_folder)

# first function (split based on simple spaces and preserving all characters: all split words are put back in line)
make_lexicon_simple <- function(text_corpus, text_corpus_title, length_limit = 5000){
  print("paste corpus")
  text_corpus <- paste(text_corpus, collapse = " ")
  print("split with space")
  text_corpus <- unlist(strsplit(text_corpus, "\\s+"))
  print("split in groups")
  text_corpus <- split(text_corpus, ceiling(seq_along(text_corpus)/length_limit))
  text_corpus <- text_corpus[-length(text_corpus)]
  print("back into one string")
  for(i in 1:length(text_corpus)){
    text_corpus[[i]] <- paste(text_corpus[[i]], collapse = " ")
  }
  names(text_corpus) <- paste(text_corpus_title, 1:length(text_corpus), sep = "_")
  print("Simple lexicon done")
  return(text_corpus)
}


# second function
remove_punctuation <- function(input_string) {
  # Define a regular expression to match punctuation marks
  punctuation_regex <- "[[:punct:]]"
  
  # Replace punctuation marks with spaces
  cleaned_string <- gsub(punctuation_regex, " ", input_string)
  
  return(cleaned_string)
}

# third function (count matches of a string in a text)
count_matches <- function(pattern, text) {
  matches <- gregexpr(pattern, text)
  num_matches <- sum(sapply(matches, function(x) ifelse(x[[1]][1] == -1, 0, length(x))))
  return(num_matches)
}

# Prepare social reading texts

# Define the file path
file_path <- "corpus_clean/SOCIAL_aNobii.txt"

# Read the file content line-by-line
lines <- readLines(file_path)

# Initialize a counter to track the expected numerical separator
expected_number <- 1
all_texts <- character()

# Loop through each line
for (line in lines) {
  if (line == "***************************")
    next
  if (!is.na(as.numeric(line)) & as.numeric(line) == expected_number){
    my_text <- expected_number
    expected_number <- expected_number+1
    all_texts[my_text] <- ""
    next
  }
  
  all_texts[my_text] <- paste(all_texts[my_text], line, sep = "\n")  
  
}

# calculate corpus lengths
text_corpus <- strsplit(all_texts, "\\W")
text_corpus <- lapply(text_corpus, function(x) x[-which(x == "")])
all_lengths <- lengths(text_corpus)
all_quartiles <- quantile(all_lengths)

# filter through quartiles
anobii_length_limit <- all_quartiles[anobii_min_quartile]
all_texts <- all_texts[-which(all_lengths < anobii_length_limit)]

# get stats
all_lengths <- all_lengths[-which(all_lengths < anobii_length_limit)]
min(all_lengths)
max(all_lengths)
mean(all_lengths)
sd(all_lengths)
sum(all_lengths)

# save texts for processing
anobii <- all_texts
anobii <- make_lexicon_simple(anobii, "aNobii", length_limit = split_length)
anobii_full <- all_texts

# external_save
for(i in 1:length(anobii))
  cat(anobii[[i]], sep = " ", file = paste(output_folder, "aNobii", "_", i, ".txt", sep = ""))


# Prepare magazine texts

sole <- readLines("corpus_clean/MAGAZINE_LaDomenicaSole24Ore.txt")

# Initialize a counter to track the expected numerical separator
expected_number <- 1
all_texts <- character()

exclude_string <- paste(1:700, "************************************")

# Loop through each line
for (line in sole) {
  if (line %in% exclude_string){
    my_text <- expected_number
    expected_number <- expected_number+1
    all_texts[my_text] <- ""
    next
  }

  all_texts[my_text] <- paste(all_texts[my_text], line, sep = " ")  
  
}

# get lengths
sole_full <- all_texts
text_corpus <- strsplit(all_texts, "\\W")
text_corpus <- lapply(text_corpus, function(x) x[-which(x == "")])
all_lengths <- lengths(text_corpus)

# get stats
min(all_lengths)
max(all_lengths)
mean(all_lengths)
sd(all_lengths)
sum(all_lengths)

# prepare for external save
exclude <- which(sole %in% exclude_string)
sole <- sole[-exclude]
sole <- make_lexicon_simple(sole, "LaDomenica", length_limit = split_length)

# external_save
for(i in 1:length(sole))
  cat(sole[[i]], sep = " ", file = paste("corpus/LaDomenica", "_", i, ".txt", sep = ""))

# stylometric analysis
results <- stylo(gui = F, 
                 corpus.lang = "Italian",
                 distance.measure = my_distance,
                 mfw.min = mfw_min,
                 mfw.max = mfw_max,
                 mfw.incr = mfw_incr,
                 write.png.file = TRUE,
                 plot.custom.height = 21,
                 plot.custom.width = 7,
                 plot.font.size = 10)

cat(results$features.actually.used, sep = "\n")

save.image(file = "Delta_results.RData")

# Lexicon analysis

# read lexicon
lexicon <- read.csv("lexicons/Dizionario_critica.csv", stringsAsFactors = F)
lexicon$ITALIAN <- sapply(lexicon$ITALIAN, function(x) paste("", x, ""))
lexicon$ITALIAN

# prepare anobii
anobii <- lapply(anobii, remove_punctuation)
anobii <- lapply(anobii, tolower)
anobii <- lapply(anobii, function(x) paste("", x, ""))
anobii[[1]]

# prepare sole
sole <- lapply(sole, remove_punctuation)
sole <- lapply(sole, tolower)
sole <- lapply(sole, function(x) paste("", x, ""))
sole[[1]]

# build dataset
dataset <- c(anobii, sole)

# Initialize an empty dataframe to store the results
results_df <- data.frame(matrix(ncol = length(lexicon$ITALIAN), nrow = length(dataset)))
colnames(results_df) <- lexicon$ITALIAN

# Nested loop to count matches
for (i in 1:length(lexicon$ITALIAN)) {
  print(i/length(lexicon$ITALIAN))
  for (j in 1:length(dataset)) {
    results_df[j, i] <- count_matches(lexicon$ITALIAN[i], dataset[[j]])
  }
}

# Add row names to result_df
row.names(results_df) <- names(dataset)

# run stylo analysis with table of matches
stylo(gui = F, 
      corpus.lang = "Italian",
      distance.measure = my_distance,
      mfw.min = length(results_df),
      mfw.max = length(results_df),
      frequencies = results_df,
      write.png.file = TRUE,
      plot.custom.height = 21,
      plot.custom.width = 7,
      plot.font.size = 10)

# remove all images generated
# unlink("*_CA_*")
