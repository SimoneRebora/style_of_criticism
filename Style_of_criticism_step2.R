###This file contains the script for the SVM classification of the texts
###It should be run after "Style_of_criticism_step1.R"
###and after the LIWC analysis

library(methods)	  # to avoid error with nohup
library(e1071)      # for the SVM classifier

first_step_output_file <- "Results_step1.RData"
LIWC_results_file <- "LIWC_results.csv"
output_file <- "Final_results.RData"

##Load results of first step
load(first_step_output_file)

##Load LIWC results
LIWC_results <- read.csv(LIWC_results_file, stringsAsFactors = F)
rownames(LIWC_results) <- unlist(strsplit(LIWC_results$Filename, ".txt"))

##LIWC results have a different row order, they have to be matched with the "evaluation_results" rows
LIWC_results <- LIWC_results[match(rownames(evaluation_results), rownames(LIWC_results)), ]

##Add LIWC measurements to the main dataframe
evaluation_results <- cbind(evaluation_results, Emotion = LIWC_results$Affett, Body = LIWC_results$Corpo, Social = LIWC_results$Social, stringsAsFactors = F)

evaluation_results$Emotion <- as.numeric(gsub(",", ".", evaluation_results$Emotion))
evaluation_results$Body <- as.numeric(gsub(",", ".", evaluation_results$Body))
evaluation_results$Social <- as.numeric(gsub(",", ".", evaluation_results$Social))

##Define ground truth for evaluation
evaluation_results$truth <- unlist(lapply(rownames(evaluation_results), function(x) {unlist(strsplit(x, "_"))[1]}))

############
####SVM classification
#########

prediction_quality <- numeric()
###SVM evaluation, through a "leave-one-out" design
for(i in 1:length(evaluation_results$Emotion)){
  training_data <- evaluation_results[-i,]
  test_data <- evaluation_results[i,]
  model <- svm(training_data$truth~., training_data, type = "C")
  res <- predict(model, newdata=test_data)
  if(res == test_data$truth)
    prediction_quality[i] <- 1
  else
    prediction_quality[i] <- 0
  print(i)
}

###calculate final quality for SVM
prediction_svm_full_quality <- sum(prediction_quality)/length(evaluation_results$stylometry)

###stylometry evaluation (to compare results)
evaluation_results$stylometry <- as.character(evaluation_results$stylometry)
stylo_prediction_quality <- numeric()
for(i in 1:length(evaluation_results$Knoop_score)){
  res <- unlist(strsplit(evaluation_results$stylometry[i], "_"))[1]
  if(res == evaluation_results$truth[i])
    stylo_prediction_quality[i] <- 1
  else
    stylo_prediction_quality[i] <- 0
}

###calculate final quality for stylometry
prediction_stylo_full_quality <- sum(stylo_prediction_quality)/length(evaluation_results$stylometry)

###print results
print("SVM")
print(prediction_svm_full_quality)
print("Stylo")
print(prediction_stylo_full_quality)

###save results
save.image(output_file)