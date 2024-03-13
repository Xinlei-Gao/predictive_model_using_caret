#!/usr/bin/env Rscript
# read the input file and parameters
args <- commandArgs(T) 
# example options for gene expression data
# args <- c("./sample_dataset/RNAseq_data.csv",
#           "./sample_dataset/labels.csv",
#           "./test_run/model.rds",
#           "./test_run/result.log",
#           "TRUE",
#           "NULL",
#           "TRUE",
#           "FALSE",
#           "TRUE",
#           "25",
#           "NULL",
#           "kNN",
#           "NULL"
#          )
# # example options for DNA mutation data
# args <- c("./sample_dataset/NGS_data.csv",
#           "./sample_dataset/labels.csv",
#           "./test_run/model.rds",
#           "./test_run/result.log",
#           "TRUE",
#           "NULL",
#           "TRUE",
#           "FALSE",
#           "FALSE",
#           "NULL",
#           "rfe",
#           "binomial_logistic",
#           "group1"
# )
# # example options for DNA methylation data (using imputation)
# args <- c("./sample_dataset/Methylation_data.csv",
#           "./sample_dataset/labels.csv",
#           "./test_run/model.rds",
#           "./test_run/result.log",
#           "TRUE",
#           "median",
#           "FALSE",
#           "FALSE",
#           "FALSE",
#           "NULL",
#           "rfe",
#           "multinomial_logistic",
#           "NULL"
# )
data_path = args[1]
label_path = args[2]
output_model = args[3]
output_log = args[4]
normalize = args[5]
impute_method = args[6]
complete_case = args[7]
encode_categorical = args[8]
reduce_dimension = args[9]
num_components = args[10]
feature_selection_method = args[11]
model_selection = args[12]
positive_outcome = args[13]

# install required packages
# load pre-built functions
source("/nfs/BaRC_Public/BaRC_code/R/predictive_model_using_caret/custom_functions.R")

preprocessed_data <- preprocess_data(data_path = data_path, 
                                     label_path = label_path, 
                                     normalize = normalize, 
                                     impute_method = impute_method, 
                                     complete_case = complete_case, 
                                     encode_categorical = encode_categorical, 
                                     reduce_dimension = reduce_dimension, 
                                     num_components = num_components, 
                                     feature_selection_method = feature_selection_method
                                     )

# Model Selection
if (!is.null(model_selection) & model_selection != "NULL") {
  if (model_selection == "knn") {
    # kNN model
    knn_result <- nested_cv_knn(preprocessed_data$processed_data, class_labels = preprocessed_data$label, k_outer = 5, k_inner = 5)
    
    cat("kNN model training finished:\n")
    cat("Average Accuracy:", knn_result$mean_accuracy, "\n")
    for(i in 1:length(knn_result$mean_auc_per_class))
      cat("Average AUC for", names(knn_result$mean_auc_per_class)[i], ":", knn_result$mean_auc_per_class[i], "\n")
    # save model
    saveRDS(knn_result, file = output_model)
    # write log file
    fileConn<-file(output_log, open = 'a') # write in 'append' mode
    writeLines(c("kNN model training finished:"), fileConn)
    writeLines(paste("Average Accuracy:", knn_result$mean_accuracy), fileConn)
    for(i in 1:length(knn_result$mean_auc_per_class))
      writeLines(paste("Average AUC for", names(knn_result$mean_auc_per_class)[i], ":", knn_result$mean_auc_per_class[i]), fileConn)
    close(fileConn)
    
  } else if (model_selection == "binomial_logistic") {
      # binomial logistic regression
    bi_logistic_result <- nested_cv_logistic(preprocessed_data$processed_data, class_labels = preprocessed_data$label, 
                                             case = positive_outcome, k_outer = 5, k_inner = 5) 
    cat("Binomial logistic regression model training finished:\n")
    cat("Average Accuracy:", bi_logistic_result$mean_accuracy, "\n")
    cat("Average AUC:", bi_logistic_result$mean_auc, "\n")
    
    # save model
    saveRDS(bi_logistic_result, file = output_model)
    
    # write log file
    fileConn<-file(output_log, open = 'a')
    writeLines(c("Binomial logistic regression model training finished:"), fileConn)
    writeLines(paste("Average Accuracy:", bi_logistic_result$mean_accuracy), fileConn)
    writeLines(paste("Average AUC:", bi_logistic_result$mean_auc), fileConn)
    close(fileConn)
    
    } else if (model_selection == "multinomial_logistic") {
      # Multinomial logistic regression
      multi_logistic_result <- nested_cv_multilogistic(preprocessed_data$processed_data, class_labels = preprocessed_data$label, 
                                                       k_outer = 5, k_inner = 5)
      cat("Multinomial logistic regression model training finished:\n")
      cat("Average Accuracy:", multi_logistic_result$mean_accuracy, "\n")
      cat("Average multi-class AUC:", multi_logistic_result$mean_multiAUC, "\n")
      
      # save model
      saveRDS(multi_logistic_result, file = output_model)
      
      # write log file
      fileConn<-file(output_log, open = 'a')
      writeLines(c("Multinomial logistic regression model training finished:"), fileConn)
      writeLines(paste("Average Accuracy:", multi_logistic_result$mean_accuracy), fileConn)
      writeLines(paste("Average multi-class AUC:", multi_logistic_result$mean_multiAUC), fileConn)
      close(fileConn)
      
    }
  else {
    stop("Invalid model selection. Use 'knn', 'binomial_logistic', or 'multinomial_logistic'.")
  }
}


