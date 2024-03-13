# loading required packages
suppressPackageStartupMessages(library(caret, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(dplyr, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(pROC, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(class, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(ROCR, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(mice, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(tidymodels, warn.conflicts = FALSE))

# load, preprocess and normalize the data
# deal with missing values (complete case analysis or imputation, such as mean/median substitution, multiple imputation)
# encode categorical variables
# split the data into training and testing sets
preprocess_data <- function(data_path = data_path, label_path = label_path, normalize = FALSE, impute_method = NULL, complete_case = FALSE, encode_categorical = FALSE, reduce_dimension = FALSE, num_components = NULL, feature_selection_method = NULL) {
    # loading the data
    data <- read.csv(data_path, row.names = 1)  # Modify this based on your data format
  
    label <- read.csv(label_path, row.names = 1) # the true labels of the samples
    #label <- as.factor(label[,1]) # convert labels to factors
    
    # Handle missing values
    if (complete_case) {
        # Perform Complete Case Analysis (CCA)
        data <- data[complete.cases(data), ]
        # subset labels for retained samples
        label <- as.factor(label[rownames(data), 1])
    } else if (!is.null(impute_method) & impute_method != "NULL") {
        # Specify the imputation method
        if (impute_method == "mean") {
            # Perform mean imputation
            #imp_method <- mice::mice(data, method = "mean")
            #imputed_data <- mice::complete(imp_method)
            for (col in names(data)) {
                data[[col]][is.na(data[[col]])] <- mean(as.numeric(data[[col]]), na.rm = TRUE)
            }
        } else if (impute_method == "median") {
            # Perform median imputation
            for (col in names(data)) {
                data[[col]][is.na(data[[col]])] <- median(as.numeric(data[[col]]), na.rm = TRUE)
            }
        } else if (impute_method == "multiple"){
            # perform multiple imputation using default method 
            imp_method <- mice::mice(data, m = 5)
            data <- mice::complete(imp_method)
        } else {
            stop("Invalid imputation method. Use 'mean', 'median', 'multiple', or set it to NULL.")
        }
      # subset labels for retained samples
      label <- as.factor(label[rownames(data), 1])
      
    }

     # Automatically identify and encode categorical variables
    if (encode_categorical) {
        # Identify categorical variables
        categorical_columns <- sapply(data, function(x) is.factor(x) || is.character(x))

        # One-hot encoding using the recipes package
        recipe_obj <- recipe(~ ., data = data) %>%
          step_dummy(all_nominal(), one_hot = TRUE, -all_outcomes()) # automatically detect nominal (categorical) variables
        
        data <- bake(recipe_obj, new_data = data)
    }

    # Reduce dimension using PCA
    if (reduce_dimension) {
        if (is.null(num_components) | num_components == "NULL") {
            stop("Number of components for PCA must be specified.")
        }

        # Extract numerical columns for PCA
        numerical_data <- data[, sapply(data, is.numeric)]
        
        # Remove features with zero variance
        zero_var_cols <- sapply(numerical_data, function(x) var(x, na.rm = TRUE) == 0)
        numerical_data <- numerical_data[, !zero_var_cols, drop = FALSE]
        
        # Perform PCA
        pca_result <- prcomp(numerical_data, scale. = TRUE)

        # Retain specified number of components
        data <- data.frame(predict(pca_result, newdata = numerical_data)[, 1:num_components, drop = FALSE], data[, !sapply(data, is.numeric), drop = FALSE])
    }

    # Feature Selection
    if (!is.null(feature_selection_method) & feature_selection_method != "NULL") {
        if (feature_selection_method == "most_variable") {
            # Select the most variable features
            data <- data[, apply(data, 2, function(x) var(x, na.rm = TRUE)) > 0, drop = FALSE]
        } else if (feature_selection_method == "lasso" || feature_selection_method == "rfe") {
            set.seed(123)
            # Extract numerical columns for feature selection
            numerical_data <- data[, sapply(data, is.numeric)]

            # Perform feature selection using LASSO or RFE
            if (feature_selection_method == "lasso") {
                # LASSO feature selection
              if(length(levels(label)) == 2)
                lasso_model <- glmnet::cv.glmnet(as.matrix(numerical_data), label, alpha = 1)
              if(length(levels(label)) > 2)
                lasso_model <- glmnet::cv.glmnet(as.matrix(numerical_data), label, alpha = 1, family = "multinomial")
                # retrieve selected features
                selected_features <- predict(lasso_model, s = "lambda.min", type = "nonzero")
                selected_features <- unlist(selected_features)
            } else if (feature_selection_method == "rfe") {
                # Recursive Feature Elimination (RFE) using caret
                control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
                rfe_model <- rfe(numerical_data, label, sizes = c(1:ncol(numerical_data)), rfeControl = control)
                selected_features <- rfe_model$optVariables
            }
        # Confirm the number of selected features
        # Subset the data with selected features
        if(length(selected_features) > 0)
          data <- data[, c(selected_features, colnames(data)[!sapply(data, is.numeric)]), drop = FALSE]
        else
          cat("No feature is selected using the current method. May try another method.")
        } else {
        stop("Invalid feature selection method. Use 'most_variable', 'lasso', 'rfe', or set it to NULL.")
        }
    }

    # Normalize data
    if (normalize) {
        # Implement data normalization code (e.g., using scale())
        data <- scale(data)
    }
    
    return(list(processed_data = data, label = label))
}

###### kNN model ##########
nested_cv_knn <- function(data, class_labels, k_outer, k_inner) {
  set.seed(123)  # Set the seed for reproducibility
  outer_folds <- createFolds(class_labels, k = k_outer)
  
  unique_classes <- unique(class_labels)
  accuracy <- numeric(k_outer)
  auc_per_class <- vector("list", length = length(unique_classes))
  roc_per_class <- vector("list", length = length(unique_classes))
  models <- vector("list", length = k_outer)
  best_model <- NULL
  best_accuracy <- 0 # select the model with best accuracy as the best model
  
  for (i in 1:k_outer) {
    train_indices <- unlist(outer_folds[-i])  # Indices for the training data
    test_indices <- unlist(outer_folds[i])   # Indices for the test data
    
    train_data <- data[train_indices, ]
    train_labels <- class_labels[train_indices]
    test_data <- data[test_indices, ]
    test_labels <- class_labels[test_indices]
    
    # build knn model
    control <- trainControl(method="cv", number=5, savePredictions = TRUE,classProbs =  TRUE)
    metric <- "Accuracy"
    knn_model <- train(train_data, train_labels, method="knn", metric=metric, trControl=control, tuneGrid=expand.grid(k=seq(1,10,1)))
    models[[i]] <- knn_model
    
    # make prediction on the test set
    knn_pred <- predict(knn_model, newdata = test_data)
    
    # Calculate accuracy on the test set using confusionMatrix
    confusion_mat <- confusionMatrix(data = as.factor(knn_pred), reference = as.factor(test_labels))
    accuracy[i] <- confusion_mat$overall["Accuracy"]
    
    # Check if the current model is the best so far
    if (accuracy[i] > best_accuracy) {
      best_accuracy <- accuracy[i]
      best_model <- knn_model
    }
    
    # Calculate ROC curve and AUC for each class
    for (j in seq_along(unique_classes)) {
      class <- unique_classes[j]
      test_labels_numeric <- ifelse(test_labels == class, 1, 0)
      knn_pred_numeric <- ifelse(knn_pred == class, 1, 0)
      
      # Calculate ROC curve and AUC for the current class
      roc_per_class[[j]][[i]] <- roc(test_labels_numeric, knn_pred_numeric)
      auc_per_class[[j]][i] <- auc(roc_per_class[[j]][[i]])
    }
  }
  
  # Return mean AUC for each class
  mean_auc_per_class <- sapply(seq_along(unique_classes), function(j) {
    class_name <- unique_classes[j]
    class_mean_auc <- mean(sapply(auc_per_class[[j]], function(auc) as.numeric(auc)))
    names(class_mean_auc) <- class_name
    return(class_mean_auc)
  })
  
  return(list(mean_accuracy = mean(accuracy),
              mean_auc_per_class = mean_auc_per_class,
              best_model = best_model))
}

######## binomial logistic regression ############

# nested 5-fold cross validation
nested_cv_logistic <- function(data, class_labels, case = NULL, k_outer, k_inner) {
  if(is.null(case)){
    stop("For binomial logistic regression, please specify the label as the positive outcome. The other labels would be treated as negative outcomes.")
  } else{
    set.seed(123)  # Set the seed for reproducibility
    # encode class labels as case and control
    class_labels <- ifelse(class_labels == case, "Yes", "No")
    outer_folds <- createFolds(class_labels, k = k_outer)
  
    accuracy <- numeric(k_outer)
    auc <- numeric(k_outer)
    roc <- list(k_outer)
    models <- list(k_outer)
    best_model <- NULL
    best_accuracy <- 0
  
    for (i in 1:k_outer) {
      train_indices <- unlist(outer_folds[-i])  # Indices for the training data
      test_indices <- unlist(outer_folds[i])   # Indices for the test data
    
      train_data <- data[train_indices, ]
      train_labels <- class_labels[train_indices]
      test_data <- data[test_indices, ]
      test_labels <- class_labels[test_indices]
    
      # build logistic regression model
      control <- trainControl(method="cv", number=5, savePredictions = TRUE,classProbs =  TRUE)
      metric <- "Accuracy"
      # Define the grid of hyperparameters to search over (e.g., for C values)
      logistic_model <- train(train_data, train_labels, method="glm", metric=metric, trControl=control)
    
      # View the results and best hyperparameters
      #print(logistic_model)
    
      # Extract the best model with the selected hyperparameters
      best_logistic_model <- logistic_model$finalModel
      models[[i]] <- best_logistic_model
    
      # make prediction on the test set
      pred <- predict(best_logistic_model, newdata = data.frame(test_data), type = "response")
      logistic_pred_numeric <- ifelse(pred > 0.5, 1, 0)
      #logistic_pred_numeric <- factor(logistic_pred_numeric, levels=c(0, 1))
    
      # Calculate accuracy on the test set
      test_labels_numeric <- ifelse(grepl("Yes",test_labels), 1, 0)
      test_labels_numeric <- factor(test_labels_numeric, levels=c(0, 1))
      accuracy[i] <- mean(logistic_pred_numeric == test_labels_numeric)
    
      # Check if the current model is the best so far
      if (accuracy[i] > best_accuracy) {
        best_accuracy <- accuracy[i]
        best_model <- models[[i]]
      }
    
      # Calculate ROC curve and AUC
      roc[[i]] <- roc(test_labels_numeric, logistic_pred_numeric)
      auc[i] <- auc(roc[[i]])
    
  }
  
  return(list(mean_accuracy = mean(accuracy), 
              mean_auc = mean(auc),
#              roc = roc,
#              models = models,
              best_model = best_model
  ))
  }
}

###### multinomial logistic regression ###########
nested_cv_multilogistic <- function(data, class_labels, k_outer, k_inner) {
  suppressPackageStartupMessages(library(nnet))
  set.seed(123)  # Set the seed for reproducibility
  outer_folds <- createFolds(class_labels, k = k_outer)
  
  multi_roc <- list(k_outer)
  confusion_matrix_metrics <- list(k_outer)
  models <- list(k_outer)
  best_model <- NULL
  best_accuracy <- 0
  
  for (i in 1:k_outer) {
    train_indices <- unlist(outer_folds[-i])  # Indices for the training data
    test_indices <- unlist(outer_folds[i])   # Indices for the test data
    
    train_data <- data[train_indices, ]
    train_labels <- class_labels[train_indices]
    test_data <- data[test_indices, ]
    test_labels <- class_labels[test_indices]
    
    # build multinomial logistic regression model
    model <- multinom(train_labels ~ ., data = data.frame(train_data), MaxNWts = 2500, decay = 0.01)
    
    # View the results 
    #summary(model)
    models[[i]] <- model
    
    # make prediction on the test set
    pred <- predict(model, newdata = test_data, type = "probs") # type = 'probs' or 'class'
    # Convert probabilities to class predictions
    predicted_labels <- colnames(pred)[max.col(pred, "first")]
    # Create a confusion matrix
    # make sure both actual and predicted labels have all levels
    predicted_labels <- factor(predicted_labels, levels = c("group1", "group2", "group3"))
    confusion_matrix <- table(test_labels, predicted_labels)
    # Print the confusion matrix
    #print(confusion_matrix)
    # Compute confusion matrix metrics
    confusion_matrix_metrics[[i]] <- confusionMatrix(confusion_matrix)
    
    # Print accuracy, precision, recall, and F1 score
    #print(confusion_matrix_metrics)
    
    # Check if the current model is the best so far
    if (confusion_matrix_metrics[[i]]$overall["Accuracy"] > best_accuracy) {
      best_accuracy <- confusion_matrix_metrics[[i]]$overall["Accuracy"]
      best_model <- models[[i]]
    }
    
    # Compute ROC curve
    multi_roc[[i]] <- multiclass.roc(test_labels, pred)
    
    # # Plot ROC curves
    # # Use pretty colours:
    # pretty_colours <- c("#F8766D","#00BA38","#619CFF")
    # # Specify the different classes 
    # classes <- levels(class_labels)
    # # For each class
    # for (j in 1:3)
    # {
    #   # Define which observations belong to class[j]
    #   true_values <- ifelse(test_labels==classes[j],1,0)
    #   # Assess the performance of classifier for class[j]
    #   pred_class <- ROCR::prediction(pred[,j],true_values)
    #   perf <- performance(pred_class, "tpr", "fpr")
    #   if (j==1)
    #   {
    #     plot(perf,main="ROC Curve",col=pretty_colours[j]) 
    #   }
    #   else
    #   {
    #     plot(perf,main="ROC Curve",col=pretty_colours[j],add=TRUE) 
    #   }
    # }
  }
  
  # evaluation
  accuracy <- lapply(confusion_matrix_metrics, function(x) x$overall["Accuracy"])
  precision <- lapply(confusion_matrix_metrics, function(x) x$byClass[,"Precision"])
  recall <- lapply(confusion_matrix_metrics, function(x) x$byClass[,"Sensitivity"])
  F1_score <- lapply(confusion_matrix_metrics, function(x) x$byClass[,"F1"])
  multiAUC <- lapply(multi_roc, function(x) x$auc)
  
  mean_accuracy <- mean(unlist(accuracy))
  mean_precision <- rowMeans(data.frame(precision))
  mean_recall <- rowMeans(data.frame(recall), na.rm = TRUE)
  mean_F1 <- rowMeans(data.frame(F1_score), na.rm = TRUE)
  mean_multiAUC <- mean(unlist(multiAUC))
  
  return(list(mean_accuracy = mean_accuracy,
              mean_multiAUC = mean_multiAUC,
              best_model = best_model
  )) # get average accuracy, average multi-class AUC and best model
}

##########################################################

##### XGBoost

##########################################################


nested_cv_XGBoost <- function(data, class_labels, k_outer, k_inner) {
  suppressPackageStartupMessages(library(xgboost))
  set.seed(123)  # Set the seed for reproducibility
  outer_folds <- createFolds(class_labels, k = k_outer)
  
  multi_roc <- list(k_outer)
  confusion_matrix_metrics <- list(k_outer)
  models <- list(k_outer)
  
  for (i in 1:k_outer) {
    train_indices <- unlist(outer_folds[-i])  # Indices for the training data
    test_indices <- unlist(outer_folds[i])   # Indices for the test data
    
    train_data <- data[train_indices, ]
    train_labels <- class_labels[train_indices]
    test_data <- data[test_indices, ]
    test_labels <- class_labels[test_indices]
    
    # XGBoost
    xgb_model <- xgboost(data = as.matrix(train_data), label = as.integer(train_labels) - 1,
                         objective = "multi:softprob", num_class = length(levels(train_labels)),
                         eval_metric = "mlogloss", nrounds = 100)
    models[[i]] <- xgb_model
    
    # make prediction on the test set
    raw_scores <- predict(xgb_model, as.matrix(test_data))
    
    # 'raw_scores' is the vector of raw prediction scores from XGBoost
    softmax <- function(x) {
      exp(x) / sum(exp(x))
    }
    
    # Apply softmax to each row of raw scores
    predictions <- t(apply(matrix(raw_scores, ncol = length(levels(class_labels))), 1, softmax))
    colnames(predictions) <- levels(class_labels)
    
    # Convert predicted probabilities to class predictions
    predicted_labels <- colnames(predictions)[max.col(predictions, "first")]
    
    # Create a confusion matrix
    predicted_labels <- factor(predicted_labels, levels = c("group1", "group2", "group3"))
    confusion_matrix <- table(test_labels, predicted_labels)
    
    # Compute confusion matrix metrics
    confusion_matrix_metrics[[i]] <- confusionMatrix(confusion_matrix)
    
    # Print accuracy, precision, recall, and F1 score
    print(confusion_matrix_metrics)
    
    # Compute ROC curve
    multi_roc[[i]] <- multiclass.roc(test_labels, predictions)
    
    # Plot ROC curves
    # Use pretty colours:
    pretty_colours <- c("#F8766D","#00BA38","#619CFF")
    # Specify the different classes 
    classes <- levels(class_labels)
    # For each class
    for (j in 1:3)
    {
      # Define which observations belong to class[j]
      true_values <- ifelse(test_labels==classes[j],1,0)
      # Assess the performance of classifier for class[j]
      pred_class <- prediction(predictions[,j],true_values)
      perf <- performance(pred_class, "tpr", "fpr")
      if (j==1)
      {
        plot(perf,main="ROC Curve",col=pretty_colours[j]) 
      }
      else
      {
        plot(perf,main="ROC Curve",col=pretty_colours[j],add=TRUE) 
      }
    }
  }
  return(list(multi_roc = multi_roc,
              confusion_matrix_metrics = confusion_matrix_metrics,
              models = models)) 
}
