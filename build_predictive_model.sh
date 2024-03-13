#!/bin/bash

# Script Description
script_description="This script runs a predictive modeling pipeline using R. It preprocesses the data, performs kNN, binomial logistic regression, and multinomial logistic regression models using nested cross-validation."

# Example Usage
example_usage="1) Build predictive models from gene expression data using principle component analysis:
./build_predictive_model.sh -i sample_dataset/RNAseq_data.csv -l sample_dataset/labels.csv -o ./test_run/model.rds -p ./test_run/result.log -n TRUE -c TRUE -r TRUE -u 25 -g knn

2) Build predictive models using DNA mutation data with feature selection:
./build_predictive_model.sh -i sample_dataset/NGS_data.csv -l sample_dataset/labels.csv -o ./test_run/model.rds -p ./test_run/result.log -n TRUE -c TRUE -f rfe -g binomial_logistic -t group1

3) Build predictive models from DNA methylation data using imputation to deal with missing values:
./build_predictive_model.sh -i sample_dataset/Methylation_data.csv -l sample_dataset/labels.csv -o ./test_run/model.rds -p ./test_run/result.log -n TRUE -m mean -f rfe -g multinomial_logistic"

parameter_descriptions="
-i: Path to the input data file (in .csv format). Rows represent observations, and columns represent features. Example: sample_dataset/RNAseq_data.csv.

-l: Path to the label file (in .csv format). The first column contains sample names matching the input data file. The second column contains true labels. Example: sample_dataset/labels.csv.

-o: Path to save the predictive model.

-p: Path to save the log file including performance metrics.

-n: Normalize data (TRUE/FALSE). Standarize and scale the data during preprocessing. Normalization is recommended.

-m: Imputation method for missing values (NULL/mean/median/multiple). 
   - 'NULL': No imputation.
   - 'mean/median': Use mean/median values to substitute missing values.
   - 'multiple': Perform multiple imputation using the 'mice' R package.

-c: Perform complete case analysis (TRUE/FALSE). 
   - TRUE: Retain only samples with no missing values.
   - Imputation is automatically disabled when set to TRUE.

-e: Encode categorical variables (TRUE/FALSE). 
   - TRUE: Perform one-hot encoding for categorical features.
   - Categorical features are automatically detected if data types are 'character' or 'factor'.

-r: Reduce dimension using PCA (TRUE/FALSE). 
   - TRUE: Perform principle component analysis.

-u: Number of components for PCA (specify if -r is TRUE, otherwise NULL). 
   - Retain specified number of top principal components for downstream analysis.

-f: Feature selection method (NULL/most_variable/lasso/rfe). 
   - 'NULL': No feature selection.
   - 'most_variable': Remove features with zero variance.
   - 'lasso': Lasso regularization for feature selection.
   - 'rfe': Recursive Feature Elimination (RFE).
   
-g: Select a ML method (knn/binomial_logistic/multinomial_logistic).
   - 'knn': k nearest neighbor (kNN) model.
   - 'binomial_logistic': binomial logistic regression model with two outcomes. The positive outcome should be specified with the -t option.
   - 'multinomial_logistic': multinomial logistic regression model supporting more than two outcomes.
   
-t: The positive outcome label. This is for binomial logistic regression model. The outcome labels other than the specified positive label would be treated as the negative label.
"

# Default values
data_path=""
label_path=""
output_model="model.rds"
output_log="result.log"
normalize="FALSE"
impute_method="NULL"
complete_case="FALSE"
encode_categorical="FALSE"
reduce_dimension="FALSE"
num_components="NULL"
feature_selection_method="NULL"
model_selection="NULL"
positive_outcome="NULL"

# Parse command-line arguments
while getopts ":i:l:o:p:n:m:c:e:r:u:f:g:t:" opt; do
case $opt in
i) data_path="$OPTARG" ;;
l) label_path="$OPTARG" ;;
o) output_model="$OPTARG" ;;
p) output_log="$OPTARG" ;;
n) normalize="$OPTARG" ;;
m) impute_method="$OPTARG" ;;
c) complete_case="$OPTARG" ;;
e) encode_categorical="$OPTARG" ;;
r) reduce_dimension="$OPTARG" ;;
u) num_components="$OPTARG" ;;
f) feature_selection_method="$OPTARG" ;;
g) model_selection="$OPTARG" ;;
t) positive_outcome="$OPTARG" ;;
\?) echo "Invalid option: -$OPTARG" >&2
exit 1 ;;
:) echo "Option -$OPTARG requires an argument." >&2
exit 1 ;;
esac
done

# Check if data_path and label_path are empty
if [ -z "$data_path" ] || [ -z "$label_path" ]; then
  echo "Error: Both -i (data_path) and -l (label_path) options are required. Provide proper input files." >&2
  # Display script description, example usage, and parameter descriptions
  echo -e "$script_description\n"
  echo -e "Example Usage:\n$example_usage\n" 
  echo -e "Parameter Descriptions:\n$parameter_descriptions"
  exit 1
else
  # Run R script with parsed arguments
  R_main_script.r "$data_path" "$label_path" "$output_model" "$output_log" "$normalize" "$impute_method" "$complete_case" "$encode_categorical" "$reduce_dimension" "$num_components" "$feature_selection_method" "$model_selection" "$positive_outcome"

fi
