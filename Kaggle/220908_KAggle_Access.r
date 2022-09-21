# https://medium.com/mcd-unison/how-to-use-kaggle-api-to-download-datasets-in-r-312179c7a99c
# Access Kaggle databases from R

setwd("C:/Users/DSEP0001/Github/R-Experiments/Kaggle")

install.packages(c("devtools"))
devtools::install_github("ldurazo/kaggler")

library(readr)
library(kaggler)
kgl_auth(creds_file = 'kaggle.json')

# Example 1
# https://www.kaggle.com/datasets/andrewmvd/violence-against-women-and-girls
response <- kgl_datasets_download_all(owner_dataset = "andrewmvd/violence-against-women-and-girls")

download.file(response[["url"]], "data/temp.zip", mode="wb")
unzip_result <- unzip("data/temp.zip", exdir = "data/", overwrite = TRUE)
violence_data <- read_csv("data/makeovermonday-2020w10/violence_data.csv")

<<<<<<< HEAD
# Example 2
# https://www.kaggle.com/datasets/shivan118/healthcare-analytics
response <- kgl_datasets_download_all(owner_dataset = "shivan118/healthcare-analytics")

download.file(response[["url"]], "data/temp.zip", mode="wb")
unzip_result <- unzip("data/temp.zip", exdir = "data/", overwrite = TRUE)
patient_data <- read_csv("data/Train/Patient_Profile.csv")

# Example 3 - Fetal Health Classification
# https://www.kaggle.com/datasets/andrewmvd/fetal-health-classification
response <- kgl_datasets_download_all(owner_dataset = "andrewmvd/fetal-health-classification")
download.file(response[["url"]], "data/temp.zip", mode="wb")
unzip_result <- unzip("data/temp.zip", exdir = "data/", overwrite = TRUE)
fetal_data <- read_csv("data/fetal_health.csv")

=======
########################################################
#https://www.kaggle.com/datasets/kanikakhera/healthcare-dataset?select=Transaction_coo.csv
response <- kgl_datasets_download_all(owner_dataset = "kanikakhera/healthcare-dataset")

download.file(response[["url"]], "data/temp.zip", mode="wb")
unzip_result <- unzip("data/temp.zip", exdir = "data/", overwrite = TRUE)
Inpatient_data <- read_csv("data/Inpatient_Pat.csv")
Transactions <- read_csv("data/Transaction_coo.csv")
Patients <- read_csv("data/Patient_history_samp.csv")

########################################################
#https://www.kaggle.com/datasets/andrewmvd/fetal-health-classification
response <- kgl_datasets_download_all(owner_dataset = "andrewmvd/fetal-health-classification")

download.file(response[["url"]], "data/temp.zip", mode="wb")
unzip_result <- unzip("data/temp.zip", exdir = "data/", overwrite = TRUE)
fetal_health <- read_csv("data/fetal_health.csv")

########################################################
#https://www.kaggle.com/datasets/andrewmvd/fetal-health-classification
response <- kgl_datasets_download_all(owner_dataset = "andrewmvd/heart-failure-clinical-data")

download.file(response[["url"]], "data/temp.zip", mode="wb")
unzip_result <- unzip("data/temp.zip", exdir = "data/", overwrite = TRUE)
data_heart <- read_csv("data/heart_failure_clinical_records_dataset.csv")

########################################################
#https://www.kaggle.com/datasets/andrewmvd/fetal-health-classification
response <- kgl_datasets_download_all(owner_dataset = "homayoonkhadivi/ai-for-medical-prognosis-diabetes-datasets")

download.file(response[["url"]], "data/temp.zip", mode="wb")
unzip_result <- unzip("data/temp.zip", exdir = "data/", overwrite = TRUE)
X_diabetes <- read_csv("data/X_data.csv")
y_diabetes <- read_csv("data/y_data.csv")

########################################################
#https://www.kaggle.com/datasets/andrewmvd/fetal-health-classification
response <- kgl_datasets_download_all(owner_dataset = "fedesoriano/body-fat-prediction-dataset")

download.file(response[["url"]], "data/temp.zip", mode="wb")
unzip_result <- unzip("data/temp.zip", exdir = "data/", overwrite = TRUE)
data_bodyfat <- read_csv("data/bodyfat.csv")

# https://www.kaggle.com/datasets/csafrit2/maternal-health-risk-data
>>>>>>> 8ea1c6c1a73054433c8a5ffdc21b878c8cbaf8a3

