# https://medium.com/mcd-unison/how-to-use-kaggle-api-to-download-datasets-in-r-312179c7a99c
# Access Kaggle databases from R

install.packages(c("devtools"))
devtools::install_github("ldurazo/kaggler")
library(readr)
library(kaggler)
kgl_auth(creds_file = 'kaggle.json')
response <- kgl_datasets_download_all(owner_dataset = "andrewmvd/violence-against-women-and-girls")

download.file(response[["url"]], "data/temp.zip", mode="wb")
unzip_result <- unzip("data/temp.zip", exdir = "data/", overwrite = TRUE)
violence_data <- read_csv("data/makeovermonday-2020w10/violence_data.csv")

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

