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


