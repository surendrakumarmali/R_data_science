###########################################################
# Anomaly detection
# Written by Hari
# Last updated: 17082017
###########################################################

# install.packages("devtools")
# install.packages("AnomalyDetection)
devtools::install_github("twitter/AnomalyDetection")


library(AnomalyDetection)
help(AnomalyDetectionTs)
help(AnomalyDetectionVec)


data(raw_data)
View(raw_data)
res = AnomalyDetectionTs(raw_data, max_anoms = 0.02, direction = 'both', plot = TRUE)

res$plot

names(res)
