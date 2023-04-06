library(tidyverse)
library(data.table) ## For the fread function
library(lubridate)
library(tictoc)
library(googledrive)

source("sepsis_monitor_functions.R")


tic()
makeSepsisDataset(50, "fread")
toc()
#6.781 sec elapsed

tic()
makeSepsisDataset(100, "fread")
toc()
#11.62 sec elapsed

tic()
makeSepsisDataset(500, "fread")
toc()
#57.1 sec elapsed

tic()
makeSepsisDataset(50, "read_delim")
toc()
#19.344 sec elapsed

tic()
makeSepsisDataset(100, "read_delim")
toc()
#37.924 sec elapsed

tic()
makeSepsisDataset(500, "read_delim")
toc()
#191 sec elapsed

######## task 3 

df <- makeSepsisDataset()

# We have to write the file to disk first, then upload it
df %>% write_csv("sepsis_data_temp.csv")

# Uploading happens here
sepsis_file <- drive_put(media = "sepsis_data_temp.csv", 
                         path = "https://drive.google.com/drive/folders/1rbs2z9hziDtw7TxFmxd8QT-n4C4L0HTM",
                         name = "sepsis_data.csv")

# Set the file permissions so anyone can download this file.
sepsis_file %>% drive_share_anyone()

## Calling drive_deauth() prevents R from trying to authenticate via a browser
## This is needed to make the GitHub Action work
drive_deauth()
file_link <- "https://drive.google.com/file/d/1-BqFL73u2z4Uq-bGZbmIfrpATnNr4ymu/view?usp=share_link"

## All data up until now
new_data <- updateData(file_link)

## Include only most recent data
most_recent_data <- new_data %>%
  group_by(PatientID) %>%
  filter(obsTime == max(obsTime))


# code for figure 1

sepsis_plots <- lapply(unique(sepsis_pts$PatientID), function(patient){
  patient_data <- subset(sepsis_pts, PatientID == patient)
  ggplot(data = patient_data, aes(x = obsTime)) +
    geom_point(data = subset(patient_data, !is.na(HR)), aes(y = HR, colour = "HR")) +
    geom_point(data = subset(patient_data, !is.na(Temp)), aes(y = Temp, colour = "Temp")) +
    geom_point(data = subset(patient_data, !is.na(Resp)), aes(y = Resp, colour = "Resp")) +
    labs(x = "Observation Time", y = "HR = beats/minute, Resp = breaths/minute, Temp = ºC", colour = "Vital Sign") +
    scale_x_datetime(date_labels = "%m/%d/%Y %H:%M") + 
    ggtitle(paste("Patient", patient))
})
sepsis_plots

sepsis_pts <- subset(new_data, SepsisLabel==1)
sepsis_plots <- lapply(unique(sepsis_pts$PatientID), function(patient){
  patient_data <- subset(sepsis_pts, PatientID == patient)
  ggplot(data = patient_data, aes(x = obsTime)) +
    geom_line(data = subset(patient_data, !is.na(HR)), aes(y = HR, colour = "HR")) +
    geom_line(data = subset(patient_data, !is.na(Temp)), aes(y = Temp, colour = "Temp")) +
    geom_line(data = subset(patient_data, !is.na(Resp)), aes(y = Resp, colour = "Resp")) +
    labs(x = "Observation Time", y = "HR = beats/minute, Resp = breaths/minute, Temp = ºC", colour = "Vital Sign") +
    scale_x_datetime(date_labels = "%m/%d/%Y %H:%M") + 
    ggtitle(paste("Patient", patient))
})
sepsis_plots


#table 2 
sepsis_pts <- subset(new_data, SepsisLabel==1)

sepsis_pts %>%
  group_by(PatientID) %>%
  arrange(obsTime) %>%
  mutate(prevHR = lag(HR), prevTemp = lag(Temp), prevResp = lag(Resp)) %>%
  filter(!is.na(HR)) %>%
  summarise(
    `Change in Heart Rate (bpm)` = last(HR) - last(prevHR),
    `Change in Temp (ºC)` = last(Temp) - last(prevTemp),
    `Change in Resp Rate (rpm)` = last(Resp) - last(prevResp)
  ) %>%
  kable()


