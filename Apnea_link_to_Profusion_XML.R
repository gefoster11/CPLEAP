#this function takes a edf file from apnealink and generates a compumedics-like XML file to be used for running apnealink data through PUPbeta to determine loop gain from apnealink PSG devices.#
#The function requires the directory to the "compumedics_xml_template.xml" file and the location of the apnealink edf file (al_edf).

####eg.  apnealink_2_compumedics(temp = "/Users/foster10/Box Sync/CPLEAP/CPLEAP/compumedics_xml_template.XML", al_edf = "/Users/foster10/Box Sync/CPLEAP/CPLEAP/Kalker_A_2018-05-28 5160m.EDF")

####Debugging####
#temp <- "/Users/foster10/Box Sync/CPLEAP/CPLEAP/compumedics_xml_template.XML"
#path <- file.choose()
#al_edf <- path

####Batch Analysis####
library(tidyverse)

file_list <- list.files(pattern = ".EDF")

file_list %>%
  map(., apnealink_2_compumedics, temp = "compumedics_xml_template.XML")

####Generate file list####
file_names <- file_list[c(TRUE, FALSE)] %>%
  map(., tools::file_path_sans_ext) %>%
  unlist() %>%
  as.data.frame() %>%
  write_csv(., path = "file_names")


apnealink_2_compumedics <- function(temp, al_edf) {

library(xml2)
library(edf)
library(lubridate)
library(tidyverse)
library(ggplot2)

####Load Compumedics Template XML File####
xmldoc <- read_xml(temp)
profusion_data <- as_list(xmldoc) ##converts xml to a list use as_xml_document to coerce a R list to xml nodes.

####Load apnea link EDF file####
edfdoc <- read.edf(al_edf)

####-Generate Sleep Epoch Data-####
duration_s <- as.duration(edfdoc$header.global$timestamp.stop-edfdoc$header.global$timestamp.start)
num_epoch <- as.numeric(duration_s/30)
SleepStages <- rep(list(SleepStage = list("1")), num_epoch)

##Replace first 30 minutes of file with wakefulness
for (i in 1:60) {
  SleepStages[[i]] <- list("0")
}
profusion_data$CMPStudyConfig$SleepStages <- SleepStages  ##Replaces SleepStages in profusion xml file with epoch length appropriate sleep stages

####Generate ScoredEvent Data####

#quick plot
# [1] "Flow"             "Snoring"          "Effort"           "Pulse"           
# [5] "Saturation"       "Recording_interr" "Start_of_evaluat" "End_of_evaluatio"
# [9] "Signal_too_small" "Unclassified_apn" "Hypopnea"         "Flow_limitation" 
# [13] "Snoring"          "Analysis_exclusi" "Flowlimitation__" "Cheyne_Stokes_Re"
# [17] "Missing_finger_s" "Missing_XPod"     "Invalid_data_XPo" "Invalid_data_bat"
# [21] "Invalid_data_flo" "Invalid_data_pul" "Invalid_data_sat" "Baseline_Saturat"
# [25] "Desaturation"     "Analysis_exclusi" "Start_of_evaluat" "Start_of_evaluat"
# [29] "Mixed_apnea"      "Central_apnea"    "Obstructive_apne" "Signal_too_small"
# [33] "Invalid_Data_Eff"

#i <- 31
#plot(edfdoc$signal[[i]]$t, edfdoc$signal[[i]]$data, type = "l")

# ####Central Apnea####
# state <- edfdoc$signal$Central_apnea$t[which(edfdoc$signal$Central_apnea$data != dplyr::lag(edfdoc$signal$Central_apnea$data))]  ##determines each state change to identify start and end of each event
# 
# if(length(state) >= 1){
# if (edfdoc$signal$Central_apnea$data[1] == 1) {
#   state <- c(edfdoc$signal$Central_apnea$t[1], state)}
# 
# 
# tStart <- state[c(TRUE, FALSE)]
# tEnd <- state[c(FALSE, TRUE)]
# evDuration <- round(tEnd-tStart, 1)
# 
# ScoredEvents <- list()
# for (i in 1:length(tStart)) {
#   #i <- 1
#   Name <- list("Central Apnea")
#   Start <- list(paste0(tStart[[i]]))
#   Duration <- list((paste0(evDuration[[i]])))
#   Input <- list("Flow")
#   tmp <- list(ScoredEvent = list(Name = Name, Start = Start, Duration = Duration, Input = Input))
#   ScoredEvents <- append(ScoredEvents, tmp)
# }
# }

ScoredEvents <- list()

####Central Apnea####
state <- edfdoc$signal$Central_apnea$t[which(edfdoc$signal$Central_apnea$data != dplyr::lag(edfdoc$signal$Central_apnea$data))]  ##determines each state change to identify start and end of each event

if(length(state) > 1){
  if (edfdoc$signal$Central_apnea$data[1] == 1) {
    state <- c(edfdoc$signal$Central_apnea$t[1], state)}
}
  
  if(length(state > 0)) {
    tStart <- state[c(TRUE, FALSE)]
    tEnd <- state[c(FALSE, TRUE)]
    evDuration <- round(tEnd-tStart, 1)
    
    for (i in 1:length(tStart)) {
      #i <- 1
      Name <- list("Central Apnea")
      Start <- list(paste0(tStart[[i]]))
      Duration <- list((paste0(evDuration[[i]])))
      Input <- list("Flow")
      tmp <- list(ScoredEvent = list(Name = Name, Start = Start, Duration = Duration, Input = Input))
      ScoredEvents <- append(ScoredEvents, tmp)
    }
  }



####Obstructive Apnea####
state <- edfdoc$signal$Obstructive_apnea$t[which(edfdoc$signal$Obstructive_apnea$data != dplyr::lag(edfdoc$signal$Obstructive_apnea$data))]  ##determines each state change to identify start and end of each event

if(length(state) > 1){
if (edfdoc$signal$Obstructive_apnea$data[1] == 1) {
  state <- c(edfdoc$signal$Obstructive_apnea$t[1], state)}
}

if(length(state > 0)) {
  tStart <- state[c(TRUE, FALSE)]
  tEnd <- state[c(FALSE, TRUE)]
  evDuration <- round(tEnd-tStart, 1)
  
  for (i in 1:length(tStart)) {
    #i <- 1
    Name <- list("Obstructive Apnea")
    Start <- list(paste0(tStart[[i]]))
    Duration <- list((paste0(evDuration[[i]])))
    Input <- list("Flow")
    tmp <- list(ScoredEvent = list(Name = Name, Start = Start, Duration = Duration, Input = Input))
    ScoredEvents <- append(ScoredEvents, tmp)
  }
}

  
  
####Mixed Apnea####
state <- edfdoc$signal$Mixed_apnea$t[which(edfdoc$signal$Mixed_apnea$data != dplyr::lag(edfdoc$signal$Mixed_apnea$data))]  ##determines each state change to identify start and end of each event

if(length(state) > 1){
if (edfdoc$signal$Mixed_apnea$data[1] == 1) {
  state <- c(edfdoc$signal$Mixed_apnea$t[1], state)}
}

if(length(state > 0)) {
  tStart <- state[c(TRUE, FALSE)]
  tEnd <- state[c(FALSE, TRUE)]
  evDuration <- round(tEnd-tStart, 1)
  
  for (i in 1:length(tStart)) {
    #i <- 1
    Name <- list("Mixed Apnea")
    Start <- list(paste0(tStart[[i]]))
    Duration <- list((paste0(evDuration[[i]])))
    Input <- list("Flow")
    tmp <- list(ScoredEvent = list(Name = Name, Start = Start, Duration = Duration, Input = Input))
    ScoredEvents <- append(ScoredEvents, tmp)
  }
}

####Respiratory artifact####
state <- edfdoc$signal$Invalid_data_pul$t[which(edfdoc$signal$Invalid_data_pul$data != dplyr::lag(edfdoc$signal$Invalid_data_pul$data))]  ##determines each state change to identify start and end of each event

if(length(state) > 1){
if (edfdoc$signal$Invalid_data_pul$data[1] == 1) {
  state <- c(edfdoc$signal$Invalid_data_pul$t[1], state)
}
}

if(length(state > 0)) {
  tStart <- state[c(TRUE, FALSE)]
  tEnd <- state[c(FALSE, TRUE)]
  evDuration <- round(tEnd-tStart, 1)
  
  for (i in 1:length(tStart)) {
    #i <- 1
    Name <- list("Respiratory artifact")
    Start <- list(paste0(tStart[[i]]))
    Duration <- list((paste0(evDuration[[i]])))
    Input <- list("SpO2")
    tmp <- list(ScoredEvent = list(Name = Name, Start = Start, Duration = Duration, Input = Input))
    ScoredEvents <- append(ScoredEvents, tmp)
  }
}

####SpO2 artifact####
state <- edfdoc$signal$Invalid_data_sat$t[which(edfdoc$signal$Invalid_data_sat$data != dplyr::lag(edfdoc$signal$Invalid_data_sat$data))]  ##determines each state change to identify start and end of each event
if(length(state) > 1){
if (edfdoc$signal$Invalid_data_sat$data[1] == 1) {
  state <- c(edfdoc$signal$Invalid_data_sat$t[1], state)
}
}

if(length(state > 0)) {
  tStart <- state[c(TRUE, FALSE)]
  tEnd <- state[c(FALSE, TRUE)]
  evDuration <- round(tEnd-tStart, 1)
  
  for (i in 1:length(tStart)) {
    #i <- 1
    Name <- list("SpO2 artifact")
    Start <- list(paste0(tStart[[i]]))
    Duration <- list((paste0(evDuration[[i]])))
    Input <- list("SpO2")
    tmp <- list(ScoredEvent = list(Name = Name, Start = Start, Duration = Duration, Input = Input))
    ScoredEvents <- append(ScoredEvents, tmp)
  }
}

####Hypopnea####
state <- edfdoc$signal$Hypopnea$t[which(edfdoc$signal$Hypopnea$data != dplyr::lag(edfdoc$signal$Hypopnea$data))]  ##determines each state change to identify start and end of each event
if(length(state) > 1){
if (edfdoc$signal$Hypopnea$data[1] == 1) {
  state <- c(edfdoc$signal$Hypopnea$t[1], state)
}
}

if(length(state > 0)) {
  tStart <- state[c(TRUE, FALSE)]
  tEnd <- state[c(FALSE, TRUE)]
  evDuration <- round(tEnd-tStart, 1)
  
  for (i in 1:length(tStart)) {
    #i <- 1
    Name <- list("Hypopnea")
    Start <- list(paste0(tStart[[i]]))
    Duration <- list((paste0(evDuration[[i]])))
    Input <- list("Flow")
    tmp <- list(ScoredEvent = list(Name = Name, Start = Start, Duration = Duration, Input = Input))
    ScoredEvents <- append(ScoredEvents, tmp)
  }
}

####Desaturation####
state <- edfdoc$signal$Desaturation$t[which(edfdoc$signal$Desaturation$data != dplyr::lag(edfdoc$signal$Desaturation$data))]  ##determines each state change to identify start and end of each event

if(length(state) > 1){
if (edfdoc$signal$Desaturation$data[1] == 1) {
  state <- c(edfdoc$signal$Desaturation$t[1], state)
}
}
  
if(length(state > 0)) {
  tStart <- state[c(TRUE, FALSE)]
  tEnd <- state[c(FALSE, TRUE)]
  evDuration <- round(tEnd-tStart, 1)
  
  for (i in 1:length(tStart)) {
    #i <- 1
    LowestSpO2 <- list(min(edfdoc$signal$Saturation$data[edfdoc$signal$Saturation$t[edfdoc$signal$Saturation$t >= round(tStart[[i]]) & edfdoc$signal$Saturation$t <= round(tEnd[[i]])]]))
    Desaturation <- list(max(edfdoc$signal$Saturation$data[edfdoc$signal$Saturation$t[edfdoc$signal$Saturation$t >= round(tStart[[i]]) & edfdoc$signal$Saturation$t <= round(tEnd[[i]])]])-min(edfdoc$signal$Saturation$data[edfdoc$signal$Saturation$t[edfdoc$signal$Saturation$t >= round(tStart[[i]]) & edfdoc$signal$Saturation$t <= round(tEnd[[i]])]]))
    Name <- list("SpO2 desaturation")
    Start <- list(paste0(tStart[[i]]))
    Duration <- list((paste0(evDuration[[i]])))
    Input <- list("SpO2")
    tmp <- list(ScoredEvent = list(LowestSpO2 = LowestSpO2, Desaturation = Desaturation, Name = Name, Start = Start, Duration = Duration, Input = Input))
    ScoredEvents <- append(ScoredEvents, tmp)
  }
}

####Insert Scored Events into New XML####
profusion_data$CMPStudyConfig$ScoredEvents <- ScoredEvents  ##Replaces ScoredEvents in profusion xml file with epoch length appropriate Scored Events
new_xml <- as_xml_document(profusion_data)  ##Generate the new_xml file with new ScoredEvents

####Save Output####
write_xml(new_xml, file = paste0(al_edf, ".XML"))
}