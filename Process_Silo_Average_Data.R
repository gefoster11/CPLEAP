#Load Silo Average Data
library(dplyr)

ID <- "NVT29"
exp_condition <- "control"
stage <- c("Baseline", "0_LBNP", "15_LBNP", "30_LBNP", "45_LBNP")
t_interval <- 15 #the time interval in which your data was outputted from Silo
ave_interval <- 60 # the duration you wish your output to average over.
f_out <- "NVT_29.csv" #Name of output file.  It will be save to the 'pre_file' path.

#Load Diameter Summary File
pre_diameter_file <- file.choose()
pre_diameter_data <- read.csv(pre_diameter_file, header = TRUE)

post_diameter_file <- file.choose()
post_diameter_data <- read.csv(post_diameter_file, header = TRUE)

diameter_data <- rbind(pre_diameter_data, post_diameter_data)

#PRE- Load DATA
pre_file <- file.choose()
pre_data <- read.csv(pre_file, header = TRUE)

#PRE - provide the desired start times of your intervals
pre_t_baseline <- 7252.103108
pre_t_0 <- 7794.572927
pre_t_15 <- 8039.196806
pre_t_30 <- 8255.402579
pre_t_45 <- 8461.927909
  
pre_time <- c(pre_t_baseline, pre_t_0, pre_t_15, pre_t_30, pre_t_45) #generates your time vector

# PRE - calculate Burst frequency, Burst Incidencem and total MSNA if you have MSNA data
pre_data <- mutate(pre_data, burst_frequency = nBursts*(60/t_interval), burst_incidence = (burst_frequency/HR_mean)*100, total_MSNA = MSNA_Amplitude_mean*burst_frequency, total_normalized_MSNA = MSNA_Normalized_Amplitude_mean*burst_frequency)

#PRE - Generate Requested Data Averages
condition <- "PRE"
pre_data_stage <- NULL

for(i in 1:length(stage)) {
rbind(pre_data_stage, summarise_at(filter(pre_data, Time_mean >= pre_time[i] & Time_mean < pre_time[i] + ave_interval), c("Time_mean", "SPO2_mean", "SBP_mean", "DBP_mean", "MAP_mean", "HR_mean", "SV_mean", "CO_mean", "TPR_mean", "V_ba_mean", "Q_BA_mean", "FVR_mean", "FVC_mean", "VI_mean", "VT_mean", "Fb_mean", "PO2_mean", "PCO2_mean", "Box.Pressure_mean", "MSNA_Baseline_mean", "MSNA_Amplitude_mean", "MSNA_Normalized_Amplitude_mean", "burst_frequency", "burst_incidence", "total_MSNA", "total_normalized_MSNA"), mean, na.rm = TRUE)) -> pre_data_stage
  }

pre_data_stage_2 <- NULL
for(i in 1:length(stage)) {
  rbind(pre_data_stage_2, summarise_at(filter(pre_data, Time_mean >= pre_time[i] & Time_mean < pre_time[i] + ave_interval), c("nBreaths", "nBeats", "nBursts"), sum, na.rm = TRUE)) -> pre_data_stage_2
}

pre_data_stage <- cbind(ID, exp_condition, condition, stage, pre_data_stage, pre_data_stage_2)

#correction of mean pull for "V_ba_mean", "Q_BA_mean", "FVR_mean", "FVC_mean"
#replace_data <- summarise_at(filter(pre_data, Time_mean >= pre_t_45-60 & Time_mean < pre_t_45-60 + ave_interval), c("V_ba_mean", "Q_BA_mean", "FVR_mean", "FVC_mean"), mean, na.rm = TRUE)
#pre_data_stage[stage == "45_LBNP", 13:16] <- replace_data


#POST
#POST- Load DATA
post_file <- file.choose()
post_data <- read.csv(post_file, header = TRUE)

#POST - provide the desired start times of your intervals
post_t_baseline <- 11301.8079
post_t_0 <- 11438.02105
post_t_15 <- 11663.94034
post_t_30 <- 11879.46026
post_t_45 <- 12079.16884

post_time <- c(post_t_baseline, post_t_0, post_t_15, post_t_30, post_t_45) #generates your time vector

# POST - calculate Burst frequency, Burst Incidencem and total MSNA if you have MSNA data
post_data <- mutate(post_data, burst_frequency = nBursts*(60/t_interval), burst_incidence = (burst_frequency/HR_mean)*100, total_MSNA = MSNA_Amplitude_mean*burst_frequency, total_normalized_MSNA = MSNA_Normalized_Amplitude_mean*burst_frequency)

#POST - Generate Requested Data Averages
condition <- "POST"
post_data_stage <- NULL

for(i in 1:length(stage)) {
  rbind(post_data_stage, summarise_at(filter(post_data, Time_mean >= post_time[i] & Time_mean < post_time[i] + ave_interval), c("Time_mean", "SPO2_mean", "SBP_mean", "DBP_mean", "MAP_mean", "HR_mean", "SV_mean", "CO_mean", "TPR_mean", "V_ba_mean", "Q_BA_mean", "FVR_mean", "FVC_mean", "VI_mean", "VT_mean", "Fb_mean", "PO2_mean", "PCO2_mean", "Box.Pressure_mean", "MSNA_Baseline_mean", "MSNA_Amplitude_mean", "MSNA_Normalized_Amplitude_mean", "burst_frequency", "burst_incidence", "total_MSNA", "total_normalized_MSNA"), mean, na.rm = TRUE)) -> post_data_stage
}

post_data_stage_2 <- NULL
for(i in 1:length(stage)) {
  rbind(post_data_stage_2, summarise_at(filter(post_data, Time_mean >= post_time[i] & Time_mean < post_time[i] + ave_interval), c("nBreaths", "nBeats", "nBursts"), sum, na.rm = TRUE)) -> post_data_stage_2
}

post_data_stage <- cbind(ID, exp_condition, condition, stage, post_data_stage, post_data_stage_2)


#COMBINED PRE AND POST AND SAVE DATA FILE
average_data_out <- rbind (pre_data_stage, post_data_stage)

average_data_out <- merge(average_data_out, diameter_data, by = c("ID", "condition", "stage"), all.x = TRUE) ##add summary diameter data

average_data_out$stage <- factor(average_data_out$stage, level = (c("Baseline", "0_LBNP", "15_LBNP", "30_LBNP", "45_LBNP")))

write.csv(average_data_out, file = paste(dirname(pre_file),"/",f_out, sep = ""), row.names = FALSE, quote = FALSE)


#PLOT FVR versus MSNA Burst Frequency
library(ggplot2)

var_list <- colnames(select(average_data_out, 5:29))

#Make Plots and save Regression Plots
plot_list = list()
for (i in 1:length(var_list)){
  p <- ggplot(average_data_out, aes_string(x = "burst_frequency", y = var_list[[i]], group = "condition", shape = "stage", colour = "condition", linetype = "condition")) +
    geom_point(size = 3) + 
    stat_smooth(method = "lm", colour = "black", se = FALSE)
  plot_list[[i]] = p
}

pdf(file = paste(dirname(pre_file),"/",ID, "_regressions", ".pdf", sep = ""))

for (i in 1:length(var_list)) {
  print(plot_list[[i]])
}
dev.off()

#Make and save Stage Plots
var_list <- colnames(select(average_data_out, 5:29))

plot_list = list()
for (i in 1:length(var_list)){
  p <- ggplot(average_data_out, aes_string(x = "stage", y = var_list[[i]], group = "condition", shape = "condition", linetype = "condition")) +
    geom_point(size = 3) +
    geom_line (size = 1)
  plot_list[[i]] = p
}

pdf(file = paste(dirname(pre_file),"/",ID, "_stage", ".pdf", sep = ""))

for (i in 1:length(var_list)) {
  print(plot_list[[i]])
}
dev.off()

           