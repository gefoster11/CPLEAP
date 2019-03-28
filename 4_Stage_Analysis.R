#Load Silo Average Data
##This function will output a stage data analysis given a breath, beat, and burst file cleaned within Silo.  This function outputs data figures.
#Provide this function with (1) ID = subject ID, exp_condition = "IH" or "control", pre_time = vector of desired start bins for pre test, post_time = vector of desired start bins for post test.  The analysis produced 60s bins for the NVT study.

#eg. stage_analysis(ID = "NVT06", exp_condition = "IH", pre_time = c(154.7499, 893.4968307, 1126.523196, 1346.96465, 1561.606437), post_time = c(4281.206337, 4322.579026, 4544.593981, 4753.662013, 4969.894556))

#stage_analysis(ID = "NVT36", exp_condition = "control", pre_time = c(3883.613407,4459.21036,4652.000393,4842.302376,5033.081484), post_time = c(7541.040134,8489.420113,8675.798401,8862.106388,9093.193171))

stage_analysis <- function(ID, exp_condition, pre_time, post_time){
library(dplyr)
library(plotrix)
library(ggplot2)
  
#ID <- "NVT29"
#exp_condition <- "control"
#pre_time <- c(18213.66933, 18594.0897, 18810.47276, 19030.9285, 19247.95222) #PRE - provide the desired start times of your intervals
#post_time <- c(21996.29212+200, 22242.94987, 22457.17352, 22668.64418, 22884.29483) #POST - provide the desired start times of your intervals

stage <- c("Baseline", "0_LBNP", "15_LBNP", "30_LBNP", "45_LBNP")

#Load Diameter Summary File
print("select file directory containing diameter summary files")
setwd(dirname(file.choose()))
pre_diameter_data <- read.csv(file = paste(ID, "_diameter_summary_PRE", ".csv", sep = ""), header = TRUE)
post_diameter_data <- read.csv(file = paste(ID, "_diameter_summary_POST", ".csv", sep = ""), header = TRUE)

diameter_data <- rbind(pre_diameter_data, post_diameter_data)


#PRE- Load DATA
print("select file directory containing cleaned breath, beat, and burst files")
setwd(dirname(file.choose()))
pre_breath <- read.csv(file = paste0("breath_", ID, "_pre_", exp_condition, "-clean.csv"))
pre_beat <- read.csv(file = paste0("beat_", ID, "_pre_", exp_condition, "-clean.csv")) 
pre_burst <- read.csv(file = paste0("burst_", ID, "_pre_", exp_condition, "-clean.csv"))

#PRE - Generate Requested Data Averages for burst file
condition <- "PRE"
pre_burst_stage <- NULL

for(i in 1:length(stage)) {
rbind(pre_burst_stage, cbind(summarise_at(filter(pre_burst, Time >= pre_time[i] & Time < pre_time[i] + 60), c("MSNA_Baseline", "MSNA_Amplitude", "MSNA_Normalized_Amplitude"), funs(mean = mean, sem = std.error), na.rm = TRUE), length(pre_burst$Time[pre_burst$Time >= pre_time[i] & pre_burst$Time < pre_time[i] + 60]))) -> pre_burst_stage
}

names(pre_burst_stage)[7] <- "nbursts"


#PRE - Generate Requested Data Averages for breath file
pre_breath_stage <- NULL

for(i in 1:length(stage)) {
  rbind(pre_breath_stage, cbind(summarise_at(filter(pre_breath, Time >= pre_time[i] & Time < pre_time[i] + 60), c("VI", "VT", "Fb", "PO2", "PCO2", "Box.Pressure"), funs(mean = mean, sem = std.error), na.rm = TRUE), length(pre_breath$Time[pre_breath$Time >= pre_time[i] & pre_breath$Time < pre_time[i] + 60]))) -> pre_breath_stage
}

names(pre_breath_stage)[13] <- "nbreath"

#PRE - Generate Requested Data Averages for beat file
pre_beat_stage <- NULL

for(i in 1:length(stage)) {
  rbind(pre_beat_stage, cbind(summarise_at(filter(pre_beat, Time >= pre_time[i] & Time < pre_time[i] + 60), c("SPO2", "SBP", "DBP", "MAP", "HR", "SV", "CO", "TPR", "V_ba", "Q_BA", "FVR", "FVC"), funs(mean = mean, sem = std.error), na.rm = TRUE), length(pre_beat$Time[pre_beat$Time >= pre_time[i] & pre_beat$Time < pre_time[i] + 60]))) -> pre_beat_stage
}

names(pre_beat_stage)[25] <- "nbeat"

pre_beat_stage[1,9:12] <- c(NA, NA, NA, NA)
pre_beat_stage[1,21:24] <- c(NA, NA, NA, NA)

pre_stage <- cbind(ID, exp_condition, condition, stage, pre_breath_stage, pre_beat_stage, pre_burst_stage)

pre_stage <- mutate(pre_stage, VT_mean = VT_mean*-1, burst_frequency = nbursts, burst_incidence = (burst_frequency/HR_mean)*100, total_MSNA = MSNA_Amplitude_mean*burst_frequency, total_normalized_MSNA = MSNA_Normalized_Amplitude_mean*burst_frequency)

#POST- Load DATA
post_breath <- read.csv(file = paste0("breath_", ID, "_post_", exp_condition, "-clean.csv"))
post_beat <- read.csv(file = paste0("beat_", ID, "_post_", exp_condition, "-clean.csv"))
post_burst <- read.csv(file = paste0("burst_", ID, "_post_", exp_condition, "-clean.csv"))

#POST - Generate Requested Data Averages for burst file
condition <- "POST"
post_burst_stage <- NULL

for(i in 1:length(stage)) {
  rbind(post_burst_stage, cbind(summarise_at(filter(post_burst, Time >= post_time[i] & Time < post_time[i] + 60), c("MSNA_Baseline", "MSNA_Amplitude", "MSNA_Normalized_Amplitude"), funs(mean = mean, sem = std.error), na.rm = TRUE), length(post_burst$Time[post_burst$Time >= post_time[i] & post_burst$Time < post_time[i] + 60]))) -> post_burst_stage
}

names(post_burst_stage)[7] <- "nbursts"


#POST - Generate Requested Data Averages for breath file
post_breath_stage <- NULL

for(i in 1:length(stage)) {
  rbind(post_breath_stage, cbind(summarise_at(filter(post_breath, Time >= post_time[i] & Time < post_time[i] + 60), c("VI", "VT", "Fb", "PO2", "PCO2", "Box.Pressure"), funs(mean = mean, sem = std.error), na.rm = TRUE), length(post_breath$Time[post_breath$Time >= post_time[i] & post_breath$Time < post_time[i] + 60]))) -> post_breath_stage
}

names(post_breath_stage)[13] <- "nbreath"

#POST - Generate Requested Data Averages for beat file
post_beat_stage <- NULL

for(i in 1:length(stage)) {
  rbind(post_beat_stage, cbind(summarise_at(filter(post_beat, Time >= post_time[i] & Time < post_time[i] + 60), c("SPO2", "SBP", "DBP", "MAP", "HR", "SV", "CO", "TPR", "V_ba", "Q_BA", "FVR", "FVC"), funs(mean = mean, sem = std.error), na.rm = TRUE), length(post_beat$Time[post_beat$Time >= post_time[i] & post_beat$Time < post_time[i] + 60]))) -> post_beat_stage
}

names(post_beat_stage)[25] <- "nbeat"

post_beat_stage[1,9:12] <- c(NA, NA, NA, NA)
post_beat_stage[1,21:24] <- c(NA, NA, NA, NA)

post_stage <- cbind(ID, exp_condition, condition, stage, post_breath_stage, post_beat_stage, post_burst_stage)

post_stage <- mutate(post_stage, VT_mean = VT_mean*-1, burst_frequency = nbursts, burst_incidence = (burst_frequency/HR_mean)*100, total_MSNA = MSNA_Amplitude_mean*burst_frequency, total_normalized_MSNA = MSNA_Normalized_Amplitude_mean*burst_frequency)


#COMBINED PRE AND POST AND SAVE DATA FILE
all_stage_out <- rbind(pre_stage, post_stage)

all_stage_out <- merge(all_stage_out, diameter_data, by = c("ID", "condition", "stage"), all.x = TRUE) ##add summary diameter data

all_stage_out$stage <- factor(all_stage_out$stage, level = (c("Baseline", "0_LBNP", "15_LBNP", "30_LBNP", "45_LBNP")))

write.csv(all_stage_out, file = paste(ID, "_IH.csv", sep = ""), row.names = FALSE, quote = FALSE)


#PLOT FVR versus MSNA Burst Frequency
library(ggplot2)

var_list <- colnames(select(all_stage_out, VI_mean, VT_mean, Fb_mean, PO2_mean, PCO2_mean, Box.Pressure_mean, SPO2_mean, SBP_mean, DBP_mean, MAP_mean, HR_mean, SV_mean, CO_mean, TPR_mean, V_ba_mean, Q_BA_mean, FVR_mean, FVC_mean, MSNA_Baseline_mean, MSNA_Amplitude_mean, MSNA_Normalized_Amplitude_mean, burst_frequency, burst_incidence, total_MSNA, total_normalized_MSNA, diameter))

sem_list <- colnames(select(all_stage_out, VI_sem, VT_sem, Fb_sem, PO2_sem, PCO2_sem, Box.Pressure_sem, SPO2_sem, SBP_sem, DBP_sem, MAP_sem, HR_sem, SV_sem, CO_sem, TPR_sem, V_ba_sem, Q_BA_sem, FVR_sem, FVC_sem, MSNA_Baseline_sem, MSNA_Amplitude_sem, MSNA_Normalized_Amplitude_sem, burst_frequency, burst_incidence, total_MSNA, total_normalized_MSNA, diameter))

#Make Plots and save Regression Plots versus burst_frequency
plot_list = list()
for (i in 1:length(var_list)){

if(var_list[[i]] != sem_list[[i]]) {  ###if var_list and sem_list are not equal then run plot with error bars else run plot without error bars
  p <- ggplot(all_stage_out, aes_string(x = "burst_frequency", y = var_list[[i]], group = "condition", shape = "stage", colour = "condition", linetype = "condition")) +
    geom_point(size = 3) + 
    stat_smooth(method = "lm", colour = "black", se = FALSE) +
    geom_errorbar(aes_string(ymin = paste0(var_list[[i]], "-",sem_list[[i]]), ymax = paste0(var_list[[i]], "+",sem_list[[i]])))
  
} else {
  p <- ggplot(all_stage_out, aes_string(x = "burst_frequency", y = var_list[[i]], group = "condition", shape = "stage", colour = "condition", linetype = "condition")) +
    geom_point(size = 3) + 
    stat_smooth(method = "lm", colour = "black", se = FALSE)
} 
  
plot_list[[i]] = p
}

pdf(file = paste(ID, "_regressions", ".pdf", sep = ""))

for (i in 1:length(var_list)) {
  print(plot_list[[i]])
}
dev.off()

#Make Plots and save Regression Plots versus total normalized MSNA
plot_list = list()
for (i in 1:length(var_list)){
  if(var_list[[i]] != sem_list[[i]]) {  ###if var_list and sem_list are not equal then run plot with error bars else run plot without error bars
  p <- ggplot(all_stage_out, aes_string(x = "total_normalized_MSNA", y = var_list[[i]], group = "condition", shape = "stage", colour = "condition", linetype = "condition")) +
    geom_point(size = 3) + 
    stat_smooth(method = "lm", colour = "black", se = FALSE) +
    geom_errorbar(aes_string(ymin = paste0(var_list[[i]], "-",sem_list[[i]]), ymax = paste0(var_list[[i]], "+",sem_list[[i]])), width = 50)
  } else {
  p <- ggplot(all_stage_out, aes_string(x = "total_normalized_MSNA", y = var_list[[i]], group = "condition", shape = "stage", colour = "condition", linetype = "condition")) +
  geom_point(size = 3) + 
  stat_smooth(method = "lm", colour = "black", se = FALSE)
  }
  plot_list[[i]] = p
}

pdf(file = paste(ID, "_regressions_total_MSNA", ".pdf", sep = ""))

for (i in 1:length(var_list)) {
  print(plot_list[[i]])
}
dev.off()

#Make and save Stage Plots
plot_list = list()
for (i in 1:length(var_list)){
  if(var_list[[i]] != sem_list[[i]]) {  ###if var_list and sem_list are not equal then run plot with error bars else run plot without error bars
    p <- ggplot(all_stage_out, aes_string(x = "stage", y = var_list[[i]], group = "condition", shape = "condition", linetype = "condition")) +
      geom_point(size = 3) +
      geom_line (size = 1) +
      geom_errorbar(aes_string(ymin = paste0(var_list[[i]], "-",sem_list[[i]]), ymax = paste0(var_list[[i]], "+",sem_list[[i]])), width = 0.5)
      
  } else {
  p <- ggplot(all_stage_out, aes_string(x = "stage", y = var_list[[i]], group = "condition", shape = "condition", linetype = "condition")) +
    geom_point(size = 3) +
    geom_line (size = 1)
  }
  plot_list[[i]] = p
}

pdf(file = paste(ID, "_stage", ".pdf", sep = ""))

for (i in 1:length(var_list)) {
  print(plot_list[[i]])
}
dev.off()
}
