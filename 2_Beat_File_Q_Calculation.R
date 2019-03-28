## Load a beat file and a blood vessel diameter file and calculates a blood flow column from velocity within the beat file.  Diameter is applied on a beat-by-beat basis.
#ID = subject ID. eg. "NVT24"
#condition = "PRE" or "POST"
#time = a vector of the end-times associated with stage 0, 15, and 30. eg. time = c(7690.034461+60,8010.123878+60,8230.899836+60)
#diameter = a vector of the diameters for all four stages (0, 15, 30, and 45).  eg. diameter = c(0.33645, 0.33013, 0.33654, 0.33876)
#f_out the output file name.  eg. f_out = "beat_NVT24_pre_control.csv")


# PRE- beat_file_Q_calculation(ID = "NVT36", condition = "PRE", time = c(4459.21036 + 60, 4652.000393 + 60, 4842.302376 + 60), diameter = c(0.430242527, 0.430868095, 0.423268884, 0.422153326), f_out = "beat_NVT36_pre_control.csv")

#POST - beat_file_Q_calculation(ID = "NVT36", condition = "POST", time = c(7968.690134 + 60, 8675.798401 + 60, 8862.106388 + 60), diameter = c(0.414332778, 0.411212222, 0.407021099, 0.419442222), f_out = "beat_NVT36_post_control.csv")


beat_file_Q_calculation <- function(ID, condition, time, diameter, f_out){
  library(dplyr)
  
  #Load beat file (after applying any blood pressure correction (see Beat_File_BP_Correct.R))
  beat_in <- file.choose()
  beat <- read.csv(beat_in, header = TRUE)
  
  #Load Diameter summary file (after using the Diameter Summarize NVT Study.RMD script) 
  #Diameter analysis done differently by Troy...adjusting code to insert average diameter directly
  stage <- c("0_LBNP", "15_LBNP", "30_LBNP", "45_LBNP")
  diameter <- as.data.frame(cbind(ID, condition, stage, diameter))
  diameter$diameter <- as.numeric(as.character(diameter$diameter))
  
  time_stage <- c("t_0", "t_15", "t_30") 
  time <- as.data.frame(cbind(ID, condition, time_stage, time))
  time$time <- as.numeric(as.character(time$time))
  
  #calculate blood flow on a stage by stage basis
  beat_t0 <- filter(beat, Time <= subset(time, time_stage == "t_0", select = "time", drop = TRUE)) %>% mutate(., Q_BA = V_ba*(pi*(subset(diameter, stage == "0_LBNP", select = "diameter", drop = TRUE)/2)^2)*60)
  
  beat_t15 <- filter(beat, Time > subset(time, time_stage == "t_0", select = "time", drop = TRUE) & Time <= subset(time, time_stage == "t_15", select = "time", drop = TRUE)) %>% mutate(., Q_BA = V_ba*(pi*(subset(diameter, stage == "15_LBNP", select = "diameter", drop = TRUE)/2)^2)*60)
  
  beat_t30 <- filter(beat, Time > subset(time, time_stage == "t_15", select = "time", drop = TRUE) & Time <= subset(time, time_stage == "t_30", select = "time", drop = TRUE)) %>% mutate(., Q_BA = V_ba*(pi*(subset(diameter, stage == "30_LBNP", select = "diameter", drop = TRUE)/2)^2)*60)
  
  beat_t45 <- filter(beat, Time > subset(time, time_stage == "t_30", select = "time", drop = TRUE)) %>% mutate(., Q_BA = V_ba*(pi*(subset(diameter, stage == "45_LBNP", select = "diameter", drop = TRUE)/2)^2)*60)
  
  #combine stages into a single data frame.
  beat <- rbind(beat_t0, beat_t15, beat_t30, beat_t45)
  
  
  #add additional calculations if needed.  For example FVR and FVC are calculated below.
  beat <- mutate(beat, FVR = beat$MAP/beat$Q_BA, FVC = beat$Q_BA/beat$MAP)
  
  beat <- mutate(beat, SV = (beat$CO/beat$HR)*1000, TPR = beat$MAP/beat$CO)
  
  #Output files
  write.csv(beat, file = paste(dirname(beat_in),"/",f_out, sep = ""), row.names = FALSE, quote = FALSE)
  
  write.csv(diameter, file = paste(dirname(beat_in), "/", ID, "_diameter_summary_", condition, ".csv", sep = ""), row.names = FALSE)
}
