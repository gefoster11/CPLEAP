#Generate burst-by-burst .csv file from labchart output.  Data from labchart must be outputed to the data pad from the selection of each R-wave to R-wave whilst grabbing the min integrated MSNA, the max-min integrated MSNA, the MSNA_comment number, and the MSNA_Comment text.  More details are available below.

#Requirements:
# (1) col_names - provide a vector to name your input columns (eg. col_names <- c("Time", "Sel_Duration", "BP_Comment", "MSNA_Comment", "MSNA_Num", "MSNA_Baseline", "MSNA_Amplitude")
# (2) output file name (f_out) eg. "burst_NVT28_pre.csv"
# (3)  Adjust the code if other calculations are needed for example the code normalizes MSNA amplitude to the largest burst in the time selection for baseline.  Other methods could be used.  Set the start and end-time of the selection you wish to find the largest burst in for normalization purposes.

#eg. burst_file(col_names = c("Time", "Sel_Duration", "BP_Comment", "MSNA_Comment", "MSNA_Num", "MSNA_Baseline", "MSNA_Amplitude"), base_start = 0, base_end = 300, f_out = "burst_NVT28_pre_test.csv")

#PRE- burst_file(col_names = c("Time", "Sel_Duration", "BP_Comment", "MSNA_Comment", "MSNA_Num", "MSNA_Baseline", "MSNA_Amplitude"), base_start = 0, base_end = 300, f_out = "burst_NVT29_pre_control.csv")

#POST- burst_file(col_names = c("Time", "Sel_Duration", "BP_Comment", "MSNA_Comment", "MSNA_Num", "MSNA_Baseline", "MSNA_Amplitude"), base_start = 0, base_end = 300, f_out = "burst_NVT29_post_control.csv")

burst_file <- function(col_names, base_start, base_end, f_out) {
  library(dplyr)
  f_in <- file.choose()
  
  data <- read.delim(f_in, header = FALSE, na.strings = "#NUM!", skip = 3)
  data <- select(data, 1:length(col_names))
  colnames(data) <- col_names
  
  data$Sel_Duration <- as.numeric(data$Sel_Duration)
  data$BP_Comment <- as.character(data$BP_Comment)
  data$MSNA_Comment <- as.character(data$MSNA_Comment)
  data <- filter(data, MSNA_Comment == "BURST")
  
  data <- data[!duplicated(data$MSNA_Num), ]
  
  #add additional calculations needed here...
  
  #normalize burst amplitude to largest burst in the selection
  #data <- mutate(data, MSNA_Normalized_Amplitude = (data$MSNA_Amplitude/max(data$MSNA_Amplitude))*100)
  
  #normalize burst amplitude to the largest burst in the time period set by base_start and base_end
  data <- mutate(data, MSNA_Normalized_Amplitude = (data$MSNA_Amplitude/max(data$MSNA_Amplitude[data$Time >= base_start & data$Time < base_end]))*100)
  
  #Output file
  write.csv(data, file = paste(dirname(f_in),"/",f_out, sep = ""), row.names = FALSE, quote = FALSE)
  }