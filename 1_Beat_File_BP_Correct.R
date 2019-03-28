#Correct BP in beat-by-beat file
#Requirements:
# (1) Start (t_start) and end-time of baseline period (t_end)
# (2) Average SBP and DBP from at least three brachial BP's collected during the baseline period (provide each as a vector ie c(120, 118, 117).
# (3) output file name (f_out) eg. beat_NVT28_pre
# (4) col_names - provide a vector to name your input columns (eg. c("Time", "SPO2", "SBP", "DBP", "MAP", "HR", "SV", "CO", "TPR"))
# (5)  Adjust the code if other calculations are needed for example to recalculate TPR after correcting BP or adding calculations like CVR and CVC.  Mutate function can be used for adding additional columns of data.
# eg. BP_Correction(bp_correct_t_f = TRUE, t_start = 5054.971792, t_end = 5143.071792, SBP = c(112, 104, 108), DBP = c(55, 57, 58), f_out = "beat_NVT28_pre_cBP.csv", col_names = c("Time", "SPO2", "SBP", "DBP", "MAP", "HR", "SV", "CO", "TPR", "V_ba"))

#PRE - BP_Correction(bp_correct_t_f = FALSE, f_out = "beat_NVT36_pre_control.csv", col_names = c("Time", "SPO2", "SBP", "DBP", "MAP", "HR", "SV", "CO", "TPR", "V_ba"))

#POST - BP_Correction(bp_correct_t_f = FALSE, f_out = "beat_NVT36_post_control.csv", col_names = c("Time", "SPO2", "SBP", "DBP", "MAP", "HR", "SV", "CO", "TPR", "V_ba"))

BP_Correction <- function(bp_correct_t_f, t_start, t_end, SBP, DBP, f_out, col_names) {
        library(dplyr)
        f_in <- file.choose()
        data <- read.delim(f_in, header = FALSE, na.strings = "#NUM!", colClasses = "numeric", skip = 3, col.names = col_names)
        
        if(bp_correct_t_f){
          SBP_ave <- sum(SBP)/length(SBP)
          DBP_ave <- sum(DBP)/length(DBP)
          MAP_ave <- DBP_ave + (1/3)*(SBP_ave - DBP_ave)
          
          base_data <- filter(data, Time >= t_start & Time < t_end)
          SBP_raw <- mean(base_data$SBP, na.rm = TRUE)
          DBP_raw <- mean(base_data$DBP, na.rm = TRUE)
          MAP_raw <- mean(base_data$MAP, na.rm = TRUE)
          
          SBP_cor <- SBP_ave - SBP_raw
          DBP_cor <- DBP_ave - DBP_raw
          MAP_cor <- MAP_ave - MAP_raw
          
          data$SBP <- data$SBP + SBP_cor
          data$DBP <- data$DBP + DBP_cor
          data$MAP <- data$MAP + MAP_cor
          
          #add additional calculations needed here...
          data$TPR <- data$MAP/data$CO
        }
        
        
        #Output file
        write.csv(data, file = paste(dirname(f_in),"/",f_out, sep = ""), row.names = FALSE, quote = FALSE)
}