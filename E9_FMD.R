##Load labchart text file including time, velocity, AVG_velocity, comments outputed at 100 Hz with header. Ensure the cyclic parameters for calculating AVG_velocity are correct.  Then select from start of each file to 540s later using the "find" command.  This period of data should correspond exactly with the 9 minute video recording of diameter.

##Load diameter data which corresponds to the loaded labchart text file which has been processed by edge detection software.  The diameter file must be created from edge detection software and bad data must be removed using the erase tool.  However, the cleaned diameter data does not automatically get outputed from the outputfile.  The copy graphs option must be utilized and the cleaned diameter file must be pasted into the outputfile prior to running this FMD function script.

##When running this function you will first be asked to load your labchart text file and then your diameter.csv file

##Provide the id and the measurement time point.  time point = "1" for the first measurement.  t_start and t_end are the search window used for narrowing in on the correct FMD peak. begin by selecting t_start to 360 and t_end to 540.  If the correct peak is not detected by the function then adjust your times accordingly to encapsulate the correct peak.

##eg.  FMD_Analysis(id = "GEF", time = "1", t_start = 360, t_end = 540)

FMD_Analysis <- function(id, time, t_start, t_end){
  
# id <- "GEF"
# time <- "1"
# t_start = 380
# t_end = 500
  
#clear the workspace
# rm(list = ls())  
  
##Load Libraries
# Load packages (checks if they are installed and loads the librarys)
y <- c("gridExtra", "plyr", "dplyr", "reshape", "reshape2", "ggplot2",
       "Rmisc", "grid", "lmerTest", "multcomp", "nlme", "lsmeans", "lattice", "psych", "car", "pbkrtest", "visreg", "rmcorr", "knitr", "car", "lme4", "sjstats", "extrafont", "grDevices", "stringr", "TTR", "DescTools", "gridExtra") 

for(i in 1:length(y)){
  is_installed <- function(mypkg){is.element(mypkg,
                                             installed.packages()[,1])}
  if(!is_installed(y[i])){ 
    install.packages(y[i],
                     repos="http://lib.stat.cmu.edu/R/CRAN")
  }
  library(y[i], character.only=TRUE,
          quietly=TRUE,verbose=FALSE)
}


#Load Chart Data.
chart_col_names <- c("time", "velocity", "V_mean", "comments")

print("load labchart text file")
chart_file <- file.choose()
setwd(dirname(chart_file))

df_chart <- read.delim(chart_file, header = FALSE, na.strings = "NaN", colClasses = c("numeric", "numeric", "numeric", "character"), skip = 9, col.names = chart_col_names)

#reference time to zero if needed
df_chart$time <- df_chart$time-df_chart$time[1]

#Load Diameter Data.
print("load diameter.csv file")
diam_file <- file.choose()
df_diam <- read.csv(diam_file, skip = 83)
df_diam <- df_diam[,1:2]
names(df_diam) <- c("time", "diameter")

df_diam$time <- ifelse(df_diam$time > 60, df_diam$time + 240, df_diam$time)


#merge dataframes
df <- approx(df_diam$time, df_diam$diameter, xout = df_chart$time, rule = 2, method = "linear", ties = mean)
df <- as.data.frame(df, col.names = c("time", "diameter"))
df <- merge(df_chart, df, by.x = "time")

#calculate BF and SR and SR_mean
df$BF <- df$V_mean * (pi*((df$diameter)/2)^2) * 60
df$SR <- (4*df$velocity)/df$diameter
df$SR_mean <- (4*df$V_mean)/df$diameter

#3s smoothing average function for diameter
df$diameter_smooth <- SMA(df$diameter, n=300)

#calculate SR_POS and SR_NEG
# for(i in 1:nrow(df)){
#   if(df$SR[i] > 0){
#     df$SR_POS[i] <- df$SR[i]
#     df$SRAUC_POS[i] <- df$SR[i]
#     df$SR_NEG[i] <- NA
#     df$SRAUC_NEG[i] <- 0
#     }
#   if(df$SR[i] < 0){
#       df$SR_POS[i] <- NA
#       df$SRAUC_POS[i] <- 0
#       df$SR_NEG[i] <- df$SR[i]
#       df$SRAUC_NEG[i] <- df$SR[i]
#       }
# }

df$SR_POS[df$SR > 0] <- df$SR[df$SR > 0]
df$SRAUC_POS[df$SR > 0] <- df$SR[df$SR > 0]
df$SR_NEG[df$SR > 0] <- NA
df$SRAUC_NEG[df$SR > 0] <- 0

df$SR_POS[df$SR < 0] <- NA
df$SRAUC_POS[df$SR < 0] <- 0
df$SR_NEG[df$SR < 0] <- df$SR[df$SR < 0]
df$SRAUC_NEG[df$SR < 0] <- df$SR[df$SR < 0]


#pull baseline data
df_baseline <- dplyr::filter(df, time >10 & time <= 60)
baseline <- summarise_all(df_baseline[c(-4, -11, -13)], .funs = "mean", na.rm = TRUE)
baseline <- mutate(baseline, OSI = (abs(SR_NEG)/(abs(SR_POS) + abs(SR_NEG)))*100)

#replace mean diameter data with values from r-wave
df_baseline_rwave <- df_baseline[df_baseline$comments != "",]
baseline_rwave <- summarise_all(df_baseline_rwave[c(-4, -11, -13)], .funs = "mean", na.rm = TRUE)
baseline$diameter <- baseline_rwave$diameter
baseline$diameter_smooth <- baseline_rwave$diameter_smooth

#calculate SRAUC
baseline$SRAUC <- AUC(df_baseline$time, df_baseline$SR, method = "trapezoid")
baseline$SRAUC_POS <- AUC(df_baseline$time, df_baseline$SRAUC_POS, method = "trapezoid")
baseline$SRAUC_NEG <- AUC(df_baseline$time, df_baseline$SRAUC_NEG, method = "trapezoid")
baseline$time_to_peak <- NA


#RESPONSE DATA
df_response <- dplyr::filter(df, time > 360)
df_response_narrow <- dplyr::filter(df, time > t_start & time < t_end)
time_peak_diameter <- df_response_narrow$time[df_response_narrow$diameter_smooth == max(df_response_narrow$diameter_smooth[df_response_narrow$comments != ""])]
df_peak_diameter <- dplyr::filter(df_response, time > (time_peak_diameter[1]-2.5) & time < (time_peak_diameter[1]+2.5))
df_response_to_peak_diameter <- dplyr::filter(df_response, time <= time_peak_diameter[1])
response_peak_diameter <- dplyr::summarise_all(df_peak_diameter[c(-4,-11,-13)], .funs = "mean", na.rm = TRUE)
response_peak_diameter <- dplyr::mutate(response_peak_diameter, OSI = (abs(SR_NEG)/(abs(SR_POS) + abs(SR_NEG)))*100)

#replace response data mean diameter data with values from r-wave
df_peak_rwave <- df_peak_diameter[df_peak_diameter$comments != "",]
response_rwave <- summarise_all(df_peak_rwave[c(-4, -11, -13)], .funs = "mean", na.rm = TRUE)
response_peak_diameter$diameter <- response_rwave$diameter
response_peak_diameter$diameter_smooth <- response_rwave$diameter_smooth

#calculate SRAUC to peak diameter
response_peak_diameter$SRAUC <- AUC(df_response_to_peak_diameter$time, df_response_to_peak_diameter$SR, method = "trapezoid")
response_peak_diameter$SRAUC_POS <- AUC(df_response_to_peak_diameter$time, df_response_to_peak_diameter$SRAUC_POS, method = "trapezoid")
response_peak_diameter$SRAUC_NEG <- AUC(df_response_to_peak_diameter$time, df_response_to_peak_diameter$SRAUC_NEG, method = "trapezoid")
response_peak_diameter$time_to_peak <- time_peak_diameter[1] - df_response$time[1]

#MERGE baseline and Peak Diameter
summary <- rbind(baseline, response_peak_diameter)
summary$FMD <- c(NA, summary$diameter[2] - summary$diameter[1])
summary$FMD_percent <- c(NA, (summary$diameter[2] - summary$diameter[1])/summary$diameter[1]*100)
summary$FMD_percent_SRAUC <- c(NA, ((summary$diameter[2] - summary$diameter[1])/summary$diameter[1]*100)/summary$SRAUC[2])

#Calculate Reactive Hypermia Parameters
summary$time_peak_BF <- c(NA, mean(df_response$time[df_response$BF == max(df_response$BF)]))

summary$BF_peak <- c(NA, mean(df_response$BF[df_response$time > (summary$time_peak_BF[2]) & df_response$time < (summary$time_peak_BF[2] + 5)]))

summary$BF_AUC <- c(NA, AUC(df_response$time, df_response$BF, method = "trapezoid"))


#check data
p_velocity <- ggplot(data = df, aes_string(x = "time", y = "velocity")) +
  geom_line() + 
  geom_point(data = summary, colour = "red", size = 2)

p_V_mean <- ggplot(data = df, aes_string(x = "time", y = "V_mean")) +
  geom_line() +
  geom_point(data = summary, colour = "red", size = 2)

p_diameter <- ggplot(data = df, aes_string(x = "time", y = "diameter")) +
  geom_line() +
  geom_point(data = summary, colour = "red", size = 2) 
  

p_diameter_smooth <- ggplot(data = df, aes_string(x = "time", y = "diameter_smooth")) +
  geom_line() +
  geom_point(data = summary, colour = "red", size = 2)

p_BF <- ggplot(data = df, aes_string(x = "time", y = "BF")) +
  geom_line() +
  geom_point(data = summary, colour = "red", size = 2) + 
  geom_point(data = summary, aes_string(x = "time_peak_BF", y = "BF_peak"), colour = "blue", size = 2)

p_SR <- ggplot(data = df, aes_string(x = "time", y = "SR")) +
  geom_line() +
  geom_point(data = summary, colour = "red", size = 2)

p_SR_mean <- ggplot(data = df, aes_string(x = "time", y = "SR_mean")) +
  geom_line() +
  geom_point(data = summary, colour = "red", size = 2)


##Save and output figure and data table.
plots.list <- list(p_velocity, p_V_mean, p_diameter, p_diameter_smooth, p_BF, p_SR, p_SR_mean)
# Builds the grobs list
grobs.list <- lapply(plots.list, ggplotGrob)

# Get the max width
widths.list <- do.call(grid::unit.pmax, lapply(grobs.list, "[[", 'widths'))

# Assign the max width to all grobs
grobs.list <- lapply(grobs.list, function(x) {
  x[['widths']] = widths.list
  x})

condition <- c("baseline", "peak_diameter")

summary_out <- cbind(id, time, condition, summary)

pdf(file = paste(id, "_", time, "_FMD.pdf", sep = ""), width = 8.5, height = 11)
grid.arrange(grobs = grobs.list, nrow = 7, ncol = 1, family = "Arial Unicode MS", paper = "special", onefile = FALSE)
dev.off()

#embed_fonts(paste(dirname(chart_file), "/", id, "_", time, "_FMD.pdf", sep = ""))

#Output file
write.csv(summary_out, file = paste(id, "_", time, "_FMD.csv", sep = ""), row.names = FALSE, quote = FALSE)
}
