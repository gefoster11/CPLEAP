##Load labchart text file including time, velocity, AVG_velocity, comments outputed at 100 Hz with header.  In lab chart - enter comments at "cuff_up" and "cuff_off".  Ensure the cyclic parameters for calculating AVG_velocity are correct.  Then select from start of each file to 540s later using the "find" command.  This period of data should correspond exactly with the 9 minute video recording of diameter.

##Load diameter data which corresponds to the loaded labchart text file which has been processed by edge detection software.

##When running this function you will first be asked to load your labchart text file and then your diameter.csv file

##Provide the id and the measurement time point.  time point = "1" for the first measurement.

##eg.  FMD_Analysis(id = "GEF", time = "1")

FMD_Analysis <- function(id, time){
  
# id <- "GEF"
# time <- "1"
  
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

for(i in 1:nrow(df_diam)){
  if(df_diam$time[i] > 60){
    df_diam$time[i] <- df_diam$time[i] + 240}
  }

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
for(i in 1:nrow(df)){
  if(df$SR[i] > 0){
    df$SR_POS[i] <- df$SR[i]
    df$SRAUC_POS[i] <- df$SR[i]
    df$SR_NEG[i] <- NA
    df$SRAUC_NEG[i] <- 0
    }
  if(df$SR[i] < 0){
      df$SR_POS[i] <- NA
      df$SRAUC_POS[i] <- 0
      df$SR_NEG[i] <- df$SR[i]
      df$SRAUC_NEG[i] <- df$SR[i]
      }
}

#pull baseline data
df_baseline <- filter(df, time >3 & time <= 60)
baseline <- summarise_all(df_baseline[c(-4, -11, -13)], .funs = "mean", na.rm = TRUE)
baseline <- mutate(baseline, OSI = (abs(SR_NEG)/(abs(SR_POS) + abs(SR_NEG)))*100)

#calculate SRAUC
baseline$SRAUC <- AUC(df_baseline$time, df_baseline$SR, method = "trapezoid")
baseline$SRAUC_POS <- AUC(df_baseline$time, df_baseline$SRAUC_POS, method = "trapezoid")
baseline$SRAUC_NEG <- AUC(df_baseline$time, df_baseline$SRAUC_NEG, method = "trapezoid")
baseline$time_to_peak <- NA

#RESPONSE DATA
df_response <- filter(df, time > 360)
time_peak_diameter <- df_response$time[df_response$diameter_smooth == max(df_response$diameter_smooth)]
df_peak_diameter <- filter(df_response, time > (time_peak_diameter-2.5) & time < (time_peak_diameter+2.5))
response_peak_diameter <- summarise_all(df_peak_diameter[c(-4,-11,-13)], .funs = "mean", na.rm = TRUE)
response_peak_diameter <- mutate(response_peak_diameter, OSI = (abs(SR_NEG)/(abs(SR_POS) + abs(SR_NEG)))*100)

#calculate SRAUC to peak diameter
response_peak_diameter$SRAUC <- AUC(df_response$time, df_response$SR, method = "trapezoid")
response_peak_diameter$SRAUC_POS <- AUC(df_response$time, df_response$SRAUC_POS, method = "trapezoid")
response_peak_diameter$SRAUC_NEG <- AUC(df_response$time, df_response$SRAUC_NEG, method = "trapezoid")
response_peak_diameter$time_to_peak <- time_peak_diameter - df_response$time[1]

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
  geom_point(data = summary, colour = "red", size = 3)

p_V_mean <- ggplot(data = df, aes_string(x = "time", y = "V_mean")) +
  geom_line() +
  geom_point(data = summary, colour = "red", size = 3)

p_diameter <- ggplot(data = df, aes_string(x = "time", y = "diameter")) +
  geom_line() +
  geom_point(data = summary, colour = "red", size = 3) 
  

p_diameter_smooth <- ggplot(data = df, aes_string(x = "time", y = "diameter_smooth")) +
  geom_line() +
  geom_point(data = summary, colour = "red", size = 3)

p_BF <- ggplot(data = df, aes_string(x = "time", y = "BF")) +
  geom_line() +
  geom_point(data = summary, colour = "red", size = 3) + 
  geom_point(data = summary, aes_string(x = "time_peak_BF", y = "BF_peak"), colour = "blue", size = 3)

p_SR <- ggplot(data = df, aes_string(x = "time", y = "SR")) +
  geom_line() +
  geom_point(data = summary, colour = "red", size = 3)

p_SR_mean <- ggplot(data = df, aes_string(x = "time", y = "SR_mean")) +
  geom_line() +
  geom_point(data = summary, colour = "red", size = 3)


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

pdf(file = paste(dirname(chart_file), "/", id, "_", time, "_FMD.pdf", sep = ""), width = 8.5, height = 11)
grid.arrange(grobs = grobs.list, nrow = 7, ncol = 1, family = "Arial Unicode MS", paper = "special", onefile = FALSE)
dev.off()

#embed_fonts(paste(dirname(chart_file), "/", id, "_", time, "_FMD.pdf", sep = ""))

#Output file
write.csv(summary_out, file = paste(dirname(chart_file), "/", id, "_", time, "_FMD.csv", sep = ""), row.names = FALSE, quote = FALSE)
}