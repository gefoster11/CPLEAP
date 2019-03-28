#Dynamic NVT Script based on Briant et al. J Physiol 594.17 (2016) pp 4753-4768.  Utilizing the burst height approach

###accepts a .csv file formatted as a brs file with columns "time", "sel_duration", "BP_CMT", "MAP", "SBP", "MSNA_CMT", "MSNA_CMT_NUM", "MSNA_baseline", "MSNA_Amp" and a 3 line header.  Data are from beat-by-beat extraction from the R-wave in labchart.  MSNA bursts must be labelled using the text "BURST" and finometer calibration periods must be labelled with the text "CAL".  The output data from this function is a nested data frame with columns for id (eg. "NVT38"), group (eg. "control" vs. "IH"), condition (eg. "pre" versus "post").  Additional columns include the processed data (data), the binned data (binned_df), the output plot (plot), and the output results (result).

#eg. data <- sNVT(id = "NVT13", group = "IH", condition = "pre", f_in = "brs_NVT13_pre_IH.csv", t_start = 18079, t_duration = 300)

sNVT <- function(id, group, condition, f_in, t_start, t_duration) {

  #input variables - use for troubleshooting function.
  #id <- "NVT38" #id tag
  #group <- "control" #group tag
  #condition <- "post" #condition tag
  #f_in <- "brs_NVT38_post_control.csv" #file name
  #t_start <- 7520 #seconds
  #t_duration <- 300 #seconds
  
  
#libraries
library(tidyverse)
library(ggplot2)
library(tseries)

df <- read_csv(file = f_in, col_names = c("time", "sel_duration", "BP_CMT", "MAP", "SBP", "DBP", "MSNA_CMT", "MSNA_CMT_NUM", "MSNA_baseline", "MSNA_Amp"), skip = 3)

df$DBP <- as.numeric(df$DBP)
df$BP_CMT[is.na(df$BP_CMT)] <- "no_cal"

df <- df %>%
  filter(., time > t_start & time < t_start + t_duration) %>%
    mutate(burst = as.numeric(!duplicated(.$MSNA_CMT_NUM) & .$MSNA_CMT == "BURST"), MSNA_Amp = burst*MSNA_Amp ,MSNA_Norm_Amp = (MSNA_Amp/max(MSNA_Amp))*100, burstXnorm_amp = burst*MSNA_Norm_Amp)


##generate vector of two beat height sums and insert into df
two_beat_sum <- vector(length = length(df$burstXnorm_amp))
for (i in seq_along(df$burstXnorm_amp)) {
  temp <- df$burstXnorm_amp[[i]] + df$burstXnorm_amp[i + 1]
  two_beat_sum[[i]] <- temp
}

df <- df %>% mutate(., "two_beat_sum" = two_beat_sum) 

#Cross Correlation to determine best lag
cross_corr <- NULL

for (i in 0:15) {
  temp <- df %>% mutate(., lag = lag(.$two_beat_sum, n=i))
  temp <- temp %>% filter(., lag > 0 & BP_CMT != "CAL")
  lm <- summary(lm(DBP ~ lag, temp))
  result <- c(lag = i, intercept = lm$coefficients[1], slope = lm$coefficients[2], r2 = lm$adj.r.squared, p = lm$coefficients[8])
  cross_corr <- rbind(cross_corr, result)
}

##Find max r2 after a lag of 5
lag <- cross_corr %>% as.data.frame(.) %>% filter(., lag > 5) %>% filter(., r2 == max(r2))

df <- df %>% mutate(., lag_MSNA_amp = lag(.$two_beat_sum, n=lag$lag)) %>% filter(., lag_MSNA_amp > 0 & BP_CMT != "CAL")

##plot of r2 as a function of lag
lag_plot <- as.data.frame(cross_corr) %>% ggplot(., aes(x = lag, y = r2)) +
  geom_point() +
  geom_line() + 
  geom_vline(aes(xintercept = 5), lty = "dashed") +
  geom_vline(data = lag, aes(xintercept = lag), lty = "dashed", colour = "red") +
  geom_hline(data = lag, aes(yintercept = r2), lty = "dashed", colour = "red")

#bin data by 1% summed heights
binned_df <- df %>% cbind(cuts = cut(.$lag_MSNA_amp, round(max(.$lag_MSNA_amp)-min(.$lag_MSNA_amp), 1)), .) %>% group_by(cuts) %>% summarise(DBP_N = length(DBP), DBP_mean = mean(DBP), DBP_se = sd(DBP)/sqrt(length(DBP)), MSNA_N = length(lag_MSNA_amp), MSNA_mean = mean(lag_MSNA_amp), MSNA_se = sd(lag_MSNA_amp)/sqrt(length(lag_MSNA_amp)))

#lm by binned data
lm_binned <- binned_df %>% lm(DBP_mean ~ MSNA_mean, weights = DBP_N, .) %>% summary(.)

#Plot
plot <- df %>% ggplot(., aes(x = lag_MSNA_amp, y = DBP)) +
    geom_point(alpha = 1/3) +
    geom_smooth(method = "lm") +
    geom_point(data = binned_df, aes(x = MSNA_mean, y = DBP_mean), colour = "red") +
    geom_errorbar(data = binned_df, aes(x = MSNA_mean, y = DBP_mean, ymin = DBP_mean - DBP_se, ymax = DBP_mean + DBP_se)) +
    annotate("text", x = max(df$lag_MSNA_amp), y = min(df$DBP), label = paste0("lag = ", lag$lag,";, y = ", round(lag$slope, 2), "x + ", round(lag$intercept, 2), ", r2 = ", round(lag$r2, 2), ", p = ", round(lag$p, 3)), hjust = 1, vjust = 0, size = 3) +
    labs(x = "2-beat Sum - MSNA Amplitude (%)", y = "DBP (mmHg)", title = "Sympathetic NVT", subtitle = paste0("ID: ", id, ", Group: ", group, ", Condition: ", condition))

##create output data - nested data frame
data <- cbind(id, group, condition, df) %>% nest(., -id, -group, -condition, .key = "data") %>% mutate(., binned_data = list(binned_data = binned_df), plot = list(plot = plot), lag_plot = list(lag_plot = lag_plot), result = list(result = lag))
}