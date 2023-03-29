# install.packages("devtools")
#devtools::install_github("adriancorrendo/metrica")

library(metrica)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(ggpubr)

rm(list=ls())

setwd("I:/My Drive/UAS_Beans/Beans_PlantHeight/Analysis")
df_SVREC20 <- read.csv('Results_final_2020_SVREC.csv')
names(df_SVREC20)
str(df_SVREC20)


# Create list of selected metrics
selected.metrics <- c("r","MAE","RMSE", "R2")

#### Ground truth vs stand count predicted ####

##### DSM / DTM method #####
# Create the plot
str(df_SVREC20)

# Perform the subtraction and save the results in a new column
df_SVREC20$Difference <- df_SVREC20$HT - df_SVREC20$EPH_0.5_median_quantile_0.9_DSM

Q1_DSM <- quantile(df_SVREC20$Difference, probs = 0.25, na.rm=TRUE)
Q3_DSM <- quantile(df_SVREC20$Difference, probs = 0.75, na.rm=TRUE)
IQR_DSM <- Q3_DSM - Q1_DSM

# Define the lower and upper bounds for outliers
kout = 1.5
lower_bound <- Q1_DSM - kout * IQR_DSM
upper_bound <- Q3_DSM + kout * IQR_DSM

# # Calculate the mean and standard deviation of the new column
# mean_diff <- mean(df_SVREC20$Difference, na.rm = TRUE)
# std_diff <- sd(df_SVREC20$Difference, na.rm = TRUE)
# 
# # Define the lower and upper bounds for outliers
# k_std = 1.5
# lower_bound <- mean_diff - k_std * std_diff
# upper_bound <- mean_diff + k_std * std_diff

# Replace the outliers in the new column with NA values
#df_SVREC20$Difference[df_SVREC20$Difference < lower_bound_diff | df_SVREC20$Difference > upper_bound_diff] <- NA
outlier_rows <- df_SVREC20$Difference < lower_bound | df_SVREC20$Difference > upper_bound
num_rows_orig <- nrow(df_SVREC20)
df_SVREC20[outlier_rows, ] <- NA
df_SVREC20 <- na.omit(df_SVREC20)
num_rows <- nrow(df_SVREC20)

#num_removed_rows <- num_rows_orig - num_rows_removed
percent_removed <- (num_rows / num_rows_orig) * 100

SVREC20 <- metrica::scatter_plot(data = df_SVREC20, 
                                               obs = HT, pred = EPH_0.5_median_quantile_0.9_DSM,
                                               # Activate print_metrics arg.
                                               print_metrics = TRUE, 
                                               # Indicate metrics list
                                               metrics_list = selected.metrics,
                                               # Customize metrics position
                                               position_metrics = c(x = 0 , y = 100),
                                               # Customize equation position
                                               position_eq = c(x = 0, y = 73),
                                               regline_size = 1,
                                               shape_color = "steelblue") + 
  ylab("Pred. Drone - CSM") +
  xlab(NULL) +
  # Customize axis breaks
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = "SVREC 2020") + 
  theme(plot.title=element_text(hjust=0.5)) +
  # Add the number of rows to the bottom right corner
  annotate("text", x = 100, y = 0, label = paste("n =", num_rows), hjust = 1, vjust = 0, size = 4, color = "black") +
  # Add the percentage of removed lines to the bottom right corner
  annotate("text", x = 100, y = -3, label = paste0(round(percent_removed, 2), "%"), hjust = 1, vjust = 0, size = 3, color = "black")



SVREC20


####

df_SVREC21 <- read.csv('Results_final_2021_SVREC.csv')
names(df_SVREC21)
str(df_SVREC21)

# Perform the subtraction and save the results in a new column
df_SVREC21$Difference <- df_SVREC21$HT - df_SVREC21$EPH_quantile_0.99_DSM

Q1_DSM <- quantile(df_SVREC21$Difference, probs = 0.25, na.rm=TRUE)
Q3_DSM <- quantile(df_SVREC21$Difference, probs = 0.75, na.rm=TRUE)
IQR_DSM <- Q3_DSM - Q1_DSM

# Define the lower and upper bounds for outliers
kout = 1.5
lower_bound <- Q1_DSM - kout * IQR_DSM
upper_bound <- Q3_DSM + kout * IQR_DSM

outlier_rows <- df_SVREC21$Difference < lower_bound | df_SVREC21$Difference > upper_bound
num_rows_orig <- nrow(df_SVREC21)
df_SVREC21[outlier_rows, ] <- NA
df_SVREC21 <- na.omit(df_SVREC21)
num_rows <- nrow(df_SVREC21)

#num_removed_rows <- num_rows_orig - num_rows_removed
percent_removed <- (num_rows / num_rows_orig) * 100

SVREC21 <- metrica::scatter_plot(data = df_SVREC21, 
                                 obs = HT, pred = EPH_quantile_0.99_DSM,
                                 # Activate print_metrics arg.
                                 print_metrics = TRUE, 
                                 # Indicate metrics list
                                 metrics_list = selected.metrics,
                                 # Customize metrics position
                                 position_metrics = c(x = 0 , y = 100),
                                 # Customize equation position
                                 position_eq = c(x = 0, y = 73),
                                 regline_size = 1,
                                 shape_color = "steelblue") + 
  
  ylab(NULL) +
  xlab(NULL) +
  # Customize axis breaks
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = "SVREC 2021") + 
  theme(plot.title=element_text(hjust=0.5))  +
  # Add the number of rows to the bottom right corner
  annotate("text", x = 100, y = 0, label = paste("n =", num_rows), hjust = 1, vjust = 0, size = 4, color = "black") +
  annotate("text", x = 100, y = -3, label = paste0(round(percent_removed, 2), "%"), hjust = 1, vjust = 0, size = 3, color = "black")

SVREC21

####

df_HUR21 <- read.csv('Results_final_HURON_2021.csv')
names(df_HUR21)
str(df_HUR21)

# Perform the subtraction and save the results in a new column
df_HUR21$Difference <- df_HUR21$HT - df_HUR21$EPH_quantile_0.99_DSM

Q1_DSM <- quantile(df_HUR21$Difference, probs = 0.25, na.rm=TRUE)
Q3_DSM <- quantile(df_HUR21$Difference, probs = 0.75, na.rm=TRUE)
IQR_DSM <- Q3_DSM - Q1_DSM

# Define the lower and upper bounds for outliers
kout = 1.5
lower_bound <- Q1_DSM - kout * IQR_DSM
upper_bound <- Q3_DSM + kout * IQR_DSM

outlier_rows <- df_HUR21$Difference < lower_bound | df_HUR21$Difference > upper_bound
num_rows_orig <- nrow(df_HUR21)
df_HUR21[outlier_rows, ] <- NA
df_HUR21 <- na.omit(df_HUR21)
num_rows <- nrow(df_HUR21)

#num_removed_rows <- num_rows_orig - num_rows_removed
percent_removed <- (num_rows / num_rows_orig) * 100

HUR21 <- metrica::scatter_plot(data = df_HUR21, 
                                 obs = HT, pred = EPH_quantile_0.99_DSM,
                                 # Activate print_metrics arg.
                                 print_metrics = TRUE, 
                                 # Indicate metrics list
                                 metrics_list = selected.metrics,
                                 # Customize metrics position
                                 position_metrics = c(x = 0 , y = 100),
                                 # Customize equation position
                                 position_eq = c(x = 0, y = 73),
                                 regline_size = 1,
                                 shape_color = "steelblue") + 
  
  ylab(NULL) +
  xlab(NULL) +
  # Customize axis breaks
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = "HURON 2021") +
  theme(plot.title=element_text(hjust=0.5)) +
  # Add the number of rows to the bottom right corner
  annotate("text", x = 100, y = 0, label = paste("n =", num_rows), hjust = 1, vjust = 0, size = 4, color = "black") +
  annotate("text", x = 100, y = -3, label = paste0(round(percent_removed, 2), "%"), hjust = 1, vjust = 0, size = 3, color = "black")

HUR21

###

df_SVREC22 <- read.csv('Results_final_2022_SVREC.csv')
names(df_SVREC22)
str(df_SVREC22)

# Perform the subtraction and save the results in a new column
df_SVREC22$Difference <- df_SVREC22$HT - df_SVREC22$EPH_0.25_median_quantile_0.99_DSM

Q1_DSM <- quantile(df_SVREC22$Difference, probs = 0.25, na.rm=TRUE)
Q3_DSM <- quantile(df_SVREC22$Difference, probs = 0.75, na.rm=TRUE)
IQR_DSM <- Q3_DSM - Q1_DSM

# Define the lower and upper bounds for outliers
kout = 1.5
lower_bound <- Q1_DSM - kout * IQR_DSM
upper_bound <- Q3_DSM + kout * IQR_DSM

outlier_rows <- df_SVREC22$Difference < lower_bound | df_SVREC22$Difference > upper_bound
num_rows_orig <- nrow(df_SVREC22)
df_SVREC22[outlier_rows, ] <- NA
df_SVREC22 <- na.omit(df_SVREC22)
num_rows <- nrow(df_SVREC22)

#num_removed_rows <- num_rows_orig - num_rows_removed
percent_removed <- (num_rows / num_rows_orig) * 100

SVREC22 <- metrica::scatter_plot(data = df_SVREC22, 
                                 obs = HT, pred = EPH_0.25_median_quantile_0.99_DSM,
                                 # Activate print_metrics arg.
                                 print_metrics = TRUE, 
                                 # Indicate metrics list
                                 metrics_list = selected.metrics,
                                 # Customize metrics position
                                 position_metrics = c(x = 0 , y = 100),
                                 # Customize equation position
                                 position_eq = c(x = 0, y = 73),
                                 regline_size = 1,
                                 shape_color = "steelblue") + 
  
  ylab(NULL) +
  xlab(NULL) +
  # Customize axis breaks
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = "SVREC 2022") +
  theme(plot.title=element_text(hjust=0.5)) +
  # Add the number of rows to the bottom right corner
  annotate("text", x = 100, y = 0, label = paste("n =", num_rows), hjust = 1, vjust = 0, size = 4, color = "black") +
  annotate("text", x = 100, y = -3, label = paste0(round(percent_removed, 2), "%"), hjust = 1, vjust = 0, size = 3, color = "black")

SVREC22


####

df_HUR22 <- read.csv('Results_final_HURON_2022.csv')
names(df_HUR22)
str(df_HUR22)

# Perform the subtraction and save the results in a new column
df_HUR22$Difference <- df_HUR22$HT - df_HUR22$EPH_0.25_median_quantile_0.99

Q1_DSM <- quantile(df_HUR22$Difference, probs = 0.25, na.rm=TRUE)
Q3_DSM <- quantile(df_HUR22$Difference, probs = 0.75, na.rm=TRUE)
IQR_DSM <- Q3_DSM - Q1_DSM

# Define the lower and upper bounds for outliers
kout = 1.5
lower_bound <- Q1_DSM - kout * IQR_DSM
upper_bound <- Q3_DSM + kout * IQR_DSM

outlier_rows <- df_HUR22$Difference < lower_bound | df_HUR22$Difference > upper_bound
num_rows_orig <- nrow(df_HUR22)
df_HUR22[outlier_rows, ] <- NA
df_HUR22 <- na.omit(df_HUR22)
num_rows <- nrow(df_HUR22)

#num_removed_rows <- num_rows_orig - num_rows_removed
percent_removed <- (num_rows / num_rows_orig) * 100
HUR22 <- metrica::scatter_plot(data = df_HUR22, 
                                 obs = HT, pred = EPH_0.25_median_quantile_0.99,
                                 # Activate print_metrics arg.
                                 print_metrics = TRUE, 
                                 # Indicate metrics list
                                 metrics_list = selected.metrics,
                                 # Customize metrics position
                                 position_metrics = c(x = 0 , y = 100),
                                 # Customize equation position
                                 position_eq = c(x = 0, y = 73),
                                 regline_size = 1,
                                 shape_color = "steelblue") + 
  
  ylab(NULL) +
  xlab(NULL) +
  # Customize axis breaks
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = "HURON 2022") +
  theme(plot.title=element_text(hjust=0.5)) +
  # Add the number of rows to the bottom right corner
  annotate("text", x = 100, y = 0, label = paste("n =", num_rows), hjust = 1, vjust = 0, size = 4, color = "black") +
  annotate("text", x = 100, y = -3, label = paste0(round(percent_removed, 2), "%"), hjust = 1, vjust = 0, size = 3, color = "black")

HUR22


##### Point Cloud method #####

# Create the plot
df_SVREC20 <- read.csv('Results_final_2020_SVREC.csv')
str(df_SVREC20)

# Perform the subtraction and save the results in a new column
df_SVREC20$Difference <- df_SVREC20$HT - df_SVREC20$EPH_0.5_median_quantile_0.9_PC

Q1_DSM <- quantile(df_SVREC20$Difference, probs = 0.25, na.rm=TRUE)
Q3_DSM <- quantile(df_SVREC20$Difference, probs = 0.75, na.rm=TRUE)
IQR_DSM <- Q3_DSM - Q1_DSM

# Define the lower and upper bounds for outliers
kout = 1.5
lower_bound <- Q1_DSM - kout * IQR_DSM
upper_bound <- Q3_DSM + kout * IQR_DSM

outlier_rows <- df_SVREC20$Difference < lower_bound | df_SVREC20$Difference > upper_bound
num_rows_orig <- nrow(df_SVREC20)
df_SVREC20[outlier_rows, ] <- NA
df_SVREC20 <- na.omit(df_SVREC20)
num_rows <- nrow(df_SVREC20)

#num_removed_rows <- num_rows_orig - num_rows_removed
percent_removed <- (num_rows / num_rows_orig) * 100

SVREC20_pc <- metrica::scatter_plot(data = df_SVREC20, 
                                 obs = HT, pred = EPH_0.5_median_quantile_0.9_PC,
                                 # Activate print_metrics arg.
                                 print_metrics = TRUE, 
                                 # Indicate metrics list
                                 metrics_list = selected.metrics,
                                 # Customize metrics position
                                 position_metrics = c(x = 0 , y = 100),
                                 # Customize equation position
                                 position_eq = c(x = 0, y = 73),
                                 regline_size = 1,
                                 shape_color = "seagreen4",
                                 regline_color = "coral3") + 
  ylab("Pred. Drone - Point Cloud") +
  xlab("Ground truth") +
  # Customize axis breaks
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  # Add the number of rows to the bottom right corner
  annotate("text", x = 100, y = 0, label = paste("n =", num_rows), hjust = 1, vjust = 0, size = 4, color = "black") +
  annotate("text", x = 100, y = -3, label = paste0(round(percent_removed, 2), "%"), hjust = 1, vjust = 0, size = 3, color = "black")

SVREC20_pc
####


df_SVREC21 <- read.csv('Results_final_2021_SVREC.csv')
str(df_SVREC21)

# Perform the subtraction and save the results in a new column
df_SVREC21$Difference <- df_SVREC21$HT - df_SVREC21$EPH_soil_0.5_quantile_0.99_PC

Q1_DSM <- quantile(df_SVREC21$Difference, probs = 0.25, na.rm=TRUE)
Q3_DSM <- quantile(df_SVREC21$Difference, probs = 0.75, na.rm=TRUE)
IQR_DSM <- Q3_DSM - Q1_DSM

# Define the lower and upper bounds for outliers
kout = 1.5
lower_bound <- Q1_DSM - kout * IQR_DSM
upper_bound <- Q3_DSM + kout * IQR_DSM

outlier_rows <- df_SVREC21$Difference < lower_bound | df_SVREC21$Difference > upper_bound
num_rows_orig <- nrow(df_SVREC21)
df_SVREC21[outlier_rows, ] <- NA
df_SVREC21 <- na.omit(df_SVREC21)
num_rows <- nrow(df_SVREC21)

#num_removed_rows <- num_rows_orig - num_rows_removed
percent_removed <- (num_rows / num_rows_orig) * 100

SVREC21_pc <- metrica::scatter_plot(data = df_SVREC21, 
                                 obs = HT, pred = EPH_soil_0.5_quantile_0.99_PC,
                                 # Activate print_metrics arg.
                                 print_metrics = TRUE, 
                                 # Indicate metrics list
                                 metrics_list = selected.metrics,
                                 # Customize metrics position
                                 position_metrics = c(x = 0 , y = 100),
                                 # Customize equation position
                                 position_eq = c(x = 0, y = 73),
                                 regline_size = 1,
                                 shape_color = "seagreen4",
                                 regline_color = "coral3") + 
  
  ylab(NULL) +
  xlab("Ground truth") +
  # Customize axis breaks
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  # Add the number of rows to the bottom right corner
  annotate("text", x = 100, y = 0, label = paste("n =", num_rows), hjust = 1, vjust = 0, size = 4, color = "black") +
  annotate("text", x = 100, y = -3, label = paste0(round(percent_removed, 2), "%"), hjust = 1, vjust = 0, size = 3, color = "black")


SVREC21_pc

####

df_HUR21 <- read.csv('Results_final_HURON_2021.csv')
names(df_HUR21)
str(df_HUR21)

# Perform the subtraction and save the results in a new column
df_HUR21$Difference <- df_HUR21$HT - df_HUR21$EPH_soil_0.5_quantile_0.99_PC

Q1_DSM <- quantile(df_HUR21$Difference, probs = 0.25, na.rm=TRUE)
Q3_DSM <- quantile(df_HUR21$Difference, probs = 0.75, na.rm=TRUE)
IQR_DSM <- Q3_DSM - Q1_DSM

# Define the lower and upper bounds for outliers
kout = 1.5
lower_bound <- Q1_DSM - kout * IQR_DSM
upper_bound <- Q3_DSM + kout * IQR_DSM

outlier_rows <- df_HUR21$Difference < lower_bound | df_HUR21$Difference > upper_bound
num_rows_orig <- nrow(df_HUR21)
df_HUR21[outlier_rows, ] <- NA
df_HUR21 <- na.omit(df_HUR21)
num_rows <- nrow(df_HUR21)

#num_removed_rows <- num_rows_orig - num_rows_removed
percent_removed <- (num_rows / num_rows_orig) * 100

HUR21_pc <- metrica::scatter_plot(data = df_HUR21, 
                               obs = HT, pred = EPH_soil_0.5_quantile_0.99_PC,
                               # Activate print_metrics arg.
                               print_metrics = TRUE, 
                               # Indicate metrics list
                               metrics_list = selected.metrics,
                               # Customize metrics position
                               position_metrics = c(x = 0 , y = 100),
                               # Customize equation position
                               position_eq = c(x = 0, y = 73),
                               regline_size = 1,
                               shape_color = "seagreen4",
                               regline_color = "coral3") + 
  
  ylab(NULL) +
  xlab("Ground truth") +
  # Customize axis breaks
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  # Add the number of rows to the bottom right corner
  annotate("text", x = 100, y = 0, label = paste("n =", num_rows), hjust = 1, vjust = 0, size = 4, color = "black") +
  annotate("text", x = 100, y = -3, label = paste0(round(percent_removed, 2), "%"), hjust = 1, vjust = 0, size = 3, color = "black")


HUR21_pc

####

df_SVREC22 <- read.csv('Results_final_2022_SVREC.csv')
names(df_SVREC22)
str(df_SVREC22)

# Perform the subtraction and save the results in a new column
df_SVREC22$Difference <- df_SVREC22$HT - df_SVREC22$EPH_0.25_median_quantile_0.99_PC

Q1_DSM <- quantile(df_SVREC22$Difference, probs = 0.25, na.rm=TRUE)
Q3_DSM <- quantile(df_SVREC22$Difference, probs = 0.75, na.rm=TRUE)
IQR_DSM <- Q3_DSM - Q1_DSM

# Define the lower and upper bounds for outliers
kout = 1.5
lower_bound <- Q1_DSM - kout * IQR_DSM
upper_bound <- Q3_DSM + kout * IQR_DSM

outlier_rows <- df_SVREC22$Difference < lower_bound | df_SVREC22$Difference > upper_bound
num_rows_orig <- nrow(df_SVREC22)
df_SVREC22[outlier_rows, ] <- NA
df_SVREC22 <- na.omit(df_SVREC22)
num_rows <- nrow(df_SVREC22)

#num_removed_rows <- num_rows_orig - num_rows_removed
percent_removed <- (num_rows / num_rows_orig) * 100

SVREC22_pc <- metrica::scatter_plot(data = df_SVREC22, 
                                 obs = HT, pred = EPH_0.25_median_quantile_0.99_PC,
                                 # Activate print_metrics arg.
                                 print_metrics = TRUE, 
                                 # Indicate metrics list
                                 metrics_list = selected.metrics,
                                 # Customize metrics position
                                 position_metrics = c(x = 0 , y = 100),
                                 # Customize equation position
                                 position_eq = c(x = 0, y = 73),
                                 regline_size = 1,
                                 shape_color = "seagreen4",
                                 regline_color = "coral3") + 
  
  ylab(NULL) +
  xlab("Ground truth") +
  # Customize axis breaks
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  # Add the number of rows to the bottom right corner
  annotate("text", x = 100, y = 0, label = paste("n =", num_rows), hjust = 1, vjust = 0, size = 4, color = "black") +
  annotate("text", x = 100, y = -3, label = paste0(round(percent_removed, 2), "%"), hjust = 1, vjust = 0, size = 3, color = "black")


SVREC22_pc

###

df_HUR22 <- read.csv('Results_final_HURON_2022.csv')
names(df_HUR22)
str(df_HUR22)

# Perform the subtraction and save the results in a new column
df_HUR22$Difference <- df_HUR22$HT - df_HUR22$EPH_0.25_median_quantile_0.99.1

Q1_DSM <- quantile(df_HUR22$Difference, probs = 0.25, na.rm=TRUE)
Q3_DSM <- quantile(df_HUR22$Difference, probs = 0.75, na.rm=TRUE)
IQR_DSM <- Q3_DSM - Q1_DSM

# Define the lower and upper bounds for outliers
kout = 1.5
lower_bound <- Q1_DSM - kout * IQR_DSM
upper_bound <- Q3_DSM + kout * IQR_DSM

outlier_rows <- df_HUR22$Difference < lower_bound | df_HUR22$Difference > upper_bound
num_rows_orig <- nrow(df_HUR22)
df_HUR22[outlier_rows, ] <- NA
df_HUR22 <- na.omit(df_HUR22)
num_rows <- nrow(df_HUR22)

#num_removed_rows <- num_rows_orig - num_rows_removed
percent_removed <- (num_rows / num_rows_orig) * 100

HUR22_pc <- metrica::scatter_plot(data = df_HUR22, 
                               obs = HT, pred = EPH_0.25_median_quantile_0.99.1,
                               # Activate print_metrics arg.
                               print_metrics = TRUE, 
                               # Indicate metrics list
                               metrics_list = selected.metrics,
                               # Customize metrics position
                               position_metrics = c(x = 0 , y = 100),
                               # Customize equation position
                               position_eq = c(x = 0, y = 73),
                               regline_size = 1,
                               shape_color = "seagreen4",
                               regline_color = "coral3" ) + 
  
  ylab(NULL) +
  xlab("Ground truth") +
  # Customize axis breaks
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  # Add the number of rows to the bottom right corner
  annotate("text", x = 100, y = 0, label = paste("n =", num_rows), hjust = 1, vjust = 0, size = 4, color = "black") +
  
  annotate("text", x = 100, y = -3, label = paste0(round(percent_removed, 2), "%"), hjust = 1, vjust = 0, size = 3, color = "black")

HUR22_pc


##### Merging plots #####

theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

ggarrange(
  SVREC20, SVREC21, HUR21, SVREC22, HUR22,
  SVREC20_pc, SVREC21_pc, HUR21_pc, SVREC22_pc, HUR22_pc,
  ncol = 5, nrow = 2, 
  common.legend = TRUE, legend = "bottom"
)
















#### Repetitions correlations- Using BLUPs ####

# library(metan)
# names(df)
# #df2 <- df[-c(13:25)] %>% as.data.frame()
# df3 <- df %>% 
#   select(Name, REP, IBLK, Experiment, StandCount_gt)
# str(df3)
# 
# df3$Name <- as.factor(df3$Name)
# df3$REP <- as.factor(df3$REP)
# df3$IBLK <- as.factor(df3$IBLK)
# df3$Experiment <- as.factor(df3$Experiment)
# str(df3)
# 
# df3 <- na.omit(df3)
# mod1 <- gamem(df3,
#                gen = Name,
#                rep = REP,
#                block = IBLK,
#                resp = StandCount_gt,
#               by = Experiment)
# 
# get_model_data(mod1, "details") 
# # Class of the model: gamem
# # Variable extracted: details
# get_model_data(mod1, "lrt") 
# # Class of the model: gamem
# # Variable extracted: lrt
# get_model_data(mod1, "genpar")
# # Class of the model: gamem
# # Variable extracted: genpar
# data <- get_model_data(mod1)
# 
# dfrep1<- df3 %>% 
#   subset(df3$REP == 1) %>% 
#   arrange(Name)
# 
# dfrep2<- df3 %>% 
#   subset(df3$REP == 2) %>% 
#   arrange(Name)
# 
# dfrep <- cbind(dfrep1,dfrep2)
# str(dfrep)
# colnames(dfrep)<- c("Name",            "REP"  ,           "IBLK"   ,         "Experiment", "StandCount_gt" ,         
#                      "Name2",            "REP2"  ,           "IBLK2"  ,          "Experiment2" ,"StandCount_gt2"  )
# ### Ploting the Reps
# plot_canopy_mask_VI <- metrica::scatter_plot(data = dfrep, 
#                                              obs = StandCount_gt, pred = StandCount_gt2,
#                                              # Activate print_metrics arg.
#                                              print_metrics = T, 
#                                              # Indicate metrics list
#                                              metrics_list = selected.metrics,
#                                              # Customize metrics position
#                                              position_metrics = c(x = 140 , y = 100),
#                                              # Customize equation position
#                                              #print_eq = F,
#                                              position_eq = c(x = 140, y = 110),
#                                              regline_size = 1,
#                                              shape_color = "steelblue") + 
#   
#   ylab("REP 2") +
#   xlab("REP 1") +
#   # Customize axis breaks
#   scale_y_continuous(breaks = seq(0,300, by = 20))+
#   scale_x_continuous(breaks = seq(0,300, by = 20)) +
#   labs(title = "Correlation between experiement reps")
# 
# 
# plot_canopy_mask_VI
# 
# 
# #### Correlation table ####
# 
# names(df)
# selected.metrics <- c("r","MAE","RMSE", "R2")
# 
# metrics.sum1 <- df %>% 
#   metrics_summary( obs = StandCount_gr, pred = boxes_gt,
#                    type = "regression")  %>% 
#   filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
#   mutate(Method = c("Boxes_gt_x_GT"))
#   
# metrics.sum2 <- df %>% 
#   metrics_summary( obs = boxes_gt, pred = pred_count_01,
#                    type = "regression")  %>% 
#   filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
#   mutate(Method = c("Boxes_gt_x_pred_count")) 
# 
# metrics.sum3 <- df %>% 
#   metrics_summary( obs = boxes_gt, pred = pred_count_10m_6_13_22,
#                    type = "regression")  %>% 
#   filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
#   mutate(Method = c("Boxes_gt_x_pred_count2"))
# 
# metrics.sum4 <- df %>% 
#   metrics_summary( obs = boxes_gt, pred = pred_count_7m_6_10_22,
#                    type = "regression")  %>% 
#   filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
#   mutate(Method = c("Boxes_gt_x_pred_count3"))
# 
# 
# data_correl_final<- rbind(metrics.sum1, metrics.sum2, metrics.sum3,
#                           metrics.sum4
#                           )
# 
# #write.csv(data_correl_final, "data_correl_final.csv")
# 
# 
# 
