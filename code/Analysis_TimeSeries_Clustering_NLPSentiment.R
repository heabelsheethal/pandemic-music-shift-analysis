

# set working directory
setwd("C:/Users/celia/Downloads")


# ------------------------------------------------------------------------------------
# ************************* REQUIRED PACKAGES ****************************************
# ------------------------------------------------------------------------------------


library(ggplot2); library(dplyr) ; library(caret); library(xgboost); library(Metrics)
library(skimr) ; library(fastDummies); library(corrplot); library(tidyr); library(iml)
library(parallel); library(scales); library(magrittr); library(ggthemes); library(stringr)
library(tidytext); library(tm); library(rpart); library(rpart.plot); library(gridExtra)
library(wordcloud); library(RColorBrewer); library(gridExtra); library(grid)

if(!require(textdata,quietly = T)) 
  install.packages('textdata')
library(textdata)

if (!require("tm", quietly = TRUE)) {
  install.packages("tm")
}
library(tm)

# ------------------------------------------------------------------------------------
# ************************* READ DATASET ****************************************
# ------------------------------------------------------------------------------------

data = read.csv(file = "C://Users//celia//Downloads//APAN5205_project_raw data.csv")
# View(data)

# Additional dataset (contain lyrics used for text mining)
extra_data = read.csv(file = "C://Users//celia//Downloads//song_lyrics.csv")
# View(extra_data)

# ------------------------------------------------------------------------------------
# ************************* DATA EXPLORATION ****************************************
# ------------------------------------------------------------------------------------

head(data)
ncol(data)
nrow(data)
colnames(data) 
summary(data)

# create copy before any alterations
data_music_time <- data
data_music_text <- data

#Remove unnecessary columns 
data <- data %>%
  select(-playlist_url, -track_id, -artist_id, -album)

library(skimr) 
skim(data)  # from skimr package (seperates numeric and character variables)
# shows histogram of numeric variables, mean, sd
# shows number of missing values

# -------> Variable type: numeric 
# 1 year 
# 2 track_popularity     
# 3 artist_popularity         
# 4 danceability        
# 5 energy          
# 6 key                 
# 7 loudness         
# 8 mode                   
# 9 speechiness             
# 10 acousticness           
# 11 instrumentalness            
# 12 liveness                 
# 13 valence                  
# 14 tempo                    
# 15 duration_ms              
# 16 time_signature 


# ------> Variable type: character
# 1 track_name         
# 2 artist_name        
# 3 artist_genres 

# ------------------------------------------------------------------------------------
# ************** Creating List of Categorical and Numerical columns ********************
# ------------------------------------------------------------------------------------

col_list = colnames(data)     # List of column Names

num_list <- c()                 # Initialize numeric variable list
cat_list <- c()                 # Initialize categorical variable list 

for (i in col_list) {
  if (is.numeric(data[[i]])) {
    num_list <- c(num_list, i)  # Append truly numeric columns
  } 
  else{
    cat_list <- c(cat_list, i)   # Append to categorical variable list
    
  }
}

col_list
num_list
cat_list

cat_df <- data[, cat_list, drop = FALSE]  
num_df <- data[, num_list, drop = FALSE]

head(cat_df)
head(num_df)


# ------------------------------------------------------------------------------------
# ************************* VISUALIZATION ****************************************
# ------------------------------------------------------------------------------------
library(dplyr); library(tidyr); library(ggplot2); library(tidyverse)


# BARCHART of all Numeric Columns
data %>%
  select_if(is.numeric) %>%
  pivot_longer(cols = 1:16, names_to = 'numeric_predictor', values_to = 'values') %>%
  ggplot(aes(x = values)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) + # Consistent color
  facet_wrap(~ numeric_predictor, scales = 'free') +
  theme_bw()



# HISTOGRAM of Categorical columns with less than 10 unioque values

data |> 
  mutate(across(all_of(cat_list), as.factor)) %>%         # Convert Categorical columns as factors
  select_if(is.factor)%>%
  pivot_longer(cols = everything(),names_to = 'categorical_predictor', values_to = 'values'  )%>%
  group_by(categorical_predictor, values)%>%
  count()%>%
  ungroup()%>%
  ggplot(aes(x = values, y = n, fill = values)) + 
  geom_col() +
  facet_wrap(categorical_predictor ~ ., scales = 'free') +
  theme_bw() +
  theme(legend.position = "none")  # Removes the legend


# ------------------------------------------------------------------------------------
# ************************* Check duplicates in dataset ****************************
# ------------------------------------------------------------------------------------  

# Check duplicates
data[duplicated(data), ]

# Output: No duplicates found  

# ------------------------------------------------------------------------------------
# ************************* Check Missing data *************************************
# ------------------------------------------------------------------------------------

# Check number of missing data
skim(data)


# Analyse Missing values
colSums(is.na(data))                       # No. of missing values in each column
missing_percentages <- colSums(is.na(data)) / nrow(data) * 100  # Percentage of missing values 
missing_percentages
# Output: No missing data

# --------------------------------------------------------------------------------------------------------------
# **************************** CORRELATION ANALYSIS (NUMERICAL VARIABLES only) ****************************************
# --------------------------------------------------------------------------------------------------------------

# correlation table of numerical values
library(corrplot)
cor_table = round(cor(num_df, method = c("pearson")),2)   # num_df is the numerical column dataframe
cor_table

# correlation plot
corrplot(cor_table)


corrplot(cor_table, 
         method = "color", 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         type = "upper", 
         order = "hclust", 
         addCoef.col = "black",
         tl.col = "darkblue", 
         tl.srt = 45, 
         tl.cex = 0.7,  # Reduce label text size
         number.cex = 0.6,  # Reduce the size of correlation coefficients
         diag = FALSE, 
         cl.lim = c(-1, 1),
         cl.ratio = 0.2, 
         main = "Correlation Plot",
         mar = c(0, 0, 1, 0))


# ------------------------------------------------------------------------------------
# ************************* Min-Max Scaling ****************************************
# ------------------------------------------------------------------------------------
# Ensure num_df contains only numeric columns
num_df <- data[, sapply(data, is.numeric), drop = FALSE]

# Remove the 'year' column before scaling
num_df <- num_df[, !names(num_df) %in% "year"]

# Apply Min-Max Scaling (range 0 to 1)
preProc <- preProcess(num_df, method = "range")  
num_df_scaled <- predict(preProc, num_df)

# Verify scaling worked (min should be 0, max should be 1)
summary(num_df_scaled)

# Add the 'year' column back to the scaled data
num_df_scaled$year <- data$year

# Combine scaled numeric data with non-numeric columns
final_data <- cbind(cat_df, num_df_scaled)

# View the final data
head(final_data)
# View(final_data)

# ------------------------------------------------------------------------------------
# *********** VISUALIZATION after scaling numerical variables***************************
# ------------------------------------------------------------------------------------
library(dplyr); library(tidyr); library(ggplot2); library(tidyverse)


# BARCHART of all Numeric Columns
final_data %>%
  select_if(is.numeric) %>%
  pivot_longer(cols = 1:16, names_to = 'numeric_predictor', values_to = 'values') %>%
  ggplot(aes(x = values)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) + # Consistent color
  facet_wrap(~ numeric_predictor, scales = 'free') +
  theme_bw()


#Analyzing various numeric variables by year to determine any potential patterns

library(ggplot2)
library(dplyr)
library(tidyr)

# Define the variables to analyze
numeric_vars <- c("danceability", "energy", "key", "loudness", "mode", 
                  "speechiness", "acousticness", "instrumentalness", 
                  "liveness", "valence", "tempo")

# Filter the data for years 2015 - 2023
plot_data <- final_data %>%
  filter(year >= 2015 & year <= 2023) %>%
  select(year, all_of(numeric_vars)) %>%
  pivot_longer(cols = -year, names_to = "Metric", values_to = "Value") %>%
  group_by(year, Metric) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE), .groups = "drop")

# Create the bar chart
ggplot(plot_data, aes(x = as.factor(year), y = mean_value, fill = Metric)) +
  geom_col(position = "dodge") +  # Use "dodge" for side-by-side bars
  theme_minimal() +
  labs(title = "Mean Music Feature Values by Year (2015-2023)",
       x = "Year",
       y = "Mean Value",
       fill = "Feature") +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Paired")  # Add color differentiation

#Line Plot 

ggplot(plot_data, aes(x = year, y = mean_value, color = Metric)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") + # Highlight COVID-19
  theme_minimal() +
  labs(title = "Trends in Music Features (2015-2023)",
       x = "Year",
       y = "Mean Value",
       color = "Feature")


# **************************************************************************************************
# **************************************************************************************************
# ------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ TIMESERIES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------------------------------------------------------
# **************************************************************************************************
# **************************************************************************************************

#>>>>>>>>>>> dataset to use: 'data_music' (this is same as raw data, can directly use it there is no missing values)  <<<<<<<<<<<<<<<<<
#>>>>>>>>>>> dataset to use: 'final_data' (this is scaled data with removed columns: playlist_url, track_id, artist_id, album  )  <<<<<<<<<<<<<<<<<


#>>>>>>>>>>> At this stage: we have added all r code of cleaning and visualizations above (from proposal) <<<<<<<<<<<<<<<<<

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)

# ----------------------------------------------------------------------------
# *********** Data Preparation ***********************************************
# ----------------------------------------------------------------------------


spotify_final_data <- data_music_time

spotify_final_data <- spotify_final_data %>%
  select(-playlist_url, -track_id, -artist_id)

str(spotify_final_data)
head(spotify_final_data)
# View(spotify_final_data)

# ----------------------------------------------------------------------------
# *********** INITIAL VISUALIZATION of Trends ***************************
# ----------------------------------------------------------------------------


# Define features of interest
selected_features <- c("danceability", "energy", "key", "acousticness", "valence", "tempo")

# Prepare the data
avg_features <- spotify_final_data %>%
  filter(year >= 2017 & year <= 2023) %>%
  group_by(year) %>%
  summarize(across(all_of(selected_features), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = all_of(selected_features), names_to = "feature", values_to = "value")

#Plot key features by year to determine which ones took the biggest hit with Covid
ggplot(avg_features, aes(x = year, y = value)) +
  # Light blue shaded region for COVID years
  annotate("rect", xmin = 2020, xmax = 2023, ymin = -Inf, ymax = Inf, 
           alpha = 0.15, fill = "lightblue") +
  
  # Add dashed line only
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red", size = 0.5) +
  
  # Add one overall annotation ABOVE the entire plot
  annotate("text", x = 2020, y = Inf, label = "COVID-19 declared a pandemic\nMarch 2020", 
           vjust = -0.5, hjust = 0.5, color = "red", size = 4) +
  
  # Main plot
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  
  # Separate plots by feature (2 columns to reduce vertical squish)
  facet_wrap(~ feature, scales = "free_y", ncol = 2) +
  
  # Theme tweaks for readability
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16),
    panel.spacing = unit(1.2, "lines")  # increase spacing between facets
  ) +
  
  labs(title = "Tracking the Emotional Pulse of Music Through the Pandemic Years",
       x = "Year",
       y = "Average Value")


#Create time series for features of interest from 2015 - 2023 

#Filter for years 2015 - 2023 
spotify_filtered <- spotify_final_data %>%
  filter(year >= 2015 & year <= 2023)

#Aggregate audio features by year, calculate mean 
spotify_yearly_avg <- spotify_filtered %>%
  group_by(year) %>%
  summarise(
    danceability = mean(danceability, na.rm = TRUE),
    acousticness = mean(acousticness, na.rm = TRUE),
    energy = mean(energy, na.rm = TRUE),
    key = mean(key, na.rm = TRUE),
    tempo = mean(tempo, na.rm = TRUE),
    valence = mean(valence, na.rm = TRUE)
  )

#Convert to time series objects
danceability_ts <- ts(spotify_yearly_avg$danceability, start = 2015, end = 2023, frequency = 1)
acousticness_ts <- ts(spotify_yearly_avg$acousticness, start = 2015, end = 2023, frequency = 1)
energy_ts <- ts(spotify_yearly_avg$energy, start = 2015, end = 2023, frequency = 1)
key_ts <- ts(spotify_yearly_avg$key, start = 2015, end = 2023, frequency = 1)
tempo_ts <- ts(spotify_yearly_avg$tempo, start = 2015, end = 2023, frequency = 1)
valence_ts <- ts(spotify_yearly_avg$valence, start = 2015, end = 2023, frequency = 1)

#Plot time series objects 
plot(danceability_ts, main = "Average Danceability (2015–2023)", ylab = "Danceability", xlab = "Year")
plot(acousticness_ts, main = "Average Acousticness (2015–2023)", ylab = "Acousticness", xlab = "Year")
plot(energy_ts, main = "Average Energy (2015–2023)", ylab = "Energy", xlab = "Year")
plot(key_ts, main = "Average Key (2015–2023)", ylab = "Key", xlab = "Year")
plot(tempo_ts, main = "Average Tempo (2015–2023)", ylab = "Tempo", xlab = "Year")
plot(valence_ts, main = "Average Valence (2015–2023)", ylab = "Valence", xlab = "Year")

# ----------------------------------------------------------------------------
# ********************** Autocorrelation Analysis ***************************
# ----------------------------------------------------------------------------
library(dplyr)

#Aggregate data annually 
spotify_yearly_avg <- spotify_final_data %>%
  filter(year >= 2000 & year <= 2023) %>%
  group_by(year) %>%
  summarise(
    danceability = mean(danceability, na.rm = TRUE),
    acousticness = mean(acousticness, na.rm = TRUE),
    valence = mean(valence, na.rm = TRUE),
    key = mean(key, na.rm = TRUE),
    energy = mean(energy, na.rm = TRUE),
    tempo = mean(tempo, na.rm = TRUE)
  )

### Valence -------------------------------------------------------------------

#Create a time series object for valence 
valence_ts <- ts(spotify_yearly_avg$valence, start = 2000, frequency = 1)

#Perform Autocorrelation Analysis 

ggAcf(valence_ts, lag.max = 10) +
  ggtitle("Autocorrelation of Valence (2000–2023)") +
  xlab("Lag (Years)") +
  ylab("Autocorrelation")

#Split pre/post-Covid 
valence_pre_covid <- ts(spotify_yearly_avg$valence[spotify_yearly_avg$year < 2020], 
                        start = 2000, frequency = 1)
valence_post_covid <- ts(spotify_yearly_avg$valence[spotify_yearly_avg$year >= 2020], 
                         start = 2020, frequency = 1)

ggAcf(valence_pre_covid, lag.max = 5) + ggtitle("Valence Autocorrelation (Pre-COVID)")
ggAcf(valence_post_covid, lag.max = 3) + ggtitle("Valence Autocorrelation (Post-COVID)")

#Drift model 

# Subset valence values from 2000 to 2019
valence_pre_covid_ts <- ts(spotify_yearly_avg$valence[spotify_yearly_avg$year <= 2019], 
                           start = 2000, frequency = 1)
# Fit the drift model
drift_model <- rwf(valence_pre_covid_ts, h = 4, drift = TRUE)

drift_model

# Actual observed values from 2020–2023
actual_valence_post_covid <- spotify_yearly_avg %>%
  filter(year >= 2020) %>%
  select(year, valence)

#Compare predicted vs. actual

#Convert actual post-COVID valence values to a time series
actual_valence_ts <- ts(actual_valence_post_covid$valence, 
                        start = 2020, frequency = 1)

#Plot forecast vs. actual
autoplot(drift_model) +
  autolayer(actual_valence_ts, series = "Actual", color = "red") +
  ggtitle("Valence Forecast (Drift Model) vs. Actual (2020–2023)") +
  xlab("Year") + ylab("Valence") +
  scale_x_continuous(breaks = seq(2000, 2023, 1)) +
  theme_minimal()

#Residual analysis 

# Get predicted values for 2020–2023
forecasted_vals <- as.numeric(drift_model$mean)

# Actual observed valence values for 2020–2023
actual_vals <- spotify_yearly_avg %>%
  filter(year >= 2020) %>%
  pull(valence)

#Calculate residuals for 2020–2023
residuals_post_covid <- actual_vals - forecasted_vals

# Predicted (fitted) values for 2000–2019 from the model
fitted_vals <- fitted(rwf(valence_pre_covid_ts, drift = TRUE))

# Actual values for 2000–2019
actual_pre_covid <- spotify_yearly_avg %>%
  filter(year <= 2019) %>%
  pull(valence)

# Residuals for pre-COVID years
residuals_pre_covid <- actual_pre_covid - fitted_vals

#Run the Wilcoxon rank-sum test
wilcox.test(residuals_post_covid, residuals_pre_covid, 
            alternative = "greater")


### Danceability Analysis ---------------------------------------------------

# Create a time series object
danceability_ts <- ts(spotify_yearly_avg$danceability, start = 2000, frequency = 1)

# Autocorrelation Analysis
ggAcf(danceability_ts, lag.max = 10) +
  ggtitle("Autocorrelation of Danceability (2000–2023)") +
  xlab("Lag (Years)") + ylab("Autocorrelation")

# Split pre/post-COVID
danceability_pre_covid <- ts(spotify_yearly_avg$danceability[spotify_yearly_avg$year < 2020], start = 2000, frequency = 1)
danceability_post_covid <- ts(spotify_yearly_avg$danceability[spotify_yearly_avg$year >= 2020], start = 2020, frequency = 1)

ggAcf(danceability_pre_covid, lag.max = 5) + ggtitle("Danceability Autocorrelation (Pre-COVID)")
ggAcf(danceability_post_covid, lag.max = 3) + ggtitle("Danceability Autocorrelation (Post-COVID)")

# Drift Model
danceability_pre_covid_ts <- ts(spotify_yearly_avg$danceability
                                [spotify_yearly_avg$year <= 2019], start = 2000, 
                                frequency = 1)
drift_model_danceability <- rwf(danceability_pre_covid_ts, h = 4, drift = TRUE)

# Actual observed values (2020–2023)
actual_danceability_post_covid <- spotify_yearly_avg %>%
  filter(year >= 2020) %>%
  select(year, danceability)

actual_danceability_ts <- ts(actual_danceability_post_covid$danceability, start = 2020, frequency = 1)

# Forecast vs. Actual Plot
autoplot(drift_model_danceability) +
  autolayer(actual_danceability_ts, series = "Actual", color = "red") +
  ggtitle("Danceability Forecast (Drift Model) vs. Actual (2020–2023)") +
  xlab("Year") + ylab("Danceability") +
  scale_x_continuous(breaks = seq(2000, 2023, 1)) +
  theme_minimal()

# Residual Analysis
forecasted_danceability <- as.numeric(drift_model_danceability$mean)
actual_danceability_vals <- spotify_yearly_avg %>%
  filter(year >= 2020) %>%
  pull(danceability)
residuals_post_covid_danceability <- actual_danceability_vals - forecasted_danceability

fitted_vals_danceability <- fitted(rwf(danceability_pre_covid_ts, drift = TRUE))
actual_pre_covid_danceability <- spotify_yearly_avg %>%
  filter(year <= 2019) %>%
  pull(danceability)
residuals_pre_covid_danceability <- actual_pre_covid_danceability - fitted_vals_danceability

# Wilcoxon Rank-Sum Test
wilcox.test(residuals_post_covid_danceability, residuals_pre_covid_danceability,
            alternative = "less")

### Energy Analysis -----------------------------------------------------------

# Create a time series object
energy_ts <- ts(spotify_yearly_avg$energy, start = 2000, frequency = 1)

# Split pre/post-COVID
energy_pre_covid <- ts(spotify_yearly_avg$energy[spotify_yearly_avg$year < 2020], start = 2000, frequency = 1)
energy_post_covid <- ts(spotify_yearly_avg$energy[spotify_yearly_avg$year >= 2020], start = 2020, frequency = 1)

ggAcf(energy_pre_covid, lag.max = 5) + ggtitle("Energy Autocorrelation (Pre-COVID)")
ggAcf(energy_post_covid, lag.max = 3) + ggtitle("Energy Autocorrelation (Post-COVID)")

# Drift Model
energy_pre_covid_ts <- ts(spotify_yearly_avg$energy[spotify_yearly_avg$year <= 2019], start = 2000, frequency = 1)
drift_model_energy <- rwf(energy_pre_covid_ts, h = 4, drift = TRUE)

# Actual observed values (2020–2023)
actual_energy_post_covid <- spotify_yearly_avg %>%
  filter(year >= 2020) %>%
  select(year, energy)

actual_energy_ts <- ts(actual_energy_post_covid$energy, start = 2020, frequency = 1)

# Forecast vs. Actual Plot
autoplot(drift_model_energy) +
  autolayer(actual_energy_ts, series = "Actual", color = "red") +
  ggtitle("Energy Forecast (Drift Model) vs. Actual (2020–2023)") +
  xlab("Year") + ylab("Energy") +
  scale_x_continuous(breaks = seq(2000, 2023, 1)) +
  theme_minimal()

# Residual Analysis
forecasted_energy <- as.numeric(drift_model_energy$mean)
actual_energy_vals <- spotify_yearly_avg %>%
  filter(year >= 2020) %>%
  pull(energy)
residuals_post_covid_energy <- actual_energy_vals - forecasted_energy

fitted_vals_energy <- fitted(rwf(energy_pre_covid_ts, drift = TRUE))
actual_pre_covid_energy <- spotify_yearly_avg %>%
  filter(year <= 2019) %>%
  pull(energy)
residuals_pre_covid_energy <- actual_pre_covid_energy - fitted_vals_energy

# Wilcoxon Rank-Sum Test
wilcox.test(residuals_post_covid_energy, residuals_pre_covid_energy, 
            alternative = "greater")

### Tempo Analysis ------------------------------------------------------------

# Create a time series object
tempo_ts <- ts(spotify_yearly_avg$tempo, start = 2000, frequency = 1)

# Split pre/post-COVID
tempo_pre_covid <- ts(spotify_yearly_avg$tempo[spotify_yearly_avg$year < 2020], start = 2000, frequency = 1)
tempo_post_covid <- ts(spotify_yearly_avg$tempo[spotify_yearly_avg$year >= 2020], start = 2020, frequency = 1)

ggAcf(tempo_pre_covid, lag.max = 5) + ggtitle("Tempo Autocorrelation (Pre-COVID)")
ggAcf(tempo_post_covid, lag.max = 3) + ggtitle("Tempo Autocorrelation (Post-COVID)")

# Drift Model
tempo_pre_covid_ts <- ts(spotify_yearly_avg$tempo[spotify_yearly_avg$year <= 2019], start = 2000, frequency = 1)
drift_model_tempo <- rwf(tempo_pre_covid_ts, h = 4, drift = TRUE)

# Actual observed values (2020–2023)
actual_tempo_post_covid <- spotify_yearly_avg %>%
  filter(year >= 2020) %>%
  select(year, tempo)

actual_tempo_ts <- ts(actual_tempo_post_covid$tempo, start = 2020, frequency = 1)

# Forecast vs. Actual Plot
autoplot(drift_model_tempo) +
  autolayer(actual_tempo_ts, series = "Actual", color = "red") +
  ggtitle("Tempo Forecast (Drift Model) vs. Actual (2020–2023)") +
  xlab("Year") + ylab("Tempo") +
  scale_x_continuous(breaks = seq(2000, 2023, 1)) +
  theme_minimal()

# Residual Analysis
forecasted_tempo <- as.numeric(drift_model_tempo$mean)
actual_tempo_vals <- spotify_yearly_avg %>%
  filter(year >= 2020) %>%
  pull(tempo)
residuals_post_covid_tempo <- actual_tempo_vals - forecasted_tempo

fitted_vals_tempo <- fitted(rwf(tempo_pre_covid_ts, drift = TRUE))
actual_pre_covid_tempo <- spotify_yearly_avg %>%
  filter(year <= 2019) %>%
  pull(tempo)
residuals_pre_covid_tempo <- actual_pre_covid_tempo - fitted_vals_tempo

# Wilcoxon Rank-Sum Test
# Choose "greater", "less", or "two.sided" based on your visual drift model results
wilcox.test(residuals_post_covid_tempo, residuals_pre_covid_tempo, alternative = "less")


# **************************************************************************************************
# **************************************************************************************************
# ------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CLUSTERING ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------------------------------------------------------
# **************************************************************************************************
# **************************************************************************************************

#>>>>>>>>>>> dataset to use: 'data_music' (this is same as raw data, can directly use it there is no missing values)  <<<<<<<<<<<<<<<<<
#>>>>>>>>>>> dataset to use: 'final_data' (this is scaled data with removed columns: playlist_url, track_id, artist_id, album  )  <<<<<<<<<<<<<<<<<


#>>>>>>>>>>> At this stage: we have added all r code of cleaning and visualizations above (from proposal) <<<<<<<<<<<<<<<<<

# ------------------------------------------------------------------------------------
# ******** Analysis of Music Trends: Pre-COVID, During COVID, and Post-COVID *********
# ------------------------------------------------------------------------------------

# 1. Load data and initial checks

# Load cleaned data 
# Load cleaned data 
data_cluster  = read.csv("C://Users//celia//Downloads//APAN5205_Project_FinalData.csv")
data_cluster <- final_data

# View the first few roads of the data 
head(data_cluster)

# Checking for missing values in the dataset
sum(is.na(data_cluster))

# Split data into pre-COVID and post-COVID based on the year
pre_covid_data_cluster <- data_cluster %>% filter(year < 2020)
during_covid_data_cluster <- data_cluster %>% filter(year == 2020)
post_covid_data_cluster <- data_cluster %>% filter(year > 2020)

# 2. Normalize data (standarize features)

# Normalize pre-COVID data
normalize_pre_covid <- scale(pre_covid_data_cluster[, c("energy", "valence", "danceability", "tempo", 
                                                        "loudness", "acousticness", "speechiness", "instrumentalness", "liveness", "mode")])

# Normalize during-COVID data (2020)
normalize_during_covid <- scale(during_covid_data_cluster[, c("energy", "valence", "danceability", "tempo", 
                                                              "loudness", "acousticness", "speechiness", "instrumentalness", "liveness", "mode")])

# Normalize post-COVID data
normalize_post_covid <- scale(post_covid_data_cluster[, c("energy", "valence", "danceability", "tempo", "loudness", "acousticness", "speechiness", "instrumentalness", "liveness", "mode")])

# 3. Elbow method

# Elbow method for pre-COVID data
wss_pre <- sapply(1:10, function(k) {
  kmeans(normalize_pre_covid, centers = k, nstart = 25)$tot.withinss
})
plot(1:10, wss_pre, type = "b", pch = 15, frame = FALSE, xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares")

# Elbow method for during-COVID (2020) data
wss_during <- sapply(1:10, function(k) {
  kmeans(normalize_during_covid, centers = k, nstart = 25)$tot.withinss
})
plot(1:10, wss_during, type = "b", pch = 15, frame = FALSE, xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares")

# Elbow method for post-COVID data
wss_post <- sapply(1:10, function(k) {
  kmeans(normalize_post_covid, centers = k, nstart = 25)$tot.withinss
})
plot(1:10, wss_post, type = "b", pch = 15, frame = FALSE, xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares")

# 4. Ratio plot for cluster separation

# Ratio plot for pre-COVID data
ratio_ss_pre <- sapply(1:10, function(k) {
  km <- kmeans(normalize_pre_covid, centers = k, nstart = 25)
  km$betweenss / km$totss
})
ggplot(data = data.frame(cluster = 1:10, ratio_ss = ratio_ss_pre), aes(x = cluster, y = ratio_ss)) +
  geom_line(col = 'steelblue', size = 1.2) +
  geom_point() +
  xlab("Number of Clusters") + ylab("Ratio SS") + ggtitle("Ratio Plot for Pre-COVID K-means Clustering")

# Ratio plot for during-COVID (2020) data
ratio_ss_during <- sapply(1:10, function(k) {
  km <- kmeans(normalize_during_covid, centers = k, nstart = 25)
  km$betweenss / km$totss
})

ggplot(data = data.frame(cluster = 1:10, ratio_ss = ratio_ss_during), aes(x = cluster, y = ratio_ss)) +
  geom_line(col = 'steelblue', size = 1.2) +
  geom_point() +
  xlab("Number of Clusters") + ylab("Ratio SS") + ggtitle("Ratio Plot for During-COVID K-means Clustering")

# Ratio plot for post-COVID data
ratio_ss_post <- sapply(1:10, function(k) {
  km <- kmeans(normalize_post_covid, centers = k, nstart = 25)
  km$betweenss / km$totss
})
ggplot(data = data.frame(cluster = 1:10, ratio_ss = ratio_ss_post), aes(x = cluster, y = ratio_ss)) +
  geom_line(col = 'steelblue', size = 1.2) +
  geom_point() +
  xlab("Number of Clusters") + ylab("Ratio SS") + ggtitle("Ratio Plot for Post-COVID K-means Clustering")

# 5. Silhouette Analysis 
library(cluster)

# Silhouette analysis for pre-COVID data
dist_matrix_pre <- dist(normalize_pre_covid)
sil_width_pre <- sapply(2:10, function(k) {
  kmeans_model <- kmeans(normalize_pre_covid, centers = k, nstart = 25)
  sil <- silhouette(kmeans_model$cluster, dist_matrix_pre)
  mean(sil[, 3])
})
plot(2:10, sil_width_pre, type = "b", pch = 15, xlab = "Number of Clusters", ylab = "Average Silhouette Width")

# Silhouette analysis for during-COVID (2020) data
dist_matrix_during <- dist(normalize_during_covid)
sil_width_during <- sapply(2:10, function(k) {
  kmeans_model <- kmeans(normalize_during_covid, centers = k, nstart = 25)
  sil <- silhouette(kmeans_model$cluster, dist_matrix_during)
  mean(sil[, 3])
})
plot(2:10, sil_width_during, type = "b", pch = 15, xlab = "Number of Clusters", ylab = "Average Silhouette Width")

# Silhouette analysis for post-COVID data
dist_matrix_post <- dist(normalize_post_covid)
sil_width_post <- sapply(2:10, function(k) {
  kmeans_model <- kmeans(normalize_post_covid, centers = k, nstart = 25)
  sil <- silhouette(kmeans_model$cluster, dist_matrix_post)
  mean(sil[, 3])
})
plot(2:10, sil_width_post, type = "b", pch = 15, xlab = "Number of Clusters", ylab = "Average Silhouette Width")

# 6. K-means Clustering and 2D Vizualization

# Perform K-means clustering on pre-COVID data 
set.seed(617)
kmeans_model_pre <- kmeans(normalize_pre_covid, centers = 2, nstart = 25)
pre_covid_data_cluster$cluster <- kmeans_model_pre$cluster

# Perform PCA on the normalized data
pca_result <- prcomp(normalize_pre_covid, scale. = TRUE)

# Visualize clusters in 2D using the first two principal components
library(factoextra)
fviz_cluster(kmeans_model_pre, data = pca_result$x[, 1:2], geom = "point")

# Perform K-means clustering on during pandemic data 
set.seed(617)
kmeans_model_during <- kmeans(normalize_during_covid, centers = 2, nstart = 25)
during_covid_data_cluster$cluster <- kmeans_model_during$cluster

# Perform PCA on the normalized data
pca_result_during <- prcomp(normalize_during_covid, scale. = TRUE)

# Visualize clusters in 2D using the first two principal components
library(factoextra)
fviz_cluster(kmeans_model_during, data = pca_result_during$x[, 1:2], geom = "point")

# Perform K-means clustering on post-COVID data (assuming 3 clusters)
set.seed(617)
kmeans_model_post <- kmeans(normalize_post_covid, centers = 2, nstart = 25)
post_covid_data_cluster$cluster <- kmeans_model_post$cluster

# Perform PCA on the normalized data
pca_result <- prcomp(normalize_post_covid, scale. = TRUE)

# Visualize clusters in 2D using the first two principal components
library(factoextra)
fviz_cluster(kmeans_model_post, data = pca_result$x[, 1:2], geom = "point")

# 7. Cluster summary by features

# Summary for pre-COVID clusters
cluster_summary_pre <- pre_covid_data_cluster %>%
  group_by(cluster) %>%
  summarize(across(c(energy, valence, danceability, tempo, loudness, acousticness, 
                     speechiness, instrumentalness, liveness, mode), mean))
print(cluster_summary_pre)

# Summary for during-COVID (2020) clusters
cluster_summary_during <- during_covid_data_cluster %>%
  group_by(cluster) %>%
  summarize(across(c(energy, valence, danceability, tempo, loudness, acousticness, 
                     speechiness, instrumentalness, liveness, mode), mean))
print(cluster_summary_during)

# Summary for post-COVID clusters
cluster_summary_post <- post_covid_data_cluster %>%
  group_by(cluster) %>%
  summarize(across(c(energy, valence, danceability, tempo, loudness, acousticness, 
                     speechiness, instrumentalness, liveness, mode), mean))
print(cluster_summary_post)

# 8. Hierarchical Clustering

# Hierarchical Clustering for pre-COVID data
dist_matrix_pre <- dist(normalize_pre_covid)
hc_pre <- hclust(dist_matrix_pre, method = "ward.D2")
plot(hc_pre, main = "Dendrogram of Pre-COVID Clustering", xlab = "", sub = "")

# Hierarchical Clustering for during-COVID (2020) data
dist_matrix_during <- dist(normalize_during_covid)
hc_during <- hclust(dist_matrix_during, method = "ward.D2")
plot(hc_during, main = "Dendrogram of During-COVID Clustering", xlab = "", sub = "")

# Hierarchical Clustering for post-COVID data
dist_matrix_post <- dist(normalize_post_covid)
hc_post <- hclust(dist_matrix_post, method = "ward.D2")
plot(hc_post, main = "Dendrogram of Post-COVID Clustering", xlab = "", sub = "")

# 9. GMM lustering using mclust

# GMM for pre-COVID data
library(mclust)
bic_values_pre <- sapply(2:10, function(k) {
  model <- Mclust(normalize_pre_covid, G = k)
  model$bic
})
plot(2:10, bic_values_pre, type = "b", pch = 15, xlab = "Number of Clusters", ylab = "BIC Value")

# GMM for during-COVID (2020) data
library(mclust)
bic_values_during <- sapply(2:10, function(k) {
  model <- Mclust(normalize_during_covid, G = k)
  model$bic
})
plot(2:10, bic_values_during, type = "b", pch = 15, xlab = "Number of Clusters", ylab = "BIC Value")

# GMM for post-COVID data
bic_values_post <- sapply(2:10, function(k) {
  model <- Mclust(normalize_post_covid, G = k)
  model$bic
})
plot(2:10, bic_values_post, type = "b", pch = 15, xlab = "Number of Clusters", ylab = "BIC Value")

# 10. Comparing all clustering results

# Combine cluster assignments from all methods for pre-COVID
pre_covid_data_cluster$kmeans_cluster <- kmeans_model_pre$cluster
pre_covid_data_cluster$gmm_cluster <- Mclust(normalize_pre_covid, G = 2)$classification
pre_covid_data_cluster$hc_cluster <- cutree(hc_pre, k = 2)

# Combine cluster assignments from all methods for during-COVID (2020)
during_covid_data_cluster$kmeans_cluster <- kmeans_model_during$cluster
during_covid_data_cluster$gmm_cluster <- Mclust(normalize_during_covid, G = 2)$classification
during_covid_data_cluster$hc_cluster <- cutree(hc_during, k = 2)

# Combine cluster assignments from all methods for post-COVID
post_covid_data_cluster$kmeans_cluster <- kmeans_model_post$cluster
post_covid_data_cluster$gmm_cluster <- Mclust(normalize_post_covid, G = 2)$classification
post_covid_data_cluster$hc_cluster <- cutree(hc_post, k = 2)

# Create comparison summary for pre-COVID
comparison_summary_pre <- pre_covid_data_cluster %>%
  select(kmeans_cluster, gmm_cluster, hc_cluster) %>%
  group_by(kmeans_cluster, gmm_cluster, hc_cluster) %>%
  summarise(count = n())

# Create comparison summary for during-COVID (2020)
comparison_summary_during <- during_covid_data_cluster %>%
  select(kmeans_cluster, gmm_cluster, hc_cluster) %>%
  group_by(kmeans_cluster, gmm_cluster, hc_cluster) %>%
  summarise(count = n())

# Create comparison summary for post-COVID
comparison_summary_post <- post_covid_data_cluster %>%
  select(kmeans_cluster, gmm_cluster, hc_cluster) %>%
  group_by(kmeans_cluster, gmm_cluster, hc_cluster) %>%
  summarise(count = n())

# Print comparison summaries
print(comparison_summary_pre)
print(comparison_summary_during)
print(comparison_summary_post)

# ------------------------------------------------------------------------------------
# ****************** Clustering Analysis of Music Trends (2000-2023) *****************
# ------------------------------------------------------------------------------------

# 1. Load data and initial checks

library(dplyr)

# Load cleaned data 
data_cluster  = read.csv("C://Users//celia//Downloads//APAN5205_Project_FinalData.csv")
data_cluster <- final_data

# View the first few roads of the data 
head(data_cluster)

# Checking for missing values in the dataset
sum(is.na(data_cluster))

# 2. Normalize data (standarize features)

normalize_data <- scale(data_cluster[, c("energy", "valence", "danceability", "tempo", 
                                      "loudness", "acousticness", "speechiness", 
                                      "instrumentalness", "liveness", "mode")])

# 3. Elbow method

# Compute the within-cluster sum of squares for different numbers of clusters
wss <- sapply(1:10, function(k) { 
  kmeans(normalize_data, centers = k, nstart = 25)$tot.withinss
})

# Plot the elbow graph
plot(1:10, wss, type = "b", pch = 15, frame = FALSE, xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares")

# 4. Ratio plot for cluster separation

# Compute the ratio of between-cluster SS to total SS for different k
ratio_ss <- sapply(1:10, function(k) { 
  km <- kmeans(normalize_data, centers = k, nstart = 25)
  km$betweenss / km$totss  # Ratio of between-cluster SS to total SS
})

# Plot the ratio plot
ggplot(data = data.frame(cluster = 1:10, ratio_ss = ratio_ss), aes(x = cluster, y = ratio_ss)) +
  geom_line(col = 'steelblue', size = 1.2) +
  geom_point() +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  xlab("Number of Clusters") +
  ylab("Ratio SS") +
  ggtitle("Ratio Plot for K-means Clustering")

# 5. Silhouette Analysis 

# Compute the distance matrix for the scaled data
dist_matrix <- dist(normalize_data)

# Compute silhouette widths for different cluster numbers
sil_width <- sapply(2:10, function(k) {
  kmeans_model <- kmeans(normalize_data, centers = k, nstart = 25)  # K-means clustering
  sil <- silhouette(kmeans_model$cluster, dist_matrix)  # Calculate silhouette
  mean(sil[, 3])  # Return the average silhouette width for each number of clusters
})

# Plot silhouette results
plot(2:10, sil_width, type = "b", pch = 15, xlab = "Number of Clusters", ylab = "Average Silhouette Width")

# 6. K-means Clustering and 2D Vizualization

# Perform K-means clustering
set.seed(617)
kmeans_model <- kmeans(normalize_data, centers = 2, nstart = 25)

# Add the cluster assignment to the original data
data_cluster$cluster <- kmeans_model$cluster

# 2D Plot using the first two principal components (PCA)
library(factoextra)
pca_result <- prcomp(normalize_data)
fviz_cluster(kmeans_model, data = pca_result$x[, 1:2], geom = "point")


# 7. Cluster summary by features

# Group by cluster and summarize key features
cluster_summary <- data_cluster %>%
  group_by(cluster) %>%
  summarize(across(c(energy, valence, danceability, tempo, loudness, acousticness, 
                     speechiness, instrumentalness, liveness, mode), mean))

print(cluster_summary)

# 8. Hierarchical Clustering

# Hierarchical Clustering (Ward's Method)
dist_matrix <- dist(normalize_data)  # Compute distance matrix
hc <- hclust(dist_matrix, method = "ward.D2")  # Hierarchical clustering using Ward's method

# Plot the dendrogram
plot(hc, main = "Dendrogram of Hierarchical Clustering", xlab = "", sub = "")

# 9. GMM lustering using mclust

# mclust
library(mclust)
# Fit GMM for different numbers of clusters (2 to 10)
bic_values <- sapply(2:10, function(k) {
  model <- Mclust(normalize_data, G = k)  # GMM with k clusters
  model$bic  # Extract BIC value
})

# Plot BIC values to find the optimal number of clusters
plot(2:10, bic_values, type = "b", pch = 15, xlab = "Number of Clusters", ylab = "BIC Value")

# 10. Comparing all clustering results

# Create data frames with cluster assignments from each model
data_cluster$kmeans_cluster <- kmeans_model$cluster
data_cluster$gmm_cluster <- Mclust(normalize_data, G = 3)$classification  # Assuming 3 clusters from GMM
data_cluster$hc_cluster <- cutree(hc, k = 3)  # Assuming 3 clusters from hierarchical clustering

# Create a summary table
comparison_summary <- data_clus %>%
  select(kmeans_cluster, gmm_cluster, hc_cluster) %>%
  group_by(kmeans_cluster, gmm_cluster, hc_cluster) %>%
  summarise(count = n())

print(comparison_summary)


# **************************************************************************************************
# **************************************************************************************************
# ------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ TEXT MINING ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------------------------------------------------------
# **************************************************************************************************
# **************************************************************************************************

# ------------------------------------------------------------------------------------
# ************************* READ ADDITIONAL DATASET **********************************
# ------------------------------------------------------------------------------------
# Reading additional dataset with "lyrics" to perform Text mining & sentiment analysis on song lyrics


# Initial dataset1
data1 = data_music_text       
nrow(data1) # 2400
colnames(data1)
unique(data1$year)
min(data1$year)  # 2000
max(data1$year)  # 2023


# Additional dataset
data2 = extra_data
data2
nrow(data2) # 5134856
colnames(data2)
unique(data2$year)


# ------------------------------------------------------------------------------------
# ******************************** Join additional datasets ***************************
# ------------------------------------------------------------------------------------

# Join dataset 1 and 2
data12 <- left_join(data1, data2, by = c("track_name" = "title","artist_name" = "artist"))
colnames(data12)

nrow(data12)    # 2400
colnames(data12)

# Rename 'tag' column as 'track_genres' , 'year.y' as 'popular year' , 'year.y' as 'release year'
data12 <- data12 %>%
  rename(track_genres = tag,
         year = year.x,
         release_year = year.y)

colnames(data12)


# ------------------------------------------------------------------------------------
# *********** Data cleaning: Remove rows where lyrics is missing (i.e., NA) ***************************
# ------------------------------------------------------------------------------------

# No of Missing values in lyrics row
sum(is.na(data12$lyrics))  # 1177


# Remove rows where lyrics is missing (i.e., NA)
data12_final <- data12 %>%
  filter(!is.na(lyrics))


# NOT same values in column "language_cld3"  & "language_ft"  &  "language"
data12_final %>%
  filter(!(language_cld3 == language_ft & language_cld3 == language))


# if column "language" is empty fill it with values from column "language_cld3"  & "language_ft"
data12_final <- data12_final %>%
  mutate(language = ifelse(is.na(language) | language == "",   # if language is missing or empty
                           coalesce(language_cld3, language_ft), # pick the first non-NA from cld3 or ft
                           language))   # else keep existing


# Removing column "language_cld3"  & "language_ft" as all 3 are almost same
data12_final <- data12_final %>%
  select(-language_cld3, -language_ft)




nrow(data12_final)         # 1223
colnames(data12_final)
unique(data12_final$year)   
min(data12_final$year)  # 2000
max(data12_final$year)  # 2023


# dataset of year 2020 to 2023
data12_20to23 <- data12_final %>%
  filter(year %in% c(2020, 2021, 2022, 2023))
nrow(data12_20to23)    # 110


# dataset of year 2016 to 2019
data12_16to19 <- data12_final %>%
  filter(year %in% c(2016, 2017, 2018, 2019))
nrow(data12_16to19)   # 205


# ------------------------------------------------------------------------------------
# ************************* DATA EXPLORATION ****************************************
# ------------------------------------------------------------------------------------

music_data <- data12_final 


head(music_data,3)
ncol(music_data)   # 30
nrow(music_data)   # 1223
colnames(music_data) 
summary(music_data)
str(music_data)


skim(music_data)  # from skimr package (seperates numeric and character variables)
# shows histogram of numeric variables, mean, sd
# shows number of missing values


# Popularity of songs 
music_data%>%
  summarize(average = mean(track_popularity), median = median(track_popularity))

# ----------------------------------------------
# Distribution of songs Popularity
ggplot(data=music_data,aes(x=track_popularity))+
  geom_histogram(fill='sienna3')+
  theme_bw()+
  scale_x_reverse()+
  xlab('track popularity')+
  coord_flip()

# ----------------------------------------------
# Average characters, words across all lyrics
music_data %>%
  select(lyrics)%>%
  mutate(characters = nchar(lyrics),
         words = str_count(lyrics,pattern='\\S+'))%>%
  summarize_at(c('characters','words'),.funs = mean,na.rm=T)

# ----------------------------------------------
# song with shortest lyrics
shortest_lyrics = which.min(str_count(string = music_data$lyrics,pattern = '\\S+'))
music_data$lyrics[shortest_lyrics]


# ----------------------------------------------
# song with longest lyrics
longest_lyrics = which.max(str_count(string = music_data$lyrics,pattern = '\\S+'))
music_data$lyrics[longest_lyrics]



#>>>>>>>>>>>> 'music_data' is dataset modified for text mining <<<<<<<<<<<


# ************************************************************************************
# ------------------------------------------------------------------------------------
# How did the thematic content of popular song lyrics change before and after the onset of COVID-19, 
# and what does this reveal about cultural or emotional shifts in society?
# ------------------------------------------------------------------------------------
# ************************************************************************************


# ------------------------------------------------------------------------------------
# *********************************** Text Mining ***********************************
# ------------------------------------------------------------------------------------


# ---------------TOKENISE-------------------------------
# Common words in lyrics (Removes stop words, 1 & 2 letter words)
data_words <- music_data%>%
          unnest_tokens(input = lyrics, output = word)%>%
          select(word)%>%
          anti_join(stop_words, by = "word") %>%
          filter(nchar(word) > 2) %>%
          anti_join(stop_words)%>%
          group_by(word)%>%
          summarize(count = n())%>%
          ungroup()%>%
          arrange(desc(count))%>%
          top_n(25, count)
data_words




# Visualize common words in lyrics after remove stop words
data_words %>%
  ggplot(aes(x = reorder(word, count), y = count, fill = count)) +
  geom_col() +
  scale_fill_gradient2(low = "#d9f0d3",  mid = "#a6dba0", high = "#1b7837", 
                    midpoint = median(data_words$count)) +
  coord_flip() +
  labs(
    title = "Common Words in Lyrics After Removing Stop Words",
    x = "Words",y = "Count") +
  theme_minimal()


# ----------------------------------------------
# Pre-COVID Common words in lyrics after removing stop words (from 2016 to 2019)
pre_covid <- music_data %>%
  filter(year>=2016 & year <= 2019) %>%
  unnest_tokens(input = lyrics, output = word) %>%
  filter(nchar(word) > 2) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(25, count) %>%
  mutate(period = "Pre-COVID")
pre_covid


# Post-COVID Common words in lyrics after removing stop words (from 2020 to 2023)
post_covid <- music_data %>%
  filter(year >= 2020 & year<=2023) %>%
  unnest_tokens(input = lyrics, output = word) %>%
  filter(nchar(word) > 2) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(25, count) %>%
  mutate(period = "Post-COVID")
post_covid 


# Combine both datasets
combined_words <- bind_rows(pre_covid, post_covid)
combined_words$period <- factor(combined_words$period, levels = c("Pre-COVID", "Post-COVID"))

# Plot - Visualize Pre-COVID & Post-COVID Common words in lyrics
ggplot(combined_words, aes(x = reorder(word, count), y = count, fill = period)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~period, scales = "free") +
  scale_fill_manual(values = c("Pre-COVID" = "#ff7f0e", "Post-COVID" = "#6baed6")) +
  coord_flip() +
  labs(x = "Word",y = "Count",title = "Top 25 Words in Song Lyrics: Pre vs Post COVID") +
  theme_minimal()

# word cloud for Pre-COVID & Post-COVID Common words in lyrics
# Reset plotting layout to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)


# Pre-COVID word cloud
wordcloud(words = pre_covid$word,
          freq = pre_covid$count,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          colors = colorRampPalette(c("#ffe5b4", "#ff7f0e"))(8),
          scale = c(2, 0.3))  # Reduced size
title("Pre-COVID Word Cloud", cex.main = 1)  # Smaller title

# Post-COVID word cloud
wordcloud(words = post_covid$word,
          freq = post_covid$count,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          colors = colorRampPalette(c("#deebf7", "#6baed6"))(8),
          scale = c(2, 0.3))  # Reduced size
title("Post-COVID Word Cloud", cex.main = 1)  # Smaller title



# ------------------------------------------------------------------------------------
# *********************************** Sentiment Analysis ***********************************
# ------------------------------------------------------------------------------------



# ************************************************************************************
# ------------------------------------------------------------------------------------
# Binary Sentiment Lexicon : Bing
# ------------------------------------------------------------------------------------
# ************************************************************************************

# Bing sentiment
as.data.frame(get_sentiments('bing'))[1:50,] 

get_sentiments('bing')%>%
  group_by(sentiment)%>%
  count()

# Overall positive and negative words & proportion of words in Lyrics
data_p_n <- music_data%>%
        group_by(track_id)%>%
        unnest_tokens(output = word, input = lyrics)%>%
        inner_join(get_sentiments('bing'))%>%
        group_by(sentiment)%>%
        summarize(n = n())%>%
        mutate(proportion = n/sum(n))
data_p_n


# Plot
data_p_n %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("negative" = "#d62728", "positive" = "#2ca02c")) +
  guides(fill = "none") +
  labs(title = "Distribution of Positive and Negative Words in Lyrics",
    x = "Sentiment",y = "Count") +
  theme_economist()


# ----------------------------------------------

# Pre-COVID positive and negative words in Lyrics 
pre_sentiment <- music_data %>%
  filter(year >= 2016 & year <= 2019) %>%
  unnest_tokens(word, lyrics) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(sentiment) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n),
         period = "Pre-COVID")
pre_sentiment



# Post-COVID positive and negative words in Lyrics 
post_sentiment <- music_data %>%
  filter(year >= 2020 & year <= 2023) %>%
  unnest_tokens(word, lyrics) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(sentiment) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n),
         period = "Post-COVID")
post_sentiment



# Combine the two datasets
combined_sentiment <- bind_rows(pre_sentiment, post_sentiment)

# Adjust factor levels to control the order of periods and sentiments
combined_sentiment$period <- factor(combined_sentiment$period, levels = c("Pre-COVID", "Post-COVID"))
combined_sentiment$sentiment <- factor(combined_sentiment$sentiment, levels = c("negative", "positive"))

# Plot with custom colors for positive and negative sentiments
ggplot(combined_sentiment, aes(x = sentiment, y = proportion, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~period, ncol = 2) +
  scale_fill_manual(values = c("negative" = "#d62728", "positive" = "#2ca02c")) +  # Custom colors
  labs(title = "Proportion of Sentiment in Song Lyrics",
       x = "Sentiment", y = "Proportion") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"),  # Title size of facets
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),  # Adjust x-axis text
        axis.text.y = element_text(size = 12),  # Adjust y-axis text
        axis.title = element_text(size = 14),  # Axis title size
        plot.title = element_text(size = 16, face = "bold"),  # Title size
        plot.subtitle = element_text(size = 14))  # Subtitle size



# ************************************************************************************
# ------------------------------------------------------------------------------------
# Emotion Lexicons : nrc
# ------------------------------------------------------------------------------------
# ************************************************************************************

# nrc Emotion
nrc = get_sentiments('nrc')  

nrc%>%
  group_by(sentiment)%>%
  count()

table(nrc$sentiment)  



# Emotions in lyrics
data_nrc <- music_data%>%
  group_by(track_id)%>%
  unnest_tokens(output = word, input = lyrics)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

data_nrc%>%
    arrange(desc(n))


# Plot
data_nrc %>%
  ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  coord_flip() +
  labs(title = "Top 10 Emotions in Lyrics") +
  theme_wsj() +
  theme(
    plot.title = element_text(size = 12, face = "bold")
  )


# ----------------------------------------------
# Pre-COVID emotions in Lyrics
data_nrc_pre <- music_data %>%
  filter(year >= 2016 & year <= 2019)%>%
  unnest_tokens(word, lyrics) %>%
  inner_join(nrc, by = "word")%>%
  group_by(sentiment) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n),
         period = "Pre-COVID")

data_nrc_pre%>%
  arrange(desc(n))


# Post-COVID emotions in Lyrics
data_nrc_post <- music_data %>%
  filter(year >= 2020 & year <= 2023)%>%
  unnest_tokens(word, lyrics) %>%
  inner_join(nrc, by = "word")%>%
  group_by(sentiment) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n),
         period = "Post-COVID")

data_nrc_post%>%
  arrange(desc(n))


# Combine the two datasets
combined_nrc <- bind_rows(data_nrc_pre, data_nrc_post)

# Calculate average proportion for ordering
sentiment_order <- combined_nrc %>%
  group_by(sentiment) %>%
  summarise(avg_proportion = mean(proportion)) %>%
  arrange(desc(avg_proportion)) %>%
  pull(sentiment)

# Apply factor level ordering
combined_nrc$sentiment <- factor(combined_nrc$sentiment, levels = sentiment_order)

# Plot
ggplot(combined_nrc, aes(x = sentiment, y = proportion, fill = period)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(
    title = "Emotions in Lyrics: Pre vs Post COVID",
    subtitle = "Proportionality analysis of emotion-linked words",
    x = "Emotion",
    y = "Proportion of Words",
    fill = "Period"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Pre-COVID" = "#ff7f0e", "Post-COVID" = "#6baed6")) +
  theme_wsj(base_size = 8) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )




# ************************************************************************************
# ------------------------------------------------------------------------------------
# Sentiment score Lexicons : afinn
# ------------------------------------------------------------------------------------
# ************************************************************************************

# affin
afinn = get_sentiments('afinn')  # or lexicon_afinn()

afinn[1:50,]

afinn %>%
  group_by(value)%>%
  count()

# Sentiment score of lyrics
data_affin <- music_data %>%
  select(track_id,lyrics)%>%
  group_by(track_id)%>%
  unnest_tokens(output=word,input=lyrics)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()

data_affin%>%
  summarize(min=min(reviewSentiment),
            max=max(reviewSentiment),
            median=median(reviewSentiment),
            mean=mean(reviewSentiment))


# Plot
data_affin %>%
  ggplot(aes(x = reviewSentiment, fill = reviewSentiment > 0)) +
  geom_histogram(binwidth = 0.1) +
  labs(
    title = "Overall Sentiment Scores in Lyrics",
    x = "Average Sentiment Score per Track",
    y = "Number of Tracks"
  ) +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  scale_fill_manual(values = c('tomato', 'seagreen')) +
  guides(fill = FALSE) +
  theme_wsj(base_size = 12) +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold")
  )

# "Pre-COVID" = "#ff7f0e", "Post-COVID" = "#6baed6"
# scale_fill_gradient2(low = "#d9f0d3",  mid = "#a6dba0", high = "#1b7837", 
# midpoint = median(data_words$count)) +
# ----------------------------------------------

# Pre-COVID Sentiment score in Lyrics
data_affin_pre <- music_data %>%
  filter(year >= 2016 & year <= 2019) %>%
  select(track_id, lyrics) %>%
  group_by(track_id) %>%
  unnest_tokens(word, lyrics) %>%
  inner_join(afinn, by = "word") %>%
  summarize(reviewSentiment = mean(value)) %>%
  mutate(period = "Pre-COVID") %>%
  ungroup()
data_affin_pre

# Post-COVID Sentiment score in Lyrics
data_affin_post <- music_data %>%
  filter(year >= 2020 & year <= 2023) %>%
  select(track_id, lyrics) %>%
  group_by(track_id) %>%
  unnest_tokens(word, lyrics) %>%
  inner_join(afinn, by = "word") %>%
  summarize(reviewSentiment = mean(value)) %>%
  mutate(period = "Post-COVID") %>%
  ungroup()
data_affin_post


# Combine the two datasets
combined_affin <- bind_rows(data_affin_pre, data_affin_post)


# Normalized Histogram Plot
ggplot(combined_affin, aes(x = reviewSentiment, fill = period)) +
  geom_histogram(aes(y = after_stat(density)), 
                 position = "identity", binwidth = 0.2, alpha = 0.7, color = "grey") +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  scale_fill_manual(values = c("Pre-COVID" = "#ff7f0e", "Post-COVID" = "#6baed6")) +
  labs(
    title = "Sentiment Scores in Lyrics: Pre vs Post COVID",
    subtitle = "Normalized density of sentiment scores per track",
    x = "Sentiment Score (Negative to Positive)",
    y = "Density",
    fill = "Period"
  ) +
  theme_wsj(base_size = 8) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )


# Normalized Density Plot
ggplot(combined_affin, aes(x = reviewSentiment, fill = period, color = period)) +
  geom_density(alpha = 0.5, size = 1.2) +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  scale_fill_manual(values = c("Pre-COVID" = "#ff7f0e", "Post-COVID" = "#6baed6")) +
  scale_color_manual(values = c("Pre-COVID" = "#ff7f0e", "Post-COVID" = "#6baed6")) +
  labs(
    title = "Smoothed Sentiment Distributions in Lyrics",
    subtitle = "Pre vs Post COVID (Normalized)",
    x = "Sentiment Score",
    y = "Density",
    fill = "Period",
    color = "Period"
  ) +
  guides(fill = guide_legend(override.aes = list(color = NA))) +  # suppress redundant color box
  theme_wsj(base_size = 10) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )




# ************************************************************************************
# ------------------------------------------------------------------------------------
# Bag of words approach: Text Preprocessing
# ------------------------------------------------------------------------------------
# ************************************************************************************


#------------- Split dataframe to Pre COVID & Post COVID dataset--------


#------------- Pre COVID (2016–2019)  -------------


# Pre-COVID data
data_preCovid <- music_data %>%
  filter(year %in% c(2016, 2017, 2018, 2019))
  

# Create VCorpus
corpus_preCovid <- VCorpus(VectorSource(data_preCovid$lyrics))

length(corpus_preCovid)  # Before cleaning

# Text cleaning pipeline
corpus_preCovid <- tm_map(corpus_preCovid, content_transformer(tolower))
corpus_preCovid <- tm_map(corpus_preCovid, content_transformer(function(x) gsub("http[[:alnum:][:punct:]]*", " ", x)))
corpus_preCovid <- tm_map(corpus_preCovid, removePunctuation)
corpus_preCovid <- tm_map(corpus_preCovid, removeWords, stopwords("english"))
corpus_preCovid <- tm_map(corpus_preCovid, stripWhitespace)
corpus_preCovid <- tm_map(corpus_preCovid, stemDocument)
corpus_preCovid[[25]][1]

length(corpus_preCovid)  # Before cleaning

# Dictionary
dict_preCovid <- findFreqTerms(DocumentTermMatrix(VCorpus(VectorSource(data_preCovid$lyrics))), lowfreq = 0)
dict_corpus_preCovid <- Corpus(VectorSource(dict_preCovid))


# DTM and cleaning
dtm_preCovid <- DocumentTermMatrix(corpus_preCovid)
xdtm_preCovid <- removeSparseTerms(dtm_preCovid, sparse = 0.95)
xdtm_preCovid <- as.data.frame(as.matrix(xdtm_preCovid))
colnames(xdtm_preCovid) <- stemCompletion(x = colnames(xdtm_preCovid), dictionary = dict_corpus_preCovid, type = "prevalent")
colnames(xdtm_preCovid) <- make.names(colnames(xdtm_preCovid))

# TF-IDF
dtm_tfidf_preCovid <- DocumentTermMatrix(corpus_preCovid, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
xdtm_tfidf_preCovid <- removeSparseTerms(dtm_tfidf_preCovid, sparse = 0.95)
xdtm_tfidf_preCovid <- as.data.frame(as.matrix(xdtm_tfidf_preCovid))
colnames(xdtm_tfidf_preCovid) <- stemCompletion(x = colnames(xdtm_tfidf_preCovid), dictionary = dict_corpus_preCovid, type = "prevalent")
colnames(xdtm_tfidf_preCovid) <- make.names(colnames(xdtm_tfidf_preCovid))


# Add popularity back
music_data_tf_preCovid <- cbind(track_popularity_score = data_preCovid$track_popularity, xdtm_preCovid)
music_data_tfidf_preCovid <- cbind(track_popularity_score = data_preCovid$track_popularity, xdtm_tfidf_preCovid)


#------------- Post COVID (2020–2023)-------------
data_postCovid <- music_data %>%
  filter(year %in% c(2020, 2021, 2022, 2023)) 

# Create corpus
corpus_postCovid <- Corpus(VectorSource(data_postCovid$lyrics))


length(corpus_postCovid)  # Before cleaning

# Clean corpus
corpus_postCovid <- tm_map(corpus_postCovid, content_transformer(tolower))
corpus_postCovid <- tm_map(corpus_postCovid, content_transformer(function(x) gsub('http[[:alnum:][:punct:]]*', ' ', x)))
corpus_postCovid <- tm_map(corpus_postCovid, removePunctuation)
corpus_postCovid <- tm_map(corpus_postCovid, removeWords, stopwords("english"))
corpus_postCovid <- tm_map(corpus_postCovid, stripWhitespace)
corpus_postCovid <- tm_map(corpus_postCovid, stemDocument)
corpus_postCovid[[25]][1]

length(corpus_postCovid)  # After cleaning


# Create a Dictionary for stem completion
dict_postCovid <- findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(data_postCovid$lyrics))), lowfreq = 0)
dict_corpus_postCovid <- Corpus(VectorSource(dict_postCovid))

# DTM (TF)
dtm_postCovid <- DocumentTermMatrix(corpus_postCovid)
xdtm_postCovid <- removeSparseTerms(dtm_postCovid, sparse = 0.95)
xdtm_postCovid <- as.data.frame(as.matrix(xdtm_postCovid))
colnames(xdtm_postCovid) <- stemCompletion(colnames(xdtm_postCovid), dictionary = dict_corpus_postCovid, type = 'prevalent')
colnames(xdtm_postCovid) <- make.names(colnames(xdtm_postCovid))

# DTM (TF-IDF)
dtm_tfidf_postCovid <- DocumentTermMatrix(corpus_postCovid, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
xdtm_tfidf_postCovid <- removeSparseTerms(dtm_tfidf_postCovid, sparse = 0.95)
xdtm_tfidf_postCovid <- as.data.frame(as.matrix(xdtm_tfidf_postCovid))
colnames(xdtm_tfidf_postCovid) <- stemCompletion(colnames(xdtm_tfidf_postCovid), dictionary = dict_corpus_postCovid, type = 'prevalent')
colnames(xdtm_tfidf_postCovid) <- make.names(colnames(xdtm_tfidf_postCovid))


# Add popularity score
music_data_tf_postCovid <- cbind(track_popularity_score = data_postCovid$track_popularity, xdtm_postCovid)
music_data_tfidf_postCovid <- cbind(track_popularity_score = data_postCovid$track_popularity, xdtm_tfidf_postCovid)




# ------> Visualization
library(tidyverse)
library(ggthemes)

# Pre-COVID top terms
df_pre <- data.frame(
  term = colnames(xdtm_preCovid),
  tf = colMeans(xdtm_preCovid),
  tfidf = colMeans(xdtm_tfidf_preCovid),
  period = "Pre-COVID"
)

# Post-COVID top terms
df_post <- data.frame(
  term = colnames(xdtm_postCovid),
  tf = colMeans(xdtm_postCovid),
  tfidf = colMeans(xdtm_tfidf_postCovid),
  period = "Post-COVID"
)

# Combine and reshape
df_combined <- bind_rows(df_pre, df_post) %>%
  pivot_longer(cols = c(tf, tfidf), names_to = "weighting_method", values_to = "weight")

# Top 20 terms by TF in each period
top_terms <- df_combined %>%
  filter(weighting_method == "tf") %>%
  group_by(period) %>%
  top_n(20, weight) %>%
  pull(term) %>%
  unique()

# Filter for those top terms only
df_filtered <- df_combined %>%
  filter(term %in% top_terms)

# Ensure Pre-COVID appears on the left
df_filtered$period <- factor(df_filtered$period, levels = c("Pre-COVID", "Post-COVID"))


# Plot side by side Bar chart 
ggplot(df_filtered, aes(x = reorder(term, weight), y = weight, fill = weighting_method)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~ period, scales = "free_y") +
  theme_economist() +
  labs(
    title = "Top 20 Terms by TF and TF-IDF Weighting in Lyrics (Pre vs Post COVID)",
    x = "Terms",
    y = "Weight",
    fill = "Weighting Method"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 8)
  )








# ************************************************************************************
# ------------------------------------------------------------------------------------
# Predictive Models to predict track_popularity_score (using TF features) 
# ------------------------------------------------------------------------------------
# ************************************************************************************


#------------------- Predictive Models for Pre COVID -------------------

set.seed(617)
split_pre_tf <- sample(1:nrow(music_data_tf_preCovid), size = 0.7 * nrow(music_data_tf_preCovid))
train_pre_tf <- music_data_tf_preCovid[split_pre_tf, ]
test_pre_tf <- music_data_tf_preCovid[-split_pre_tf, ]

# Remove duplicate and extra columns (e.g., row names accidentally included)
train_pre_tf <- train_pre_tf[, !duplicated(names(train_pre_tf))]
test_pre_tf <- test_pre_tf[, !duplicated(names(test_pre_tf))]

# Ensure 'X' column (if accidentally created from rownames) is removed
train_pre_tf <- train_pre_tf[, !names(train_pre_tf) %in% c("X")]
test_pre_tf <- test_pre_tf[, !names(test_pre_tf) %in% c("X")]

# Linear Regression with multicollinearity check
library(caret)

combo_info <- findLinearCombos(train_pre_tf)
train_pre_tf_clean <- if (!is.null(combo_info$remove)) {
  train_pre_tf[, -combo_info$remove]
} else {
  train_pre_tf
}

reg_pre_tf <- lm(track_popularity_score ~ ., data = train_pre_tf_clean)
pred_reg_pre_tf <- predict(reg_pre_tf, newdata = test_pre_tf)
rmse_reg_pre_tf <- sqrt(mean((pred_reg_pre_tf - test_pre_tf$track_popularity_score)^2))
print(rmse_reg_pre_tf)




#------------------- Predictive Models for Post COVID -------------------

set.seed(617)
split_post_tf <- sample(1:nrow(music_data_tf_postCovid), size = 0.7 * nrow(music_data_tf_postCovid))
train_post_tf <- music_data_tf_postCovid[split_post_tf, ]
test_post_tf <- music_data_tf_postCovid[-split_post_tf, ]

# Remove duplicate and extra columns
train_post_tf <- train_post_tf[, !duplicated(names(train_post_tf))]
test_post_tf <- test_post_tf[, !duplicated(names(test_post_tf))]

train_post_tf <- train_post_tf[, !names(train_post_tf) %in% c("X")]
test_post_tf <- test_post_tf[, !names(test_post_tf) %in% c("X")]

# Linear Regression with collinearity removal
combo_info_post <- findLinearCombos(train_post_tf)
train_post_tf_clean <- if (!is.null(combo_info_post$remove)) {
  train_post_tf[, -combo_info_post$remove]
} else {
  train_post_tf
}

reg_post_tf <- lm(track_popularity_score ~ ., data = train_post_tf_clean)
pred_reg_post_tf <- predict(reg_post_tf, newdata = test_post_tf)
rmse_reg_post_tf <- sqrt(mean((pred_reg_post_tf - test_post_tf$track_popularity_score)^2))
print(rmse_reg_post_tf)



#------------- Comparison for Pre COVID & Post COVID  -------------

#--------> Top Predictors from Linear Regression
# Get top 10 coefficients (by absolute value) for Pre-COVID TF
coef_pre_tf <- coef(reg_pre_tf)
df_pre_tf <- data.frame(Term = names(coef_pre_tf), Coefficient = coef_pre_tf) %>%
  filter(Term != "(Intercept)") %>%
  mutate(abs_coef = abs(Coefficient)) %>%
  arrange(desc(abs_coef)) 

# Get top 10 coefficients for Post-COVID TF
coef_post_tf <- coef(reg_post_tf)
df_post_tf <- data.frame(Term = names(coef_post_tf), Coefficient = coef_post_tf) %>%
  filter(Term != "(Intercept)") %>%
  mutate(abs_coef = abs(Coefficient)) %>%
  arrange(desc(abs_coef)) 

# Filter top 10 by absolute coefficient
df_pre_tf_top10 <- df_pre_tf %>%
  arrange(desc(abs_coef)) %>%
  slice_head(n = 20)

df_post_tf_top10 <- df_post_tf %>%
  arrange(desc(abs_coef)) %>%
  slice_head(n = 20)

# Plotting
plot_tf_pre <- ggplot(df_pre_tf_top10, aes(x = reorder(Term, abs_coef), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Pre COVID", x = "Term", y = "Coefficient") +
  theme_minimal()

plot_tf_post <- ggplot(df_post_tf_top10, aes(x = reorder(Term, abs_coef), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  coord_flip() +
  labs(title = "Post COVID", x = "Term", y = "Coefficient") +
  theme_minimal()

# Side-by-side plot
grid.arrange(
  plot_tf_pre, plot_tf_post,
  ncol = 2,
  top = textGrob(
    "Top 20 Predictors of Popularity Score (using TF)",
    gp = gpar(fontsize = 16, fontface = "bold")
  )
)
















