library(ggplot2)
library(dplyr)
library(caret)
library(skimr) 
library(fastDummies)
library(corrplot)
library(tidyr)
library(xgboost)
library(Metrics)
library(iml)
library(parallel)

# ------------------------------------------------------------------------------------
# ************************* READ DATASET ****************************************
# ------------------------------------------------------------------------------------

setwd('/Users/shelbyjoji/Desktop/Columbia Classes (Spring)/01_FRAMEWORKS & METHDS II/Group_Project/Covid&Spotify_Project/Dataset')
data = read.csv(file = 'playlist_2010to2023.csv')
View(data)


# ------------------------------------------------------------------------------------
# ************************* DATA EXPLORATION ****************************************
# ------------------------------------------------------------------------------------

head(data)
ncol(data)
nrow(data)
colnames(data) 
summary(data)

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
  mutate(across(all_of(cat_list), as.factor)) |>          # Convert Categorical columns as factors
  select_if(is.factor)|>
  pivot_longer(cols = everything(),names_to = 'categorical_predictor', values_to = 'values'  )|>
  group_by(categorical_predictor, values)|>
  count()|>
  ungroup()|>
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
View(final_data)



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


