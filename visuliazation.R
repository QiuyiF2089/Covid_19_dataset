#1.What are the basic summary statistics for the dataset?
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the data
# covid_data <- read_csv("/mnt/data/Covid_data.csv")

# Inspect the structure of the dataset
str(covid_data)

# Summary statistics for key numerical variables
summary(covid_data)

# Visualizations: Histograms and boxplots for key variables
ggplot(covid_data, aes(x = total_cases)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Distribution of COVID-19 Cases", x = "Cases", y = "Frequency")


ggplot(covid_data, aes(y = total_cases)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of COVID-19 Cases", y = "Cases")


