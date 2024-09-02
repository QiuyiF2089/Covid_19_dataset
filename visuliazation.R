


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



#2
# Aggregate data by date
daily_data <- covid_data %>%
  filter(total_cases != -Inf, total_deaths != -Inf) %>% 
  group_by(date) %>%
  summarize(
    daily_cases = sum(total_cases, na.rm = TRUE),
    daily_deaths = sum(total_deaths, na.rm = TRUE),
    across(everything(), first)
  ) 

# Time series plot for cases and deaths
ggplot(daily_data, aes(x = date)) +
  geom_line(aes(y = daily_cases, color = "Cases")) +
  geom_line(aes(y = daily_deaths, color = "Deaths")) +
  labs(title = "Global COVID-19 Cases and Deaths Over Time", 
       x = "Date", y = "Count") +
  scale_color_manual(name = "Legend", values = c("Cases" = "blue", "Deaths" = "red"))


#3.
# Aggregate data by country
country_data <- covid_data %>%
  filter(!is.na(continent)) %>% 
  group_by(location) %>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE),
            total_deaths = sum(total_deaths, na.rm = TRUE),
            population = first(population)) %>%
  arrange(desc(total_cases))
country_data
# Top 10 countries by cases
top_10_countries <- country_data %>% top_n(10, total_cases)

# Bar chart for top 10 countries
ggplot(top_10_countries, aes(x = reorder(location, -total_cases), y = total_cases)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 10 Countries by COVID-19 Cases", x = "Country", y = "Total Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#4.
# Calculate Case Fatality Rate (CFR)
country_data <- country_data %>%
  filter(!is.na(continent)) %>% 
  mutate(cfr = (total_deaths / total_cases) * 100)

# Scatter plot: Cases vs. CFR
ggplot(country_data, aes(x = total_cases, y = cfr)) +
  geom_point(aes(size = population), color = "red") +
  labs(title = "COVID-19 Cases vs. Case Fatality Rate", x = "Total Cases", y = "Case Fatality Rate (%)") +
  scale_size_continuous(name = "Population")



#5.
# Calculate cases per million
country_data <- country_data %>%
  mutate(cases_per_million = (total_cases / population) * 1e6)

# Choropleth map (using the 'maps' package)
library(maps)
world_map <- map_data("world")
world_map
country_data
country_data_map <- merge(world_map, country_data, by.x = "region", by.y = "country")

# Plot cases per million by country
ggplot(country_data_map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = cases_per_million), color = "white") +
  labs(title = "COVID-19 Cases per Million People by Country", fill = "Cases per Million") +
  theme_minimal()

#6.
final_data <- covid_data %>%
  filter(total_cases != -Inf, total_deaths != -Inf,!is.na(continent)) %>% 
  
  group_by(location) %>%
  summarize(
    total_cases_max = max(total_cases, na.rm = TRUE),
    total_deaths_max = max(total_deaths, na.rm = TRUE),
    across(everything(), first)
  ) 
final_data


continent_data <- final_data %>%
  filter(!is.na(continent)) %>%
  group_by(continent) %>% 
  summarise(total_cases_sum = sum(total_cases_max, na.rm = TRUE),
                         total_deaths_sum = sum(total_deaths_max, na.rm = TRUE))
continent_data

ggplot(continent_data, aes(x = continent, y = total_cases_sum, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "COVID-19 Cases by Continent", x = "Continent", y = "Total Cases") +
  facet_wrap(~continent, scales = "free") +
  theme(legend.position = "none")


ggplot(continent_data, aes(x = continent, y = total_deaths_sum, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "COVID-19 Cases by Continent", x = "Continent", y = "Total Cases") +
  facet_wrap(~continent, scales = "free") +
  theme(legend.position = "none")



#7.
vacc_final_data <- covid_data %>%
  filter(total_cases != -Inf, total_vaccinations != -Inf) %>% 
  group_by(location) %>%
  summarize(
    total_cases_max = max(total_cases, na.rm = TRUE),
    total_vaccinations_max = max(total_vaccinations, na.rm = TRUE)
  ) 

ggplot(vacc_final_data, aes(x =total_vaccinations_max, y = total_cases_max)) +
  geom_point(color = "blue") +
  labs(title = "Vaccination Rate vs. New COVID-19 Cases", x = "Vaccination Rate (%)", y = "New Cases")+
  geom_smooth(method = "lm", col = "red") +
  theme_minimal()



#8.
# Load necessary library for time series
library(forecast)

# Convert data to time series
ts_data <- ts(daily_data$daily_cases, start = c(2020, as.numeric(format(min(daily_data$date), "%j"))), frequency = 365)

# Fit a basic ARIMA model
fit <- auto.arima(ts_data)

# Forecast future cases
forecast_cases <- forecast(fit, h = 30)

# Plot actual vs. predicted case numbers
autoplot(forecast_cases) +
  labs(title = "COVID-19 Case Forecasting", x = "Date", y = "Predicted Cases") +
  autolayer(ts_data, series = "Actual Cases")
#9

# Calculate Q1 (25th percentile) and Q3 (75th percentile)
Q1 <- quantile(final_data$total_deaths, 0.25, na.rm = TRUE)
Q3 <- quantile(final_data$total_deaths, 0.75, na.rm = TRUE)

# Calculate IQR (Interquartile Range)
IQR <- Q3 - Q1

# Determine lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- final_data$total_deaths[final_data$total_deaths < lower_bound | final_data$total_deaths > upper_bound]
outliers

final_data %>% 
  filter(total_deaths < lower_bound | total_deaths > upper_bound)

# Filter out the outliers
final_data_no_outliers <- final_data %>%
  filter(total_deaths >= lower_bound & total_deaths <= upper_bound)


# Scatter plot: Population density vs. total cases
ggplot(final_data, aes(x = population_density, y = total_deaths_per_million)) +
  geom_point(color = "darkblue") +
  labs(title = "Population Density vs. Total COVID-19 Cases", x = "Population Density (people per sq. km)", y = "Total Cases") +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal()+
  ylim(0, max(final_data_no_outliers$total_deaths, na.rm = TRUE))
#10.  

# Aggregate data by continent and date, calculate average stringency index
stringency_data <- covid_data %>%
  group_by(continent, date) %>%
  summarise(avg_stringency = mean(stringency_index, na.rm = TRUE))

# Plot: Stringency index over time by continent
ggplot(stringency_data, aes(x = date, y = avg_stringency, color = continent)) +
  geom_line() +
  labs(title = "Stringency Index Over Time by Continent", x = "Date", y = "Average Stringency Index") +
  theme_minimal()

#11.


# Scatter plot: GDP per capita vs. total deaths per million
ggplot(covid_data, aes(x = gdp_per_capita, y = total_deaths_per_million)) +
  geom_point(color = "purple") +
  labs(title = "GDP per Capita vs. Total COVID-19 Deaths per Million", x = "GDP per Capita (USD)", y = "Total Deaths per Million") +
  geom_smooth(method = "lm", col = "darkred") +
  theme_minimal()+
  ylim(0, max(final_data_no_outliers$total_deaths, na.rm = TRUE))
#12



# Check the data without outliers
head(final_data_no_outliers)




ggplot(final_data_no_outliers, aes(x = aged_65_older, y = total_deaths)) +
  geom_point(color = "green") +
  labs(title = "Impact of Age on COVID-19 Fatality", x = "Percentage of Population Aged 65+", y = "Total Deaths") +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  ylim(0, max(final_data_no_outliers$total_deaths, na.rm = TRUE))


#13
# Scatter plot: Tests per thousand vs. positive rate
ggplot(covid_data, aes(x = total_tests_per_thousand, y = positive_rate)) +
  geom_point(color = "orange") +
  labs(title = "COVID-19 Testing Rates vs. Positive Cases", x = "Total Tests per Thousand", y = "Positive Rate (%)") +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal()

