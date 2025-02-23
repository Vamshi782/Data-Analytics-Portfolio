# Install required packages if not already installed
packages <- c("tidyverse", "readxl", "ggplot2", "dplyr", "janitor", "knitr", 
              "kableExtra", "forecast", "corrplot", "gridExtra", "scales")

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
lapply(packages, library, character.only = TRUE)

# Load school-level dataset
school_data <- read_excel("Schooldata.xlsx")

# Load DOE dataset (State-level absenteeism)
doe_data <- read.csv("2018 to 2023 comparison by state (Version 12-10-2024).xlsx - By State.csv", skip = 1)

head(doe_data)# Clean column names

# Rename columns properly
colnames(doe_data)[1] <- "state"

# Reshape the data (pivot longer)
doe_data_long <- doe_data %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "year", 
               values_to = "chronic_absenteeism_rate")

# Extract actual years from column names
doe_data_long$year <- gsub("X\\.", "", doe_data_long$year)

# Convert year column to factor
doe_data_long$year <- as.factor(doe_data_long$year)

# Check structure
str(doe_data_long)

school_data <- clean_names(school_data)
doe_data <- clean_names(doe_data)

# Ensure necessary columns exist
doe_data <- doe_data %>% rename_with(~ gsub(" ", "_", .), everything())

colnames(doe_data)
# Convert year column to factor
if("year" %in% colnames(doe_data)) {
  doe_data$year <- as.factor(doe_data$year)
} else {
  stop("Column 'year' not found in DOE dataset")
}

# Rename and clean school_data columns
expected_columns <- c("name", "town", "plan", "total_absences", "ca_2023_24", "ca_2022_23", 
                      "ca_2021_22", "unexcused", "excused", "illness", "appointment", 
                      "vacation_travel", "religious_holiday", "personal", "other", 
                      "credits", "gpa", "extracurriculars", "music", "hctc", "notes")
colnames(school_data) <- expected_columns[1:length(colnames(school_data))]

# Extract and categorize excused vs. unexcused absences
if("ca_2023_24" %in% colnames(school_data)) {
  school_data <- school_data %>% 
    mutate(
      excused_absences = as.numeric(str_extract(ca_2023_24, "\\d+")),
      unexcused_absences = total_absences - excused_absences
    )
} else {
  stop("Column 'ca_2023_24' not found in school dataset")
}

# Handle missing values
school_data <- school_data %>% replace_na(list(
  excused_absences = 0, 
  unexcused_absences = 0, 
  gpa = median(school_data$gpa, na.rm = TRUE)
))

# Summary statistics
summary_stats <- school_data %>% summarise(
  avg_gpa = mean(gpa, na.rm = TRUE),
  avg_total_absences = mean(total_absences, na.rm = TRUE),
  avg_excused = mean(excused_absences, na.rm = TRUE),
  avg_unexcused = mean(unexcused_absences, na.rm = TRUE)
)
kable(summary_stats, caption = "Summary of Absenteeism Data")

# Correlation Analysis
corr_matrix <- cor(school_data %>% select(total_absences, excused_absences, unexcused_absences, gpa), use = "complete.obs")
corrplot(corr_matrix, method = "color", title = "Correlation Between Absenteeism and GPA")

# Scatter Plot: Absenteeism Impact on GPA
school_data$plan <- as.factor(school_data$plan)
absenteeism_gpa_plot <- ggplot(school_data, aes(x = total_absences, y = gpa, color = plan)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Impact of Absenteeism on GPA", x = "Total Absences", y = "GPA") +
  theme_minimal()
print(absenteeism_gpa_plot)

# Trend Analysis - State-Level Absenteeism
doedata_summary <- doe_data %>% group_by(year) %>% summarise(avg_absenteeism = mean(chronic_absenteeism_rate, na.rm = TRUE))
state_absenteeism_plot <- ggplot(doedata_summary, aes(x = year, y = avg_absenteeism, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "State-Level Chronic Absenteeism Trends (2018-2023)", x = "Year", y = "Chronic Absenteeism Rate (%)") +
  theme_minimal()
print(state_absenteeism_plot)

# Identify top 5 and bottom 5 states by absenteeism
state_summary <- doe_data %>% 
  group_by(state) %>% 
  summarise(avg_absenteeism = mean(chronic_absenteeism_rate, na.rm = TRUE)) %>% 
  arrange(desc(avg_absenteeism))

top_5_states <- head(state_summary, 5)
bottom_5_states <- tail(state_summary, 5)

# Bar plot for top vs bottom absenteeism states
top_bottom_plot <- ggplot(rbind(top_5_states, bottom_5_states), aes(x = reorder(state, avg_absenteeism), y = avg_absenteeism, fill = state)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 5 vs Bottom 5 States in Absenteeism", x = "State", y = "Avg Absenteeism Rate (%)") +
  theme_minimal()
print(top_bottom_plot)

# Time Series Forecasting for Chronic Absenteeism
ts_data <- ts(doe_data$chronic_absenteeism_rate, start = c(2018, 1), frequency = 1)
model <- auto.arima(ts_data)
forecast_values <- forecast(model, h = 5)
plot(forecast_values, main = "Predicted Absenteeism Trends", ylab = "Chronic Absenteeism Rate (%)")

# Recommendations for increasing attendance and reducing absenteeism
recommendations <- "1. Implement reward-based attendance programs to encourage student presence.\n2. Strengthen parental involvement and communication strategies.\n3. Utilize early warning systems to identify at-risk students.\n4. Offer flexible learning options to support students with special needs.\n5. Analyze policies from states with the lowest absenteeism to implement best practices."
writeLines(recommendations, "recommendations.txt")

# Save plots
ggsave("absenteeism_gpa_plot.png", absenteeism_gpa_plot)
ggsave("state_absenteeism_plot.png", state_absenteeism_plot)
ggsave("top_bottom_plot.png", top_bottom_plot)
