library(ggplot2)
library(lubridate)

# Load the data using Excel
df <- read_excel("AFL-1 Dataset.xlsx", sheet = "Dataset")

# 1. Number of Patients per department
plot1 <- ggplot(df, aes(x = Department)) +
  geom_bar(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Number of Patients per Department")
print(plot1)

# 2. Proportion smokers vs non-smokers
plot2 <- ggplot(df, aes(x = "", fill = Smoker)) +
  geom_bar(width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Proportion of Smokers vs Non-Smokers")
print(plot2)

# 3. Monthly trend of patient visits
df$Month <- format(as.Date(df$Visit_Date), "%Y-%m")
plot3 <- df %>%
  count(Month) %>%
  ggplot(aes(x = Month, y = n)) +
  geom_line(group = 1, color = "steelblue") +
  theme_minimal() +
  labs(title = "Monthly Trend of Patient Visits", x = "Month", y = "Patients")
print(plot3)

# 4. Distribution of Blood Pressure
plot4 <- ggplot(df, aes(x = Blood_Pressure)) +
  geom_histogram(bins = 20, fill = "salmon", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Blood Pressure")
print(plot4)

# 5. Cholesterol vs Heart Rate colored by Gender
plot5 <- ggplot(df, aes(x = Cholesterol, y = Heart_Rate, color = Gender)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Cholesterol vs Heart Rate by Gender")
print(plot5)