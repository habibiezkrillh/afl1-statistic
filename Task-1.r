# Load some packages
library(readxl)
library(dplyr)
library(tidyr)

# Load the data using Excel
df <- read_excel("AFL-1 Dataset.xlsx", sheet = "Dataset")

# 1. Average age per department
avg_age_by_dept <- df %>%
  group_by(Department) %>%
  summarise(Average_Age = mean(Age, na.rm = TRUE))

print("Rata-rata umur per departemen:")
print(avg_age_by_dept)


# 2. Average & STD of Blood Pressure, Heart Rate, and Cholestrol
mean_bp <- mean(df$Blood_Pressure, na.rm = TRUE)
sd_bp <- sd(df$Blood_Pressure, na.rm = TRUE)
mean_hr <- mean(df$Heart_Rate, na.rm = TRUE)
sd_hr <- sd(df$Heart_Rate, na.rm = TRUE)
mean_chol <- mean(df$Cholesterol, na.rm = TRUE)
sd_chol <- sd(df$Cholesterol, na.rm = TRUE)

cat("Mean Blood Pressure:", round(mean_bp, 2), "\n")
cat("Standard Deviation of Blood Pressure:", round(sd_bp, 2), "\n")
cat("Mean Heart Rate:", round(mean_hr, 2), "\n")
cat("Standard Deviation of Heart Rate:", round(sd_hr, 2), "\n")
cat("Mean Cholesterol:", round(mean_chol, 2), "\n")
cat("Standard Deviation of Cholesterol:", round(sd_chol, 2), "\n")

# 3. Percentage of smokers and/or diabetic
percentages <- df %>%
  summarise(
    Percent_Smokers = mean(Smoker == "Yes") * 100,
    Percent_Diabetic = mean(Diabetes == "Yes") * 100,
    Percent_Either = mean(Smoker == "Yes" | Diabetes == "Yes") * 100
  )
print("Percentage of smokers and/or diabetic:")
print(percentages)

# 4. Department with most patients
most_patients <- df %>%
  count(Department) %>%
  arrange(desc(n)) %>%
  slice(1)
print("Department with the most patients:")
print(most_patients)

# 5. Avg satisfaction by department and gender
avg_satisfaction <- df %>%
  group_by(Department, Gender) %>%
  summarise(
    Avg_Satisfaction = mean(Satisfaction_Score, na.rm = TRUE),
    .groups = "drop"
  )
print("Average satisfaction by department and gender:")
print(avg_satisfaction)

# 6. Most common combinations of smoking & diabetes
common_combination <- df %>%
  count(Smoker, Diabetes) %>%
  arrange(desc(n)) %>%
  slice(1)
print("Most common combinations of smoking and diabetes:")
print(common_combination)

# 7. Comparison of heart rate between smokers and non-smokers
heart_rate_comparison <- df %>%
  group_by(Smoker) %>%
  summarise(Avg_Heart_Rate = mean(Heart_Rate, na.rm = TRUE))
print("Comparison of heart rate between smokers and non-smokers:")
print(heart_rate_comparison)
