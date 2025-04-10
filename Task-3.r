# 1. Departemen dengan kunjungan terbanyak dan kepuasan tertinggi

# Ensure the dataset is loaded
library(readxl)
df <- read_excel("AFL-1 Dataset.xlsx", sheet = "Dataset")

# Fix column names if necessary
colnames(df) <- make.names(colnames(df))

# Calculate department visit counts and satisfaction scores
dept_count <- table(df$Department)
dept_satisfaction <- aggregate(Satisfaction_Score ~ Department, data = df, FUN = mean)

# Print the department with the most visits
print("Department with the most visits:")
print(dept_count[which.max(dept_count)])

# Print the department with the highest satisfaction score
print("Department with the highest satisfaction score:")
print(dept_satisfaction[which.max(dept_satisfaction$Satisfaction_Score), ])

# 4. Rata-rata tekanan darah perokok vs bukan
bp_smoker <- aggregate(Blood_Pressure ~ Smoker, data = df, FUN = mean)
print("Average Blood Pressure for Smokers vs Non-Smokers:")
print(bp_smoker)

# 5. Kelompok demografis dengan kepuasan tertinggi
demo_sat <- aggregate(Satisfaction_Score ~ Gender + Department, data = df, FUN = mean)
highest_demo_sat <- demo_sat[which.max(demo_sat$Satisfaction_Score), ]
print("Demographic group with the highest satisfaction score:")
print(highest_demo_sat)

# Scatter plot: Relationship between Cholesterol and Heart Rate
library(ggplot2)

cholesterol_vs_heart_rate_plot <- ggplot(df, aes(x = Cholesterol, y = Heart_Rate)) +
  geom_point(color = "blue", alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Relationship between Cholesterol and Heart Rate",
    x = "Cholesterol",
    y = "Heart Rate"
  )

# Print the scatter plot
print(cholesterol_vs_heart_rate_plot)