par(mfrow=c(3,3)) # Menampilkan 3 grafik per baris

# Load the data using Excel
df <- read_excel("AFL-1 Dataset.xlsx", sheet = "Dataset")

# Variabel untuk di cek
for (var in c("Blood_Pressure", "Heart_Rate", "Cholesterol")) {
  hist(df[[var]], main = paste("Histogram of", var), col = "lightblue", xlab = var)
  qqnorm(df[[var]], main = paste("QQ Plot of", var)); qqline(df[[var]])
  boxplot(df[[var]], main = paste("Boxplot of", var), col = "orange")
}

# Cek normalitas secara statistik (opsional)
shapiro.test(df$Blood_Pressure)
shapiro.test(df$Heart_Rate)
shapiro.test(df$Cholesterol)