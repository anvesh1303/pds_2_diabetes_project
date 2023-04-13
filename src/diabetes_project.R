# Load the data
diabetes <- read.csv("/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/data_raw/diabetes.csv")

# Make a copy of the original data
diabetes_clean <- diabetes

# Preprocess the copy of the data
# Check for missing values
sum(is.na(diabetes_clean))

# Replace missing values with median for Glucose, BloodPressure, SkinThickness, Insulin, and BMI
diabetes_clean$Glucose <- ifelse(diabetes_clean$Glucose == 0, median(diabetes_clean$Glucose), diabetes_clean$Glucose)
diabetes_clean$BloodPressure <- ifelse(diabetes_clean$BloodPressure == 0, median(diabetes_clean$BloodPressure), diabetes_clean$BloodPressure)
diabetes_clean$SkinThickness <- ifelse(diabetes_clean$SkinThickness == 0, median(diabetes_clean$SkinThickness), diabetes_clean$SkinThickness)
diabetes_clean$Insulin <- ifelse(diabetes_clean$Insulin == 0, median(diabetes_clean$Insulin), diabetes_clean$Insulin)
diabetes_clean$BMI <- ifelse(diabetes_clean$BMI == 0, median(diabetes_clean$BMI), diabetes_clean$BMI)

# Check for outliers
library(dplyr)
diabetes_clean %>% 
  select(-Outcome) %>% 
  boxplot()

# Remove extreme outliers for Glucose, BloodPressure, and BMI
diabetes_clean <- diabetes_clean %>%
  filter(Glucose <= 200 & BloodPressure <= 120 & BMI <= 60)

# Check the cleaned data
summary(diabetes_clean)

# Save the cleaned data to a new CSV file
write.csv(diabetes_clean, "/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/data_clean/diabetes_cleaned.csv", row.names = FALSE)

diabetes_clean = read.csv("/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/data_clean/diabetes_cleaned.csv")
  


# part A

# Set seed for reproducibility
set.seed(1234)

# Take a random sample of 25 observations
sample_data <- diabetes_clean[sample(nrow(diabetes_clean), 25), ]

# Find the mean Glucose and highest Glucose values of the sample
mean_glucose <- mean(sample_data$Glucose)
max_glucose <- max(sample_data$Glucose)

# Create a data frame with the mean and max values
result_df <- data.frame(mean_glucose, max_glucose)

# Write the data frame to a new file, named "Mean_Max_Glucosevalues"
write.table(result_df, file = "/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/results/Mean_Max_Glucosevalues", row.names = FALSE)

# Calculate the population statistics for comparison
pop_mean_glucose <- mean(diabetes_clean$Glucose)
pop_max_glucose <- max(diabetes_clean$Glucose)

# Create a data frame with the Population mean and max values
result_df <- data.frame(pop_mean_glucose, pop_max_glucose)

# Write the data frame to a new file, named "Mean_Max_Population"
write.table(result_df, file = "/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/results/Mean_Max_Population", row.names = FALSE)


# Create a bar chart to compare mean Glucose values
mean_data <- data.frame(Type = c("Population", "Sample"),
                        Glucose = c(pop_mean_glucose, mean_glucose))
library(ggplot2)
ggplot(mean_data, aes(x = Type, y = Glucose, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Comparison of Mean Glucose Values") +
  xlab("") +
  ylab("Mean Glucose") +
  theme_bw()

# Save the plot as a png file
ggsave("/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/results/Mean_Comparison.png")

# Create a bar chart to compare highest Glucose values
max_data <- data.frame(Type = c("Population", "Sample"),
                       Glucose = c(pop_max_glucose, max_glucose))

ggplot(max_data, aes(x = Type, y = Glucose, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Comparison of Highest Glucose Values") +
  xlab("") +
  ylab("Highest Glucose") +
  theme_bw()
# Save the plot as a png file
ggsave("/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/results/Max_Comparison.png")



#Part B

# Find the 98th percentile of BMI of the sample and population
sample_bmi_percentile <- quantile(sample_data$BMI, 0.98)
pop_bmi_percentile <- quantile(diabetes_clean$BMI, 0.98)

# Save the values to a file
write.table(c(sample_bmi_percentile, pop_bmi_percentile), file = "/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/results/bmi_percentiles.txt")

# Create a bar chart to compare 98th percentile of BMI values
percentile_data <- data.frame(Type = c("Population", "Sample"),
                              BMI = c(pop_bmi_percentile, sample_bmi_percentile))
ggplot(percentile_data, aes(x = Type, y = BMI, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Comparison of 98th Percentile of BMI Values") +
  xlab("") +
  ylab("98th Percentile of BMI") +
  theme_bw()

# Save the plot as a png file
ggsave("/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/results/compare_98thpercentile.png")



# Part C

# Set the number of bootstrap samples and sample size
num_samples <- 500
sample_size <- 150

# Create a matrix to store the bootstrap samples
bootstrap_data <- matrix(nrow = num_samples, ncol = sample_size)

# Perform the bootstrap
for (i in 1:num_samples) {
  bootstrap_sample <- sample(diabetes_clean$BloodPressure, sample_size, replace = TRUE)
  bootstrap_data[i, ] <- bootstrap_sample
}

# Find the average mean, standard deviation, and percentile for BloodPressure
bootstrap_means <- apply(bootstrap_data, 1, mean)
bootstrap_mean_avg <- mean(bootstrap_means)
bootstrap_mean_sd <- sd(bootstrap_means)
bootstrap_percentile <- quantile(bootstrap_data, 0.98)

# Create a data frame with the average mean, standard deviation, and percentile values
result_df <- data.frame(bootstrap_mean_avg, bootstrap_mean_sd, bootstrap_percentile)

# Write the data frame to a new file, named "bootstrap_mean_avg_sd_percentile_values"
write.table(result_df, file = "/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/results/bootstrap_mean_avg_sd_percentile_values", row.names = FALSE)


# Calculate the population statistics for comparison
pop_mean <- mean(diabetes_clean$BloodPressure)
pop_sd <- sd(diabetes_clean$BloodPressure)
pop_percentile <- quantile(diabetes_clean$BloodPressure, 0.98)

# Create a data frame with the pop mean, standard deviation, and percentile values
result_df <- data.frame(pop_mean, pop_sd, pop_percentile)

# Write the data frame to a new file, named "pop_mean_sd_percentile_values"
write.table(result_df, file = "/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/results/pop_mean_sd_percentile_values", row.names = FALSE)


# Create a bar chart to compare mean BloodPressure values
mean_data <- data.frame(Type = c("Population", "Bootstrap"),
                        Mean = c(pop_mean, bootstrap_mean_avg))
ggplot(mean_data, aes(x = Type, y = Mean, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Comparison of Mean BloodPressure Values") +
  xlab("") +
  ylab("Mean BloodPressure") +
  theme_bw()

# Save the plot as a png file
ggsave("/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/results/mean_bloodpressure_values_comparison.png")


# Create a bar chart to compare standard deviation of BloodPressure values
sd_data <- data.frame(Type = c("Population", "Bootstrap"),
                      SD = c(pop_sd, bootstrap_mean_sd))
ggplot(sd_data, aes(x = Type, y = SD, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Comparison of Standard Deviation of BloodPressure Values") +
  xlab("") +
  ylab("Standard Deviation of BloodPressure") +
  theme_bw()

# Save the plot as a png file
ggsave("/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/results/sd_bloodpressure_comparison.png")


# Create a bar chart to compare 98th percentile of BloodPressure values
percentile_data <- data.frame(Type = c("Population", "Bootstrap"),
                              BloodPressure = c(pop_percentile, bootstrap_percentile))
ggplot(percentile_data, aes(x = Type, y = BloodPressure, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Comparison of 98th Percentile of BloodPressure Values") +
  xlab("") +
  ylab("98th Percentile of BloodPressure") +
  theme_bw()

# Save the plot as a png file
ggsave("/Users/anveshkumar/Desktop/Assignpds2/diabetes_project/results/_bloodpressure_comparison.png")

