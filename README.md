# pds_2_diabetes_project

The code provided loads the diabetes.csv dataset and preprocesses it by handling missing values, outliers, and saving the cleaned data as a new CSV file. It then performs the following tasks:

Part A: Sets a seed for reproducibility, takes a random sample of 25 observations, and calculates the mean and maximum Glucose values. It compares these values with the population statistics using bar charts and saves the results as PNG files.

Part B: Calculates the 98th percentile of BMI for the sample and population, compares them using bar charts, and saves the results as PNG files.

Part C: Uses the bootstrap method to create 500 samples (each containing 150 observations) with replacement. It calculates the average mean, standard deviation, and 98th percentile for BloodPressure in the bootstrap samples and compares these statistics with the population values. The comparisons are visualized using bar charts and saved as PNG files.

A comprehensive analysis and comparison of the results can be found in a Word document titled REPORT.docx.
