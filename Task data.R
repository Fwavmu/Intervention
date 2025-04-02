library(readxl)
library(dplyr)

#Loading the data
task_data <- read_excel("C:/Users/ADMIN/Downloads/task_data.xlsx")
View(task_data)
head(task_data)
summary(task_data)

# Renaming columns for clarity
colnames(task_data) <- c("Time","Condition", "PHQ_1", "PHQ_2", "PHQ_3", "PHQ_4", 
                  "PHQ_5", "PHQ_6", "PHQ_7", "PHQ_8", "GAD_1", "GAD_2", "GAD_3", "GAD_4",
                  "GAD_5", "GAD_6", "GAD_7", "Age", "Form", "Gender")
View(task_data)

# Defining the response mapping 
response_mapping <- c("Not at all" = 0, 
                      "Several days (between 2 and 7 days)" = 1, 
                      "Over half the days (more than 7 days)" = 2, 
                      "Nearly/almost every day" = 3)


# Converting 'Time' and 'Condition' to factors
task_data$Time <- factor(task_data$Time, levels = c("Baseline", "Endpoint"), labels = c(0, 1))
task_data$Condition <- as.factor(task_data$Condition)
View(task_data)
print(head(task_data)) 


#Dropping missing values
sum(is.na(task_data))
task_data <- na.omit(task_data)
View(task_data)

# Computing PHQ-8 and GAD-7 Scores
library(dplyr)

task_data <- task_data %>%
# Ensuring that we are working with numeric columns
  mutate(across(starts_with("PHQ"), as.numeric),  
         across(starts_with("GAD"), as.numeric)) %>%  
  rowwise() %>%
  mutate(PHQ_Total = sum(c_across(starts_with("PHQ")), na.rm = TRUE), 
         GAD_Total = sum(c_across(starts_with("GAD")), na.rm = TRUE)) %>%
  ungroup()  # Ungrouping after row-wise to return to a regular data frame

# Viewing the updated task_data with totals
View(task_data)

library(dplyr)

#PHQ-Totals Likert scale
task_data <- task_data %>%
  mutate(PHQ_Category = case_when(
    PHQ_Total >= 0 & PHQ_Total <= 5 ~ "Mild",
    PHQ_Total >= 6 & PHQ_Total <= 14 ~ "Moderate",
    PHQ_Total >= 15 & PHQ_Total <= 19 ~ "Moderately Severe",
    PHQ_Total >= 20  ~ "Severe"
  ))
View(task_data)

#GAD-Totals Likert scale
task_data <- task_data %>%
  mutate(GAD_Category = case_when(
    GAD_Total >= 0 & GAD_Total <= 5~ "Mild",
    GAD_Total >= 5 & GAD_Total <= 14 ~ "Moderate",
    GAD_Total >= 15 ~ "Severe anxiety"
  ))
View(task_data)

#Visualization of Likert Scale Distribution
library(ggplot2)
library(dplyr)

# PHQ-8 Likert Scale Distribution
ggplot(task_data, aes(x = PHQ_Category, fill = PHQ_Category)) +
  geom_bar() +
  labs(title = "PHQ-8 Likert Scale Distribution", x = "Depression Severity", y = "Count") +
  theme_minimal()

# GAD-7 Likert Scale Distribution
ggplot(task_data, aes(x = GAD_Category, fill = GAD_Category)) +
  geom_bar() +
  labs(title = "GAD-7 Likert Scale Distribution", x = "Anxiety Severity", y = "Count") +
  theme_minimal()


#Conducting statistical analysis
# Loading necessary libraries
library(dplyr)
library(ggplot2)

# Separating baseline and endpoint
task_data_baseline <- task_data %>% filter(Time == 0)
task_data_endpoint <- task_data %>% filter(Time == 1)

# Merge by Study_ID
task_data_wide <- merge(task_data_baseline, task_data_endpoint, by = "Study_ID", suffixes = c("_baseline", "_endpoint"))

# Computing change scores
task_data_wide <- task_data_wide %>%
  mutate(PHQ_Change = PHQ_Total_endpoint - PHQ_Total_baseline,
         GAD_Change = GAD_Total_endpoint - GAD_Total_baseline)

# Comparing intervention vs. control
intervention <- task_data_wide %>% filter(Condition_baseline == "intervention")
control <- task_data_wide %>% filter(Condition_baseline == "control")

# Checking for missing values before t-tests
if (nrow(intervention) > 0 && nrow(control) > 0) {
  # T-tests
  phq_ttest <- t.test(intervention$PHQ_Change, control$PHQ_Change)
  gad_ttest <- t.test(intervention$GAD_Change, control$GAD_Change)
  
  # Output t-test results
  print(phq_ttest)
  print(gad_ttest)
} else {
  print("Not enough data for t-tests.")
}

# Boxplots
ggplot(task_data_wide, aes(x = Condition_baseline, y = PHQ_Change)) +
  geom_boxplot() +
  ggtitle("PHQ-8 Change by Condition")

ggplot(task_data_wide, aes(x = Condition_baseline, y = GAD_Change)) +
  geom_boxplot() +
  ggtitle("GAD-7 Change by Condition")

# Output t-test results
print(phq_ttest)
print(gad_ttest)

# Checking the data frame
head(task_data_wide)
View(task_data_wide)
# Saving the cleaned data frame to a CSV file
write.csv(task_data_wide, "clean_data.csv", row.names = FALSE)

# Confirming the file has been created
list.files()
#Saving the cleaned data
write.csv(task_data, "clean_data.csv", row.names = FALSE)

# Summary of demographic variables
summary(task_data_wide$Age_baseline)
table(task_data_wide$Gender_baseline)
table(task_data_wide$Form_baseline)

