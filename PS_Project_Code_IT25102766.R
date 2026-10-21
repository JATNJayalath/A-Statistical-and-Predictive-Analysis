library(tidyverse)
library(ggplot2)
library(psych)
library(corrplot)
library(readr)

data <- read_csv("student_career_success_dataset.csv")

#Task 1 - DataSet Introduction
cat("Task 1 - Dataset Introduction\n")
cat("========================================\n\n")

cat("Educational Problem:\n")
cat("To investigate how academic performance, practical experience and soft skills influence students’ early career placement outcomes\n\n")

cat("Dataset Source:\n")
cat("Student Career Success Prediction Dataset from Kaggle.\n\n")

cat("Number of Observations:", nrow(data), "\n")
cat("Number of Variables:", ncol(data), "\n\n")

cat("Variable Descriptions:\n")
cat("CGPA - Cumulative Grade Point Average of the student.\n")
cat("Attendance_Percentage - Percentage of classes attended by the student.\n")
cat("Study_Hours_Per_Week - Number of hours spent studying per week.\n")
cat("Internships - Number of internships completed.\n")
cat("Projects_Completed - Number of projects completed.\n")
cat("Programming_Skill - Level of programming skill.\n")
cat("Communication_Skills - Level of communication skill.\n")
cat("Teamwork - Level of teamwork skill.\n")
cat("Problem_Solving - Level of problem-solving skill.\n")
cat("English_Proficiency - Level of English proficiency.\n")
cat("Placement_Status - Indicates whether the student was placed or not placed.\n\n")

cat("Target Variable: Placement Status\n")

#Dataset Overview
dim(data)           
names(data)         
str(data)           
head(data, 6)       
tail(data, 6)       
summary(data)

#Check data quality and completeness
colSums(is.na(data)) 
round(colMeans(is.na(data)) * 100, 2) 
sum(complete.cases(data)) 
sum(!complete.cases(data)) 
data[!complete.cases(data), ] 
sum(duplicated(data)) 
sapply(data, function(x) length(unique(x))) 

#Select Numerical Variables
numeric_variables <- c(
  "CGPA",
  "Attendance_Percentage",
  "Study_Hours_Per_Week",
  "Internships",
  "Projects_Completed",
  "Programming_Skill",
  "Communication_Skills",
  "Teamwork",
  "Problem_Solving"
)

#Select Categorical Variables
categorical_variables <- c(
  "Placement_Status",
  "English_Proficiency"
)

#All Variables
all_variables <- c(
  numeric_variables,
  categorical_variables
)

#Task 2 - Descriptive Statistical Analysis
cat("Task 2 - Descriptive Statistical Analysis\n")
cat("========================================\n\n")

#1. Frequency Tables
frequency_tables <- lapply(all_variables, function(var) {
  cat("\n")
  cat(var, "Frequency Table\n")
  cat("\n")
  if(var == "CGPA"){
    groups <- cut(
      data$CGPA,
      breaks = c(0, 2.0, 2.5, 3.0, 3.5, 4.0),
      labels = c(
        "Below 2.00",
        "2.00 - 2.49",
        "2.50 - 2.99",
        "3.00 - 3.49",
        "3.50 - 4.00"
      ),
      include.lowest = TRUE
    )
  }
  else if(var == "Attendance_Percentage"){
    groups <- cut(
      data$Attendance_Percentage,
      breaks = c(0, 60, 70, 80, 90, 100),
      labels = c(
        "Below 60%",
        "60% - 69%",
        "70% - 79%",
        "80% - 89%",
        "90% - 100%"
      ),
      include.lowest = TRUE
    )
  }
  else if(var == "Study_Hours_Per_Week"){
    groups<- cut(
      data$Study_Hours_Per_Week,
      breaks = c(0, 10, 20, 30, 40, Inf),
      labels = c(
        "0 - 10 hours",
        "11 - 20 hours",
        "21 - 30 hours",
        "31 - 40 hours",
        "More than 40 hours"
      ),
      include.lowest = TRUE
    )
  }
  else if(var == "Projects_Completed"){
    groups <- cut(
      data$Projects_Completed,
      breaks = c(-1, 2, 5, 8, 11, 15),
      labels = c(
        "0-2",
        "3-5",
        "6-8",
        "9-11",
        "12-15"
      ),
      include.lowest = TRUE
    )
  }
  else{
    groups <- data[[var]]
  }
  freq <- table(groups)
    frequency_table <- data.frame(
      Category = names(freq),
      Frequency = as.vector(freq),
      Percentage = round(as.vector(prop.table(freq) * 100), 2),
      row.names = NULL
    )
    print(frequency_table)
})
cat("\nFrequency tables provide an overview of how students are distributed across the selected variables.\n")

#2. Measures of central tendency
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
central_tendency <- data.frame(
  Variable = numeric_variables,
  Mean = sapply(data[numeric_variables], mean, na.rm = TRUE),
  Median = sapply(data[numeric_variables], median, na.rm = TRUE),
  Mode = sapply(data[numeric_variables], get_mode),
  row.names = NULL
)
central_tendency
cat("\nInterpretation:\n")
cat("The mean and median describe the typical values of the numerical variables, while the mode identifies the most frequently occurring value.\n")

#3. Measures of dispersion
dispersion <- data.frame(
  Variable = numeric_variables,
  Minimum = sapply(data[numeric_variables], min, na.rm = TRUE),
  Maximum = sapply(data[numeric_variables], max, na.rm = TRUE),
  Range = sapply(data[numeric_variables], function(x) {
    max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  }),
  Variance = sapply(data[numeric_variables], var, na.rm = TRUE),
  Standard_Deviation = sapply(data[numeric_variables], sd, na.rm = TRUE),
  row.names = NULL
)
dispersion
cat("\nInterpretation:\n")
cat("Minimum and maximum show the observed limits, range shows the overall spread, variance measures squared variability, and standard deviation shows the typical distance from the mean.\n")

#4. Percentiles or quartiles
#percentiles
percentiles <- data.frame(
  Variable = numeric_variables,
  P10 = sapply(data[numeric_variables], quantile, probs = 0.10, na.rm = TRUE),
  P25 = sapply(data[numeric_variables], quantile, probs = 0.25, na.rm = TRUE),
  P50 = sapply(data[numeric_variables], quantile, probs = 0.50, na.rm = TRUE),
  P75 = sapply(data[numeric_variables], quantile, probs = 0.75, na.rm = TRUE),
  P90 = sapply(data[numeric_variables], quantile, probs = 0.90, na.rm = TRUE),
  row.names = NULL
)
percentiles

#quartiles
quartiles <- data.frame(
  Variable = numeric_variables,
  Q1 = sapply(data[numeric_variables], quantile, probs = 0.25, na.rm = TRUE),
  Q2 = sapply(data[numeric_variables], quantile, probs = 0.50, na.rm = TRUE),
  Q3 = sapply(data[numeric_variables], quantile, probs = 0.75, na.rm = TRUE),
  row.names = NULL
)
quartiles
cat("\nInterpretation:\n")
cat("P10 represents the 10th percentile, P50 is the median, and P90 represents the 90th percentile.\n\n")
cat("The quartiles show how the observations are distributed across the lower, middle, and upper portions of the dataset.\n")

#5. Correlation analysis 
correlation_matrix <- cor(
  data[numeric_variables],
  use = "complete.obs",
  method = "pearson"
)
round(correlation_matrix, 2)
cat("\nInterpretation:\n")
cat("The correlation matrix can be used to identify which academic, practical experience, and soft-skill variables have stronger or weaker linear relationships with each other.\n")

#6. Appropriate graphical visualizations 
#Placement Status
ggplot(data, aes(x = Placement_Status)) +
  geom_bar() +
  labs(
    title = "Distribution of Placement Status",
    x = "Placement Status",
    y = "Number of Students"
  ) +
  theme_minimal()

#CGPA by Placement Status
ggplot(data, aes(
  x = Placement_Status,
  y = CGPA,
  fill = Placement_Status
)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Placed" = "skyblue",
    "Not Placed" = "plum"
  )) +
  labs(
    title = "CGPA by Placement Status",
    x = "Placement Status",
    y = "CGPA"
  ) +
  theme_minimal()

#Attendance by Placement Status
ggplot(data, aes(
  x = Placement_Status,
  y = Attendance_Percentage,
  fill = Placement_Status
)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Placed" = "skyblue",
    "Not Placed" = "plum"
  )) +
  labs(
    title = "Attendance Percentage by Placement Status",
    x = "Placement Status",
    y = "Attendance Percentage"
  ) +
  theme_minimal()

#Study Hours Per Week by Placement Status
ggplot(data, aes(
  x = Placement_Status,
  y = Study_Hours_Per_Week,
  fill = Placement_Status
)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Placed" = "skyblue",
    "Not Placed" = "plum"
  )) +
  labs(
    title = "Study Hours Per Week by Placement Status",
    x = "Placement Status",
    y = "Study Hours Per Week"
  ) +
  theme_minimal()

#Internships by Placement Status
ggplot(data, aes(
  x = Placement_Status,
  y = Internships,
  fill = Placement_Status
)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Placed" = "lightgreen",
    "Not Placed" = "lightcoral"
  ))+
  labs(
    title = "Internships by Placement Status",
    x = "Placement Status",
    y = "Number of Internships"
  ) +
  theme_minimal()

#Projects Completed by Placement Status
ggplot(data, aes(
  x = Placement_Status,
  y = Projects_Completed,
  fill = Placement_Status
)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Placed" = "lightgreen",
    "Not Placed" = "lightcoral"
  ))+
  labs(
    title = "Projects Completed by Placement Status",
    x = "Placement Status",
    y = "Projects Completed"
  ) +
  theme_minimal()

#Programming Skills by Placement Status
ggplot(data, aes(
  x = Placement_Status,
  y = Programming_Skill,
  fill = Placement_Status
)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Placed" = "lightgreen",
    "Not Placed" = "lightcoral"
  ))+
  labs(
    title = "Programming Skills by Placement Status",
    x = "Placement Status",
    y = "Programming Skill"
  ) +
  theme_minimal()

#Communication Skills by Placement Status
ggplot(data, aes(
  x = Placement_Status,
  y = Communication_Skills,
  fill = Placement_Status
)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Placed" = "#AFEEEE",
    "Not Placed" = "#C4B5FD"
  ))+
  labs(
    title = "Communication Skills by Placement Status",
    x = "Placement Status",
    y = "Communication Skills"
  ) +
  theme_minimal()

#TeamWork by Placement Status
ggplot(data, aes(
  x = Placement_Status,
  y = Teamwork,
  fill = Placement_Status
)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Placed" = "#AFEEEE",
    "Not Placed" = "#C4B5FD"
  ))+
  labs(
    title = "Teamwork Skills by Placement Status",
    x = "Placement Status",
    y = "Teamwork Skill"
  ) +
  theme_minimal()

#Problem Solving by Placement Status
ggplot(data, aes(
  x = Placement_Status,
  y = Problem_Solving,
  fill = Placement_Status
)) +
  scale_fill_manual(values = c(
    "Placed" = "#AFEEEE",
    "Not Placed" = "#C4B5FD"
  ))+
  geom_boxplot() +
  labs(
    title = "Problem Solving Skills by Placement Status",
    x = "Placement Status",
    y = "Problem Solving Skill"
  ) +
  theme_minimal()


# English Proficiency by Placement Status
ggplot(data, aes(
  x = factor(English_Proficiency),
  fill = Placement_Status
)) +
  geom_bar(position = "dodge", na.rm = TRUE) +
  labs(
    title = "English Proficiency by Placement Status",
    x = "English Proficiency",
    y = "Number of Students",
    fill = "Placement Status"
  ) +
  scale_fill_manual(values = c(
    "Placed" = "#AFEEEE",
    "Not Placed" = "#C4B5FD"
  ))+
  theme_minimal()


#Correlation Plot
corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45
)


#Task 3 - Statistical Inference
cat("TASK 3 - STATISTICAL INFERENCE\n")
cat("========================================\n\n")

#Confidence Interval for Placement Status
# Number of students who are placed
placed <- sum(data$Placement_Status == "Placed", na.rm = TRUE)
# Total number of valid students
total <- sum(!is.na(data$Placement_Status))
# Proportion placed
placement_proportion <- placed / total
# 95% Confidence Interval
placement_ci <- prop.test(
  placed,
  total,
  conf.level = 0.95
)
# Display results
cat("Number of placed students:", placed, "\n")
cat("Total valid students:", total, "\n")
cat("Placement proportion:", placement_proportion, "\n")
cat("Placement percentage:", placement_proportion * 100, "%\n")
placement_ci
cat("Research Question: Is the proportion of students who are placed significantly different from 50%?\n")
cat("Null Hypothesis: The population placement proportion is 50% (p = 0.50).\n")
cat("Alternative Hypothesis: The population placement proportion is different from 50% (p ≠ 0.50).\n")
cat("Significance Level: 0.05 (95% confidence level)\n")
cat("Test Statistic: X^2 = 15,769, df = 1\n")
cat("p value: p < < 2.2e-16\n")
cat("Decision: Reject H0\n")
cat("Interpretation: The population placement proportion is different from 50%\n")
cat("The sample placement proportion is 78.08%, which is substantially higher than 50%. The 95% confidence interval for the population placement proportion is approximately 77.71% to 78.44%.\n")

#Two-Sample Hypothesis Tests
test_variables <- c(
  "CGPA",
  "Attendance_Percentage",
  "Study_Hours_Per_Week",
  "Internships",
  "Projects_Completed",
  "Programming_Skill",
  "Communication_Skills",
  "Teamwork",
  "Problem_Solving"
)
for (variable in test_variables) {
  formula <- as.formula(
    paste(variable, "~ Placement_Status")
  )
  
  test <- t.test(formula, data = data)
  
  cat("\n============================================\n")
  cat("Variable:", variable, "\n")
  cat("============================================\n")
  
  cat("Research Problem: Is there a significant difference in the mean",variable, "between placed and non-placed students?\n")
  cat("Null Hypothesis (H0): There is no significant difference in the mean",variable, "between placed and non-placed students.\n")
  cat("Alternative Hypothesis (H1): There is a significant difference in the mean",variable,"between placed and non-placed students.\n")
  cat("Significance Level (alpha): 0.05\n")
  cat("Test Statistic (t):", round(test$statistic, 5),"\n")
  # P-value
  if (test$p.value < 0.001) {
    cat("p-value: < 0.001\n")
  } else {
    cat("p-value:",
        round(test$p.value, 5),
        "\n")
  }
  # Decision and interpretation
  if (test$p.value < 0.05) {
    cat("Decision: Reject H0\n")
    cat("Interpretation: There is statistically significant evidence that the mean", variable, "differs between placed and non-placed students.\n")
  } else {
    cat("Decision: Fail to reject H0\n")
    cat("Interpretation: There is insufficient evidence to conclude that the mean", variable, "differs between placed and non-placed students.\n")
  }
}

#Chi Square Test
#Create contingency table
english_table <- table(
  data$English_Proficiency,
  data$Placement_Status
)
#Perform Chi-square test
english_chi <- chisq.test(english_table)
cat("\n")
cat("Variable: English_Proficiency\n")
cat("Chi-square statistic:", english_chi$statistic, "\n")
cat("p-value:", english_chi$p.value, "\n")
if (english_chi$p.value < 0.05) {
  cat("Decision: Reject H0\n")
} else {
  cat("Decision: Fail to reject H0\n")
}
cat("Research Question: Is there a significant association between English Proficiency and Placement Status among students?\n")
cat("Null Hypothesis: There is no significant association between English Proficiency and Placement Status.\n")
cat("Alternative Hypothesis: There is a significant association between English Proficiency and Placement Status.\n")
cat("Significance Level: 0.05\n")
cat("Test Statistic: 4301.387\n")
cat("p value: p<0.001 (p=0)\n")
cat("Decision: Reject H0\n")
cat("Interpretation: At the 95% confidence level, there is statistically significant evidence of an association between English Proficiency and Placement Status among students.\n")

#Task 4 - Predictive Modeling
cat("TASK 4 - Predictive Modeling\n")
cat("========================================\n\n")
# 1. Feature Selection
selected_variables <- c(
  "CGPA",
  "Attendance_Percentage",
  "Study_Hours_Per_Week",
  "Internships",
  "Projects_Completed",
  "Programming_Skill",
  "Communication_Skills",
  "Teamwork",
  "Problem_Solving",
  "English_Proficiency"
)
model_data <- data[, c(selected_variables, "Placement_Status")]
cat("The following variables were selected as predictors because they represent academic performance, practical experience, and soft skills that may be related to student placement outcomes.\n")
cat("Selected predictor variables:\n")
print(selected_variables)

#2. Data Preprocessing
colSums(is.na(model_data))
model_data <- na.omit(model_data)
model_data$Placement_Status <- as.factor(model_data$Placement_Status)
model_data$English_Proficiency <- as.factor(model_data$English_Proficiency)
cat("\nPlacement_Status was converted to a categorical factor.\n")
cat("English_Proficiency was converted to a categorical factor.\n")
model_data$English_Proficiency <- relevel(
  model_data$English_Proficiency,
  ref = "Advanced"
)
cat("Advanced was selected as the reference category for English_Proficiency.\n")
cat("Placement Status Distribution in Modeling Data:\n")
table(model_data$Placement_Status)

#split data into training and testing sets
set.seed(123)
train_index <- sample(
  1:nrow(model_data),
  size = 0.8 * nrow(model_data)
)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

#3. Train Logistic Regression Model
cat("A logistic regression model was selected because the target variable, Placement_Status, has two possible outcomes: Placed and Not Placed.\n")
cat("The model predicts the probability that a student belongs to the Placed category based on the selected predictor variables.\n")
logistic_model <- glm(
  Placement_Status ~ .,
  data = train_data,
  family = binomial
)
cat("\nLogistic Regression Model Summary:\n")
print(summary(logistic_model))

#4. Model Evaluation
cat("The model is evaluated using the test dataset, which contains observations that were not used during model training.\n")
predicted_probability <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)
predicted_class <- ifelse(
  predicted_probability >= 0.5,
  "Placed",
  "Not Placed"
)

predicted_class <- factor(
  predicted_class,
  levels = levels(test_data$Placement_Status)
)

#confusion matrix
confusion_matrix <- table(
  Actual = test_data$Placement_Status,
  Predicted = predicted_class
)
cat("Confusion Matrix:\n")
print(confusion_matrix)

#Accuracy
cat("Accuracy measures the overall proportion of correct predictions.\n")
accuracy <- sum(diag(confusion_matrix)) /
  sum(confusion_matrix)
cat("Accuracy =", round(accuracy * 100, 2), "%\n")

#Precision
cat("Precision measures how many students predicted as Placed were actually Placed.\n")
precision <- confusion_matrix["Placed", "Placed"] /
  sum(confusion_matrix[, "Placed"])
cat("Precision =", round(precision * 100, 2), "%\n")

#Recall
cat("Recall measures how many of the students who were actually Placed were correctly identified.\n")
recall <- confusion_matrix["Placed", "Placed"] /
  sum(confusion_matrix["Placed", ])
cat("Recall =", round(recall * 100, 2), "%\n")

#F1 score
cat("F1-Score provides a balance between precision and recall.\n")
f1_score <- 2 * (precision * recall) /
  (precision + recall)
cat("F1-Score =", round(f1_score * 100, 2), "%\n")

#5. Interpretation of results
cat("The coefficient shows the direction of the relationship between a predictor and the log-odds of placement.\n")
cat("A positive coefficient indicates higher odds of placement as the predictor increases, while a negative coefficient indicates lower odds.\n")
cat("The p-value is used to assess statistical significance, using 0.05 as the significance level.\n")
cat("The odds ratio is obtained by exponentiating the coefficient and provides a more interpretable measure of the change in odds.\n")
model_summary <- summary(logistic_model)

interpretation_table <- data.frame(
  Variable = rownames(model_summary$coefficients),
  Coefficient = round(model_summary$coefficients[, "Estimate"], 3),
  P_Value = round(model_summary$coefficients[, "Pr(>|z|)"], 4),
  Odds_Ratio = round(exp(model_summary$coefficients[, "Estimate"]), 3),
  row.names = NULL
)
cat("Interpretation Table:\n")
interpretation_table

#Task 5 - Conclusion
cat("TASK 5 - CONCLUSIONS\n")
cat("========================================\n\n")

cat("1. MAJOR FINDINGS\n")
cat("The analysis examined the relationship between academic performance, practical experience, soft skills, student placement outcomes.\n")
cat("The descriptive analysis identified the distribution and characteristics of the selected variables.\n")
cat("The inferential analysis tested whether selected student characteristics differed between placement groups and whether English proficiency was associated with placement status.\n")
cat("The hypothesis tests indicated statistically significant differences for the selected numerical variables between the placement groups. The Chi-square test was also used to examine the relationship between English proficiency and placement status.\n")
cat("The logistic regression model was developed to predict student placement status using the selected academic, practical, and soft-skill variables.\n\n")

cat("2. EDUCATIONAL IMPLICATIONS\n")
cat("The findings provide useful information about factors associated with students' early-career placement outcomes.\n")
cat("Educational institutions can use these findings to understand the importance of academic performance, practical experience, technical skills, and soft skills in students' career preparation.\n")
cat("The results may support the development of academic support programs, internship opportunities, project-based learning, and soft-skill development activities.\n\n")

cat("3. PRACTICAL RECOMMENDATIONS\n")
cat("Students can be encouraged to maintain good academic performance while gaining practical experience through internships and projects.\n")
cat("Career preparation programs can include activities that develop programming skills, communication skills, teamwork, problem-solving abilities, and interview skills.\n")
cat("Universities can also provide additional career guidance and practical training to help students prepare for employment opportunities.\n\n")

cat("4. LIMITATIONS\n")
cat("The analysis is based on the variables and observations available in the dataset.\n")
cat("The dataset is a synthetic dataset, so the findings may not fully represent the characteristics of real-world students or actual employment outcomes.\n")
cat("Removing observations with missing values may reduce the amount of data available for some analyses.\n")
cat("The logistic regression model identifies statistical associations and predictive performance, but the results should not automatically be interpreted as proof of causation.\n")
cat("The selected variables may not capture all factors such as family background, university reputation, networking, industry demand, and interview performance that influence student placement outcomes .\n\n")

cat("5. POSSIBLE FUTURE IMPROVEMENTS\n")
cat("Future studies could include additional variables that may influence student placement outcomes.\n")
cat("More advanced missing-value treatment techniques could be investigated instead of removing observations with missing values.\n")
cat("Future analysis could also compare logistic regression with other classification techniques such as decision trees, random forests, and Naive Bayes.\n\n")

