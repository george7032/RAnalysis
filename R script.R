#Importing dataset

library(readxl)
culture <- read_excel("C:/Users/Kimani/Desktop/culture.xlsx")
View(culture)
#Installing and Loading packages
install.packages(c("tidyverse", "car", "ggplot2", "broom", "ggpubr", "multcomp"))

View(culture)
library(tidyverse)

print(culture)
# A tibble: 103 × 13


library(car)
library(carData)
library(ggplot2)
library(ggpubr)
library(multcomp)

library(mvtnorm)
library(survival)
library(TH.data)
library(MASS)
library(dplyr)
install.packages("psych")
library(psych)

#Descriptive statistics
summary(culture$AgeGroups)
summary(culture[, c("DD", "KA", "CFB", "DB", "EFB", "HE")])


describe(culture[, c("DD", "KA", "CFB", "DB", "EFB", "HE")])
# distribution of scores within each culture
scores_by_culture <- culture %>%
  +     +     +     gather(key = "Task", value = "Score", -c(AgeGroups, Sex)) %>%
  +     +     +     ggplot(aes(x = Task, y = Score, fill = data$Culture)) +
  +     +     +     geom_boxplot() +
  +     +     +     facet_wrap(~data$Culture, scales = "free_y") +
  +     +     +     labs(title = "Distribution of Scores by Task and Culture")
#Group Comparison
library(dplyr)

culture %>%
  +     group_by(Culture) %>%
  +     summarise(mean_DD = mean(DD, na.rm = TRUE), sd_DD = sd(DD, na.rm = TRUE), n = n())
# A tibble: 1 × 4
Culture mean_DD sd_DD     n
<chr>     <dbl> <dbl> <int>
  1 Ovambo    0.408 0.494   103
# the above was the group comparison for the DD tasks between cultures
#group comparison for the 'KA' task between cultures
#We will start by removing non-numerical values from the 'KA' column
culture$KA <- as.numeric(as.character(culture$KA))

group_comparison_culture_ka <- culture %>%
  +     +     group_by(Culture) %>%
  +     +     summarise(mean_KA = mean(KA, na.rm = TRUE), sd_KA = sd(KA, na.rm = TRUE), n = n())

culture %>%
  +     group_by(Culture) %>%
  +     summarise(mean_KA = mean(KA, na.rm = TRUE), sd_KA = sd(KA, na.rm = TRUE), n = n())


# Group comparison for the 'HE' task between cultures
culture$HE <- as.numeric(as.character(culture$HE))

group_comparison_culture_he <- culture %>%
  +     +     group_by(Culture) %>%
  +     +     summarise(mean_HE = mean(HE, na.rm = TRUE), sd_HE = sd(HE, na.rm = TRUE), n = n())

culture %>%
  +     group_by(Culture) %>%
  +     summarise(mean_HE = mean(HE, na.rm = TRUE), sd_HE = sd(HE, na.rm = TRUE), n = n())
#Correlation analysis between 'Age' and '5PointScale'
culture$`5PointScale` <- as.numeric(as.character(culture$`5PointScale`))
cor.test(culture$AgeGroups, culture$`5PointScale`, method = "pearson", na.rm = TRUE)



#To analyze the distribution on the scores on the 'DD' task, we will create a boxplot to visualize the distribution
boxplot_dd_age <- culture %>%
  +     ggplot(aes(x = as.factor(AgeGroups), y = DD)) +
  +     geom_boxplot(fill = "skyblue") +
  +     labs(title = "Distribution of DD Scores by Age Groups", x = "Age Groups", y = "DD Scores")

print(boxplot_dd_age)


#ANOVA for 'DD' scores by Age Groups
anova_dd_age <- aov(DD ~ as.factor(AgeGroups), data = culture)
print(anova_dd_age)

summary(anova_dd_age)

#removing rows that have missing values

culture_cleaned <- culture %>%
  +     drop_na(DD, KA, CFB, DB, EFB, HE)
print(culture_cleaned)

#Impute missing values

culture_imputed <- culture %>%
  +     mutate(across(c("DD", "KA", "CFB", "DB", "EFB", "HE"), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

culture %>%
  +     mutate(across(c("DD", "KA", "CFB", "DB", "EFB", "HE"), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

#columns for correlation analysis
numeric_cols <- culture[, c("DD", "KA", "CFB", "DB", "EFB", "HE")]

print(numeric_cols)


culture_numeric <- culture %>%
  +     mutate_at(vars(-Culture), as.numeric, na.rm = TRUE)

numeric_cols <- culture_numeric[, c("DD", "KA", "CFB", "DB", "EFB", "HE")]
correlation_matrix <- cor(numeric_cols, use = "complete.obs")
print(correlation_matrix)


#the above displays how to calculate the correlation matrix


#Group Comparisons
group_comparison_culture <- culture_imputed %>%
  +     group_by(Culture) %>%
  +     summarise(
    +         mean_DD = mean(DD, na.rm = TRUE),
    +         mean_KA = mean(KA, na.rm = TRUE),
    +         mean_CFB = mean(CFB, na.rm = TRUE),
    +         mean_DB = mean(DB, na.rm = TRUE),
    +         mean_EFB = mean(EFB, na.rm = TRUE),
    +         mean_HE = mean(HE, na.rm = TRUE)
    +     )

print(group_comparison_culture)



group1_scores <- culture_numeric$KA[culture_numeric$Culture == "Group1"]
group2_scores <- culture_numeric$KA[culture_numeric$Culture == "Group2"]

# Check if both groups have enough observations
if (length(group1_scores) < 2 || length(group2_scores) < 2) {
  +     print("Not enough observations in one or both groups.")
  + } else {
    +     # Your code for further analysis goes here
      + }

#More on data visualization

ggplot(culture, aes(x = DD)) +
  +     geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  +     labs(title = "Distribution of DD Scores", x = "DD Scores", y = "Frequency")

# the above is the distribution of DD scores (Histogram for Numeric variables)


#Below is the density plot of DD scores

ggplot(culture, aes(x = DD)) +
  +     geom_density(fill = "skyblue") +
  +     labs(title = "Density Plot of DD Scores", x = "DD Scores", y = "Density")

#Visualizing relationships
#boxplots by age groups
ggplot(culture, aes(x = as.factor(AgeGroups), y = DD)) +
  +     geom_boxplot(fill = "skyblue") +
  +     labs(title = "Distribution of DD Scores by Age Groups", x = "Age Groups", y = "DD Scores")

#visualizing group comparison
#bar plots for mean scores
ggplot(culture, aes(x = as.factor(AgeGroups), y = DD)) +
  +     geom_boxplot(fill = "skyblue") +
  +     labs(title = "Distribution of DD Scores by Age Groups", x = "Age Groups", y = "DD Scores")



#T-Test Visualization( mean scores by culture)
ggplot(culture, aes(x = Culture, y = KA, fill = Culture)) +
  +     geom_boxplot() +
  +     labs(title = "Distribution of KA Scores by Culture", x = "Culture", y = "KA Scores") +
  +     stat_compare_means(method = "t.test")


# Visualization - Distribution of scores within each culture
scores_by_culture <- culture %>%
  +     gather(key = "Task", value = "Score", -c(SubjectNumber, Culture, AgeGroups, Sex)) %>%
  +     ggplot(aes(x = Task, y = Score, fill = Culture)) +
  +     geom_boxplot() +
  +     facet_wrap(~Culture, scales = "free_y") +
  +     labs(title = "Distribution of Scores by Task and Culture")
print(scores_by_culture)



table(subset_data$Culture)
#our results returns one level(Ovambo)


#t-test to compare the 'DD' scores between the Ovambo and German cultures
subset_data_two_cultures <- subset(culture, Culture %in% c("Ovambo", "German"))
unique(subset_data_two_cultures$Culture)

table(subset_data_two_cultures$Culture)


t_test_result_DD <- t.test(DD ~ Culture, data = subset_data_two_cultures, na.rm = TRUE)



print(scores_by_culture3)
scores_by_culture <- culture %>%
  +     gather(key = "Task", value = "Score", -c("AgeGroups", "Sex", "Culture")) %>%
  +     ggplot(aes(x = Task, y = Score, fill = Culture)) +
  +     geom_boxplot() +
  +     facet_wrap(~Culture, scales = "free_y") +
  +     labs(title = "Distribution of Scores by Task and Culture") +
  +     theme(
    +         axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Adjust text size and rotation
    +         plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm")  # Add larger margins
    +     )

print(scores_by_culture)
#the display on the Y-axis is too small. I have tested various ways to make shorter margins but all in vain



# Data visualization for EFB, CFB, DB, and HE
scores_by_culture_other_tasks <- culture %>%
  +     gather(key = "Task", value = "Score", -c(AgeGroups, Sex, Culture, `5PointScale`, `6PointScale`)) %>%
  +     ggplot(aes(x = Task, y = Score, fill = Culture)) +
  +     geom_boxplot() +
  +     facet_wrap(~Culture, scales = "free_y") +
  +     labs(title = "Distribution of Scores by Task and Culture") +
  +     theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(scores_by_culture_other_tasks)

# Correlation analysis between Age and 5PointScale
cor_test_result <- cor.test(culture$AgeGroups, culture$`5PointScale`, method = "pearson", na.rm = TRUE)
print(cor_test_result)


# ANOVA for 'DD' scores by Age Groups
anova_dd_age <- aov(DD ~ as.factor(AgeGroups), data = culture)
summary(anova_dd_age)


# Distribution of scores by Age Groups
boxplot_dd_age <- culture %>%
  +     ggplot(aes(x = as.factor(AgeGroups), y = DD)) +
  +     geom_boxplot(fill = "skyblue") +
  +     labs(title = "Distribution of DD Scores by Age Groups", x = "Age Groups", y = "DD Scores")



#***************************************************************************
#***********German Sample***********************

#loading the german sample 
#i saved the file as german, which is an excel file.

library(readxl)
german <- read_excel("C:/Users/Kimani/Desktop/german.xlsx")
View(german)

print(german)

str(german)



summary(german)

head(german)

#Frequency table for categorical variables (replace column names as needed)
table(german$AgeGroups)


table(german$Sex)


# Visualize Age distribution
hist(german$Ageyearmonth, main = "Age Distribution", xlab = "Age", col = "skyblue", border = "black")


# Boxplot for 5PointScale and 6PointScale
boxplot(german$`5PointScale`, main = "Boxplot of 5PointScale", ylab = "5PointScale", col = "lightgreen", border = "black")

boxplot(german$`6PointScale`, main = "Boxplot of 6PointScale", ylab = "6PointScale", col = "lightgreen", border = "black")


# Correlation matrix (for numeric variables)
cor(german[, c("Ageyearmonth", "DD", "KA", "CFB", "DB", "EFB", "HE", "5PointScale", "6PointScale")])

# Scatterplot matrix (for numeric variables)
pairs(german[, c("Ageyearmonth", "DD", "KA", "CFB", "DB", "EFB", "HE", "5PointScale", "6PointScale")])

#Sex Barchart
barplot(table(german$Sex), main = "Bar Chart of Sex", xlab = "Sex", col = "lightgreen", border = "black")



#we will use pair plot to visualize the relationship between variables
library(ggplot2)
pairs(~ Ageyearmonth + DD + KA + CFB + DB + EFB + HE + `5PointScale` + `6PointScale`, data = german)


#group comparison
boxplot(Ageyearmonth ~ Sex, data = german, main = "Boxplot of Age by Sex", ylab = "Age", col = c("lightblue", "lightgreen"))
german$Sex <- toupper(german$Sex)
t_test_result <- t.test(Ageyearmonth ~ Sex, data = german)

print(t_test_result)


boxplot(Ageyearmonth ~ Sex, data = german, main = "Boxplot of Age by Sex", ylab = "Age", col = c("lightblue", "lightgreen"))

hist(german$Ageyearmonth, main = "Age Distribution", xlab = "Age", col = "skyblue", border = "black")


#sex bar chart
german$Sex <- toupper(german$Sex)
barplot(table(german$Sex), main = "Bar Chart of Sex", xlab = "Sex", col = "lightgreen", border = "black")

#boxplot for age by sex
boxplot(Ageyearmonth ~ Sex, data = german, main = "Boxplot of Age by Sex", ylab = "Age", col = c("lightblue", "lightgreen"))



#Correlation analysis
correlation_matrix <- cor(german[, c("Ageyearmonth", "DD", "KA", "CFB", "DB", "EFB", "HE", "5PointScale", "6PointScale")])

print(correlation_matrix)

# Assuming you want to perform ANOVA on the 5PointScale variable based on the AgeGroups

anova_result <- aov(`5PointScale` ~ AgeGroups, data = german)

print(anova_result)

print(summary(anova_result))

#removing rows that have missing values
german <- na.omit(german)

print(german)



t_test_sex_result <- t.test(german$`5PointScale` ~ german$Sex, var.equal = TRUE)

print(t_test_sex_result)



print(summary(t_test_sex_result))

# paired t-test for the 5PointScale variable between AgeGroups 4 and 5
paired_t_test_result <- t.test(german$`5PointScale` ~ german$AgeGroups, subset = (german$AgeGroups %in% c(4, 5)) & complete.cases(german$`5PointScale`), paired = TRUE)

# Print the results
print(paired_t_test_result)


# T-test for 5PointScale between German and Culture datasets
t_test_result <- t.test(german$`5PointScale`, culture$`5PointScale`, var.equal = TRUE)

# Print the results
print(t_test_result)

#Boxplot Comparison: Create boxplots to visually compare the distributions of the 5PointScale variable.
boxplot(german$`5PointScale`, culture$`5PointScale`, names = c("German", "Culture"), main = "Comparison of 5PointScale", col = c("lightgreen", "lightblue"))

#T-Test: Perform a t-test to compare the means between the two datasets.

t_test_result <- t.test(german$`5PointScale`, culture$`5PointScale`, var.equal = TRUE)
print(t_test_result)

#Correlation Matrix: Check if there are any correlations between variables in each dataset.

cor_matrix_german <- cor(german[, c("5PointScale", "Ageyearmonth")])
cor_matrix_culture <- cor(culture[, c("5PointScale", "SomeOtherVariable")])



#Histograms: Compare histograms of the 5PointScale variable in each dataset.
hist(german$`5PointScale`, col = "lightgreen", main = "Histogram of 5PointScale - German", xlab = "5PointScale")
hist(culture$`5PointScale`, col = "lightblue", main = "Histogram of 5PointScale - Culture", xlab = "5PointScale", add = TRUE)


oshivambo <- culture
par(mfrow = c(2, 1))  # To create a 2-row layout

hist(german$`5PointScale`, col = "lightgreen", main = "Histogram of 5PointScale - German", xlab = "5PointScale")
hist(oshivambo$`5PointScale`, col = "lightblue", main = "Histogram of 5PointScale - Oshivambo", xlab = "5PointScale")



hist(german$`6PointScale`, col = "lightcoral", main = "Histogram of 6PointScale - German", xlab = "6PointScale")
hist(oshivambo$`6PointScale`, col = "lightpink", main = "Histogram of 6PointScale - Oshivambo", xlab = "6PointScale")




# Rename the 'culture' dataset to 'oshivambo'
library(readxl)
culture <- read_excel("C:/Users/Kimani/Desktop/culture.xlsx")
View(culture)



oshivambo <- culture
par(mfrow = c(2, 1))  # To create a 2-row layout

hist(german$`5PointScale`, col = "lightgreen", main = "Histogram of 5PointScale - German", xlab = "5PointScale")
hist(oshivambo$`5PointScale`, col = "lightblue", main = "Histogram of 5PointScale - Oshivambo", xlab = "5PointScale")



hist(german$`6PointScale`, col = "lightcoral", main = "Histogram of 6PointScale - German", xlab = "6PointScale")
hist(oshivambo$`6PointScale`, col = "lightpink", main = "Histogram of 6PointScale - Oshivambo", xlab = "6PointScale")


par(mfrow = c(2, 1))  # To create a 2-row layout

hist(german$`5PointScale`, col = "lightgreen", main = "Histogram of 5PointScale - German", xlab = "5PointScale")
hist(oshivambo$`5PointScale`, col = "lightblue", main = "Histogram of 5PointScale - Oshivambo", xlab = "5PointScale")


# Histograms: Compare histograms of the 5PointScale variable in each dataset.
hist(german$`5PointScale`, col = "lightgreen", main = "Histogram of 5PointScale - German", xlab = "5PointScale")
hist(oshivambo$`5PointScale`, col = "lightblue", main = "Histogram of 5PointScale - Oshivambo", xlab = "5PointScale", add = TRUE)



hist(german$`5PointScale`, col = "lightgreen", main = "Histogram of 5PointScale - German", xlab = "5PointScale")
hist(oshivambo$`5PointScale`, col = "lightblue", main = "Histogram of 5PointScale - Oshivambo", xlab = "5PointScale")
par(mfrow = c(2, 1))  # To create a 2-row layout

hist(german$`5PointScale`, col = "lightgreen", main = "Histogram of 5PointScale - German", xlab = "5PointScale")
hist(oshivambo$`5PointScale`, col = "lightblue", main = "Histogram of 5PointScale - Oshivambo", xlab = "5PointScale")



par(mfrow = c(2, 1))  # To create a 2-row layout

hist(german$`5PointScale`, col = "lightgreen", main = "Histogram of 5PointScale - German", xlab = "5PointScale")
hist(oshivambo$`5PointScale`, col = "lightblue", main = "Histogram of 5PointScale - Oshivambo", xlab = "5PointScale")


# Histograms: Compare histograms of the 5PointScale variable in each dataset.
hist(german$`5PointScale`, col = "lightgreen", main = "Comparison of 5PointScale - German", xlab = "5PointScale")
hist(oshivambo$`5PointScale`, col = "lightblue", main = "Comparison of 5PointScale - Oshivambo", xlab = "5PointScale", add = TRUE)

# Histograms: Compare histograms of the 5PointScale variable in each dataset.
hist(german$`5PointScale`, col = "lightgreen", main = "Comparison of 5PointScale - German", xlab = "5PointScale")
hist(oshivambo$`5PointScale`, col = "lightblue", main = "Comparison of 5PointScale - Oshivambo", xlab = "5PointScale", add = TRUE)


# Histograms: Compare histograms of the 6PointScale variable in each dataset.
hist(german$`6PointScale`, col = "lightgreen", main = "Comparison of 6PointScale - German", xlab = "6PointScale")
hist(oshivambo$`6PointScale`, col = "lightblue", main = "Comparison of 6PointScale - Oshivambo", xlab = "6PointScale", add = TRUE)

# Histograms: Compare histograms of the 6PointScale variable in each dataset.
hist(german$`6PointScale`, col = "lightgreen", main = "Comparison of 6PointScale - German", xlab = "6PointScale")
hist(oshivambo$`6PointScale`, col = "lightblue", main = "Comparison of 6PointScale - Oshivambo", xlab = "6PointScale", add = TRUE)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  +     install.packages("dplyr")
  + }
library(dplyr)

german$Culture <- "German"
german$Culture <- as.factor(german$Culture)
group_comparison_German <- german %>%
  +     group_by(Culture) %>%
  +     summarise(
    +         mean_DD = mean(DD),
    +         mean_KA = mean(KA),
    +         mean_CFB = mean(CFB),
    +         mean_DB = mean(DB),
    +         mean_EFB = mean(EFB),
    +         mean_HE = mean(HE)
    +     )
print(group_comparison_German)