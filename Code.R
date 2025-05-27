
install.packages("naniar")
install.packages("VIM")
install.packages("readxl")
install.packages("dplyr")

library(readxl)
library(dplyr)

install.packages("openxlsx")
library(openxlsx)
Data <- read.xlsx("D:/AIUB/10th semester courser/data/dataset.xlsx", sheet = 1)
Data
is.na(Data)
View(Data)

sum(is.na(Data))
colSums(is.na(Data))
rowSums(is.na(Data))

cleaned_data <- na.omit(Data)

Data$age[is.na(Data$age)] <- mean(Data$age, na.rm = TRUE)

Data$pressurehight[is.na(Data$pressurehight)] <- median(Data$pressurehight, na.rm = TRUE)

mode_val <- names(sort(table(Data$gender), decreasing = TRUE))[1]
Data$gender[is.na(Data$gender)] <- mode_val

install.packages("naniar")
library(naniar)

gg_miss_var(Data)
gg_miss_case(Data)

vis_miss(Data)


hist(Data$age,
     col = "skyblue",       
     main = "Histogram of Age",  
     xlab = "Age",          
     ylab = "Frequency",    
     breaks = 5)           

boxplot(Data$age, main = "Boxplot", ylab = "Values")


Q1 <- quantile(Data$pressurelow, 0.25)
Q3 <- quantile(Data$pressurelow, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

outliers <- Data$pressurelow[Data$pressurelow < lower_bound | Data$pressurelow > upper_bound]
pressurelow_median <- median(Data$pressurelow, na.rm = TRUE)

Data$pressurelow[Data$pressurelow < lower_bound | Data$pressurelow > upper_bound] <- pressurelow_median


Data$age_group <- cut(Data$age, breaks = c(0, 18, 30, 45, 60, 100), 
                      labels = c("0-18", "18-30", "31-45", "46-60", "61-100"))

Data$gender_numeric <- ifelse(Data$gender == "Male", 1, 2)


Data$age_normalized <- (Data$age - min(Data$age)) / (max(Data$age) - min(Data$age))
Data$pressurelow_normalized <- (Data$pressurelow - min(Data$pressurelow)) / (max(Data$pressurelow) - min(Data$pressurelow))

duplicates <- Data[duplicated(Data), ]

Data_cleaned <- Data[!duplicated(Data), ]

sum(duplicated(Data))  

duplicates <- Data[duplicated(Data), ]

filtered_data_age<- Data[Data$age < 85, ]
View(filtered_data_age)

invalid_data <- Data %>%
  filter(age < 0 | pressurehight < 0 | pressurelow < 0 | impluse < 0)

View(invalid_data)

Data$age[Data$age < 0 | Data$age > 120] <- NA
Data$pressurehight[Data$pressurehight < 0] <- NA
Data$pressurelow[Data$pressurelow < 0] <- NA
Data$impluse[Data$impluse < 0] <- NA

Data$age[is.na(Data$age)] <- median(Data$age, na.rm = TRUE)
Data$pressurehight[is.na(Data$pressurehight)] <- median(Data$pressurehight, na.rm = TRUE)
Data$pressurelow[is.na(Data$pressurelow)] <- median(Data$pressurelow, na.rm = TRUE)
Data$impluse[is.na(Data$impluse)] <- median(Data$impluse, na.rm = TRUE)

unique(Data$gender)

Data$gender <- tolower(Data$gender) 
Data$gender <- ifelse(grepl("^fem", Data$gender), "Female",
                      ifelse(grepl("^male", Data$gender), "Male", NA))

unique(Data$gender)

table(Data$class)
prop.table(table(Data$class)) 

positive_class <- Data %>% filter(class == "positive")
negative_class <- Data %>% filter(class == "negative")

set.seed(123)
negative_oversampled <- negative_class %>% sample_n(size = nrow(positive_class), replace = TRUE)
balanced_data <- bind_rows(positive_class, negative_oversampled)
balanced_data <- balanced_data %>% sample_frac(1)

table(balanced_data$class)


set.seed(123)
positive_undersampled <- positive_class %>% sample_n(size = nrow(negative_class))
balanced_data <- bind_rows(negative_class, positive_undersampled)
balanced_data <- balanced_data %>% sample_frac(1)

table(balanced_data$class)

install.packages("caret")
library(caret)

set.seed(123)

split_index <- createDataPartition(balanced_data$class, p = 0.7, list = FALSE)

train_data <- balanced_data[split_index, ]
test_data  <- balanced_data[-split_index, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Testing set size:", nrow(test_data), "\n")

table(train_data$class)
table(test_data$class)

library(dplyr)
library(ggplot2)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
summary_stats <- Data %>%
  group_by(gender) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    mode_age = get_mode(age)
  ) %>%
  tidyr::pivot_longer(cols = c(mean_age, median_age, mode_age),
                      names_to = "Statistic", values_to = "Age")

ggplot(summary_stats, aes(x = gender, y = Age, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Mean, Median, and Mode of Age by Gender",
       x = "Gender", y = "Age") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

ggplot(Data, aes(x = gender, y = age, fill = gender)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Gender",
       x = "Gender", y = "Age") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

library(dplyr)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
get_mode <- function(v) { 
  v <- na.omit(v)  # remove NAs
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

age_glucose_stats <- Data %>%
  group_by(glucose) %>%
  summarise(
    count = n(),
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    mode_age = get_mode(age)
  )
print(age_glucose_stats)

install.packages("tidyr")  
library(tidyr)

plot_data <- age_glucose_stats %>%
  pivot_longer(cols = c(mean_age, median_age, mode_age),
               names_to = "Statistic", values_to = "Age")

ggplot(plot_data, aes(x = glucose, y = Age, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Central Tendencies of Age by Glucose Level",
       x = "Glucose Level", y = "Age") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

range_df <- Data %>%
  group_by(gender) %>%
  summarise(
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    range = max_age - min_age
  )

print(range_df)
ggplot(range_df, aes(x = gender, y = range, fill = gender)) +
  geom_col() +
  labs(title = "Range of Age by Gender", x = "Gender", y = "Range (Max - Min)") +
  theme_minimal()


iqr_df <- Data %>%
  group_by(gender) %>%
  summarise(
    IQR = IQR(age, na.rm = TRUE)
  )

print(iqr_df)
ggplot(Data, aes(x = gender, y = age, fill = gender)) +
  geom_boxplot() +
  labs(title = "Interquartile Range (IQR) of Age by Gender", x = "Gender", y = "Age") +
  theme_minimal()

variance_df <- Data %>%
  group_by(gender) %>%
  summarise(
    variance = var(age, na.rm = TRUE)
  )

print(variance_df)
ggplot(variance_df, aes(x = gender, y = variance, fill = gender)) +
  geom_col() +
  labs(title = "Variance of Age by Gender", x = "Gender", y = "Variance") +
  theme_minimal()


