library(readr)
library(dplyr)
library(caret)
library(rpart)
library(xgboost)
df = read_csv("C:\\Users\\user\\Desktop\\ds_salaries.csv")
df$company_size = replace(df$company_size,df$company_size=='L',1)
df$company_size = replace(df$company_size,df$company_size=='M',2)
df$company_size = replace(df$company_size,df$company_size=='S',3)
df2= df%>% group_by(df$company_size)%>% summarise(total_count = n())%>% as.data.frame()
df2
df = select(df,-experience_level,-employment_type,-job_title,-salary,-salary_currency,-employee_residence,-remote_ratio,-company_location)
#write.csv(new_df, file = "C:\\Users\\user\\Desktop\\2aditya.csv", row.names = FALSE) 
df
##################################################TRAIN&TEST
set.seed(123)
train_proportion <- 0.7
train_indices <- createDataPartition(df$salary_in_usd, p = train_proportion, list = FALSE)
training_data <- df[train_indices, ]
testing_data <- df[-train_indices, ]
#write.csv(training_data, file = "C:\\Users\\user\\Desktop\\2adityaTrain.csv", row.names = FALSE) 
#write.csv(testing_data, file = "C:\\Users\\user\\Desktop\\2adityaTest.csv", row.names = FALSE) 
################################Model
# Assuming your data frame is named 'df'
model <- lm(salary_in_usd ~ work_year + company_size, data = training_data)
# Make predictions on the test data
predictions <- predict(model, newdata = testing_data)
###########################ACCURACY
# Calculate RMSE
rmse <- sqrt(mean((testing_data$salary_in_usd - predictions)^2))

# Calculate the range of the target variable
range_of_target_variable <- max(testing_data$salary_in_usd) - min(testing_data$salary_in_usd)

# Calculate percentage accuracy
percentage_accuracy <- (range_of_target_variable - rmse) / range_of_target_variable * 100

# Display the percentage accuracy
cat("Percentage Accuracy:", percentage_accuracy, "%\n")

