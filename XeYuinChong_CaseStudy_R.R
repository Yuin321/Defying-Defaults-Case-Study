library (readr)
library (dplyr)
library(caret)
library (ggplot2)
library (randomForest)
library (janitor)

#read datasets
payments <- read_csv("C:\\Users\\yuin3\\Downloads\\Edgered\\Payments.csv",show_col_types = FALSE)
clients <- read_csv("C:\\Users\\yuin3\\Downloads\\Edgered\\Clients.csv",show_col_types = FALSE)

##Data Preprocessing##

#Check for missing values in 'payments'
#Initialize an empty vector to store the results
missing_counts_payments <- vector("integer", ncol(payments))

# Loop through each column in payments
for (i in 1:ncol(payments)) {
    # Count the number of missing values in the current column
  missing_counts_payments[i] <- sum(is.na(payments[, i]))
}

# Display the results
for (i in 1:ncol(payments)) {
    cat("Column", names(payments)[i], "has", missing_counts_payments[i], "missing values.\n")
}

#Repeat for 'clients'
missing_counts_clients <- vector("integer", ncol(clients))

for (i in 1:ncol(clients)) {
  missing_counts_clients[i] <- sum(is.na(clients[, i]))
}

for (i in 1:ncol(clients)) {
  cat("Column", names(clients)[i], "has", missing_counts_clients[i], "missing values.\n")
}

#Check for duplicate records
duplicates_payment <- payments %>%
  filter(duplicated(transaction_id) | duplicated(transaction_id, fromLast = TRUE)) 

duplicates_payment #No duplicates

duplicates_client <- clients %>%
  filter(duplicated(client_id) | duplicated(client_id, fromLast = TRUE)) 

duplicates_client #Found duplicates

#Handle duplicated clients
filtered_clients <- clients %>%
  group_by(client_id) %>%
  filter(!(duplicated(client_id) | duplicated(client_id, fromLast = TRUE) &
             entity_year_established != max(entity_year_established)))

#Check for typos
unique(payments$payment_code)
unique(clients$entity_type)

#Date type conversion
payments <- payments %>%
  mutate(transaction_date = as.POSIXct(transaction_date, origin = "1970-01-01", tz = "UTC")) %>% 
  mutate(transaction_date = format(transaction_date, format = "%Y-%m-%d"))

head(payments)


#Handling Outliers
boxplot(payments$payment_amt, main = "Box Plot of Payment Amounts")

#Use Inter-Quartile Range (IQR) proximity rule
quartiles <- quantile(payments$payment_amt, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(payments$payment_amt)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
payments_no_outlier <- subset(payments, payments$payment_amt > Lower & payments$payment_amt < Upper)

boxplot(payments_no_outlier$payment_amt, main = "Box Plot of Payment Amounts after Cleaning")

#Merge data
merged_df <- merge(payments_no_outlier, filtered_clients, by = "client_id", all = FALSE)
head(merged_df)


##Exploratory Data Analysis (EDA)##


#Histogram for payment_amt
hist(merged_df$payment_amt,
     main = "Histogram of Repayment Amounts",
     xlab = "Repayment Amount",
     ylab = "Frequency",
     col = "skyblue",        # Color of the bars
     border = "black",      # Color of the bar borders
     xlim = c(0, max(merged_df$payment_amt)),  # Adjust x-axis limits
     breaks = 20)

#Calculate mean, median, standard deviation
mean_amount <- mean(merged_df$payment_amt)
median_amount <- median(merged_df$payment_amt)
sd_amount <- sd(merged_df$payment_amt)

#Scatterplot for payment_code, payment_amt, entity_type
ggplot(data = merged_df) +
  aes(x = payment_code, y = payment_amt, color = entity_type) +
  geom_point() +
  labs(x = "Payment Code", y = "Payment Amount", color = "Entity Type") +
  theme_minimal()

#Entity type distribution
frequency_table <- table(merged_df$entity_type)
frequency_df <- as.data.frame(frequency_table)
ggplot(data = frequency_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Entity Type", y = "Frequency", title = "Bar Plot of Frequency by Entity Type") +
  theme_minimal()

#Payment distribution overtime
ggplot(data = merged_df) +
  aes(x = as.Date(transaction_date, origin = "1970-01-01"), group = payment_code, color = payment_code) +
  geom_line(stat = "count") +
  labs(x = "Transaction Date", y = "Count", color = "Payment Code") +
  theme_minimal()

#Entity type and their payment codes
ggplot(data = merged_df) +
  aes(x = entity_type, fill = payment_code) +
  geom_bar() +
  labs(x = "Type", y = "Count", fill = "Payment Code") +
  theme_minimal()


##Feature Engineering##

#Encoding Entity Type
merged_df <- cbind(merged_df, model.matrix(~ entity_type - 1, data = merged_df))

# Remove the original 'entity_type' column
merged_df <- merged_df[, !names(merged_df) %in% c("entity_type")]

#Encode payment_code
merged_df$payment_code <- ifelse(merged_df$payment_code == "PAYMENT", 1, 0)
head(merged_df)

# Calculate the average payment amount per client
average_payment_per_client <- merged_df %>%
  group_by(client_id) %>%
  summarize(average_payment = mean(payment_amt, na.rm = TRUE))

#Barplot to visualize data
ggplot(data = average_payment_per_client) +
  aes(x = client_id, y = average_payment) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(x = "Client ID", y = "Average Payment Amount") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("Average Payment Amount per Client")

# Calculate the time since the last payment for each transaction within each client group

# Sort the data by client_id and transaction_date
merged_df <- merged_df %>%
  arrange(client_id, transaction_date)

merged_df <- merged_df %>%
  group_by(client_id) %>%
  mutate(time_since_last_payment = difftime(transaction_date, lag(transaction_date), units = "days"))

# For the first transaction of each client, 'time_since_last_payment' will be NA; replace it with 0
merged_df$time_since_last_payment[is.na(merged_df$time_since_last_payment)] <- 0

# View the resulting dataset
head(merged_df[c("client_id", "time_since_last_payment", "transaction_date")])

# Calculate the number of previous defaults for each client
merged_df <- merged_df %>%
  group_by(client_id) %>%
  mutate(previous_default = cumsum(payment_code == "0"))

# View the resulting dataset in a bar plot
ggplot(data = merged_df, aes(x = factor(previous_default))) +
  geom_bar() +
  labs(x = "Number of Previous Defaults", y = "Frequency") +
  theme_minimal()

# Calculate the total payments made to date for each client
merged_df <- merged_df %>%
  group_by(client_id) %>%
  mutate(total_payments_to_date = cumsum(payment_amt))

# View the resulting dataset
head(merged_df[c("client_id", "total_payments_to_date", "transaction_date")])

merged_df <- merged_df %>% ungroup()

##Predictive Model##

# Create a new dataframe with only the selected columns for prediction
select_merged_df <- merged_df %>%
  select(-client_id, -transaction_id, -contract_id)

#Clean column names to prevent errors in random forest
cleaned_df <- clean_names(select_merged_df)

#Convert target variable to factor
cleaned_df$payment_code <- as.factor(cleaned_df$payment_code)

#Data Partition
set.seed(222)
ind <- sample(2, nrow(cleaned_df), replace = TRUE, prob = c(0.7, 0.3))
train <- cleaned_df[ind==1,]
test <- cleaned_df[ind==2,]

#Finds best optimized value of random variable
bestmtry <- tuneRF(train,train$payment_code,stepFactor = 1.2, improve = 0.01, trace=T, plot= T)

#random forest
rf <- randomForest(payment_code~., data=train, mtry=4, proximity=FALSE)
print(rf)

#Check importance of variables
varImpPlot(rf)

#Predict on test data
pred_test <- predict(rf, newdata = test, type= "class")

# The prediction to compute the confusion matrix and see the accuracy score
confusionMatrix(table(pred_test,test$payment_code))
