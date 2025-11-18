#Step 1: Data Import
bmw_data <- read.csv("bmw.csv", stringsAsFactors = FALSE)

# Step 2: Data Overview
# Show first few rows
head(bmw_data)

# Get structure of data
str(bmw_data)

# Summary statistics
summary(bmw_data)

# Step 3: Data Cleaning
# Check for missing values
missing_counts <- colSums(is.na(bmw_data))
print(missing_counts)

# Remove rows with missing values (if any)
bmw_data <- na.omit(bmw_data)


# Step 4: Basic Exploratory Analysis
# Price distribution histogram
hist(bmw_data$price, main="Distribution of BMW Car Prices",
     xlab="Price", col="lightblue", breaks=20)


# Relationship between price and mileage scatter plot
plot(bmw_data$mileage, bmw_data$price,
     main="Price vs Mileage",
     xlab="Mileage", ylab="Price",
     pch=20, col="blue")


#Step 5: Simple Linear Regression (Predicting Price using Mileage)
# Fit model
lm_model <- lm(price ~ mileage, data = bmw_data)



# Plot regression line
plot(bmw_data$mileage, bmw_data$price,
     main="Price vs Mileage with Regression Line",
     xlab="Mileage", ylab="Price", pch=20, col="blue")
abline(lm_model, col="red", lwd=2)




# Step 6: Predicted values and residuals
predicted <- lm_model$fitted.values
residuals <- lm_model$residuals

# Compare actual vs predicted price of first 10 cars
comparison <- data.frame(Actual = bmw_data$price[1:10], Predicted = predicted[1:10])
print(comparison)



# Step 7: Conclusion statement
cat("This simple linear regression model shows how mileage can be used to predict car prices.\n")
