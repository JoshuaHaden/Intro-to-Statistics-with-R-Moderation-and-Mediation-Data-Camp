###Chapter 3 An Introduction to Mediation

###Data Exploration
# Summary statistics
describeBy(med, med$condition) 

# Create a boxplot of the data
boxplot(med$iq ~ med$cond, main = "Boxplot", xlab = "Group condition", ylab = "IQ")

###Run 3 Regression Models on the Data
# Run the three regression models
model_yx <- lm(med$iq ~ med$condition)
model_mx <- lm(med$wm ~ med$condition)
model_yxm <- lm(med$iq ~ med$condition + med$wm)

# Make a summary of the three models
summary(model_yx)
summary(model_mx)
summary(model_yxm)

###Sobel Test
# Compare the previous results to the output of the sobel function
model_all <- sobel(med$condition, med$wm, med$iq) 

model_all