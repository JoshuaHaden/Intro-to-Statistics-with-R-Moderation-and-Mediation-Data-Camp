###Chapter 1 An Introduction to Moderation

###Data Exploration
# The dataset `mod` is preloaded in Data Camp's workspace.

# Summary statistics
describeBy(mod, mod$condition) 

# Create a boxplot of the data
boxplot(mod$iq ~ mod$condition, main = "Boxplot", xlab = "Group condition", ylab = "IQ")

###Calculate Correlations
# Create subsets of the three groups
# Make the subset for the group condition = "control"
mod_control <- subset(mod, mod$condition == "control")
# Make the subset for the group condition = "threat1"
mod_threat1 <- subset(mod, mod$condition == "threat1")
# Make the subset for the group condition = "threat2"
mod_threat2 <- subset(mod, mod$condition == "threat2")

# Calculate the correlations
cor(mod_control$iq, mod_control$wm)
cor(mod_threat1$iq, mod_threat1$wm)
cor(mod_threat2$iq,mod_threat2$wm)

###Model with and without Moderation
# Model without moderation (tests for "first-order effects")
model_1 <- lm(mod$iq ~ mod$wm + mod$d1 + mod$d2)

# Make a summary of model_1
summary(model_1)

# Create new predictor variables
wm_d1 <- mod$wm * mod$d1
wm_d2 <- mod$wm * mod$d2

# Model with moderation
model_2 <- lm(mod$iq ~ mod$wm + mod$d1 + mod$d2 + wm_d1 + wm_d2)

# Make a summary of model_2
summary(model_2)

###Model Comparison
# Compare model_1 and model_2

anova(model_1, model_2)

###Scatterplot
# Choose colors to represent the points by group
color <- c("red","green","blue")


# Illustration of the first-order effects of working memory on IQ
ggplot(mod, aes(x = wm, y = iq)) + geom_smooth(method = "lm", color = "black") + 
  geom_point(aes(color = condition))

# Illustration of the moderation effect of working memory on IQ
ggplot(mod, aes(x = wm, y = iq)) + 
  geom_smooth(aes(group = condition), method = "lm", se = T, color = "black", fullrange = T) +
  geom_point(aes(color = condition))

