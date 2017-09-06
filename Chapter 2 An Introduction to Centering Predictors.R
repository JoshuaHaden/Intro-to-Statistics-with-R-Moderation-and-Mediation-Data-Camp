###Chapter 2 An Introduction to Centering Predictors

###Centering Data
# Define wm_center
wm_center <- mod$wm - mean(mod$wm)

# Compare with the variable wm.centered
all.equal(wm_center, mod$wm.centered)

###Centering Versus No Centering
# Model without moderation and with centered data
model_1_centered <- lm(mod$iq ~ mod$wm.centered + mod$d1 + mod$d2)

# Make a summary of model_1_centered
summary(model_1_centered)

###Centering Versus No Centering with Moderation
# Create new predictor variables  
wm_d1_centered <- mod$wm.centered * mod$d1
wm_d2_centered <- mod$wm.centered * mod$d2

# Define model_2_centered
model_2_centered <- lm(mod$iq ~ mod$wm.centered + mod$d1 + mod$d2 + wm_d1_centered + wm_d2_centered)

# Make a summary of model_2_centered
summary(model_2_centered)

###Model Comparison
# Compare model_1_centered and model_2_centered
anova(model_1_centered, model_2_centered)

# Compare model_1 and model_2
anova(model_1, model_2)

###Some Correlations
# Calculate the correlations between working memory capacity and the product terms
cor_wmd1 <- cor(mod$wm, wm_d1)
cor_wmd2 <- cor(mod$wm, wm_d2)
cor_wmd1_centered <- cor(mod$wm.centered, wm_d1_centered)
cor_wmd2_centered <- cor(mod$wm.centered, wm_d2_centered)



# Calculate the correlations between the dummy variables and the product terms
cor_d1d1<- cor(mod$d1, wm_d1)
cor_d2d2 <- cor(mod$d2, wm_d2)
cor_d1d1_centered <- cor(mod$d1, wm_d1_centered)
cor_d2d2_centered <- cor(mod$d2, wm_d2_centered)

# correlations
rbind(c(cor_wmd1, cor_wmd2), c(cor_wmd1_centered, cor_wmd2_centered))
rbind(c(cor_d1d1, cor_d2d2), c(cor_d1d1_centered, cor_d2d2_centered))



