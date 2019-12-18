library(perm)
library(np)
library(rdd)

rm(list = ls())
survey_data=read.csv('resource/SleepStudyData.csv')

# Create dummy variables for Enough, PhoneTime, PhoneReach, Breakfast
survey_data$enough_d[survey_data$Enough == 'No'] <- 0
survey_data$enough_d[survey_data$Enough == 'Yes'] <- 1
survey_data$ptime_d[survey_data$PhoneTime == 'No'] <- 0
survey_data$ptime_d[survey_data$PhoneTime == 'Yes'] <- 1
survey_data$preach_d[survey_data$PhoneReach == 'No'] <- 0
survey_data$preach_d[survey_data$PhoneReach == 'Yes'] <- 1
survey_data$breakfast_d[survey_data$Breakfast == 'No'] <- 0
survey_data$breakfast_d[survey_data$Breakfast == 'Yes'] <- 1

# Multiple regression to predict Hours based on all other regressors
model_all = lm(formula = Hours ~ enough_d + ptime_d + preach_d + breakfast_d + Tired, data=survey_data)
summary(model_all)

# Call:
#   lm(formula = Hours ~ enough_d + ptime_d + preach_d + breakfast_d + 
#        Tired, data = survey_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.0880 -0.6515 -0.0270  0.4381  3.9153 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.549870   0.695618   7.978 3.16e-12 ***
#   enough_d     1.076799   0.298968   3.602 0.000503 ***
#   ptime_d      0.583607   0.336456   1.735 0.086026 .  
# preach_d    -0.062070   0.298004  -0.208 0.835446    
# breakfast_d  0.494199   0.281463   1.756 0.082308 .  
# Tired        0.003323   0.145324   0.023 0.981804    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.299 on 96 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.2018,	Adjusted R-squared:  0.1602 
# F-statistic: 4.854 on 5 and 96 DF,  p-value: 0.0005311


# Predict Hours based on Tired score
model_tired = lm(formula = Hours ~ Tired, data=survey_data)
summary(model_tired)

# Call:
#   lm(formula = Hours ~ Tired, data = survey_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.1448 -0.6805  0.0517  0.7347  3.5874 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   7.4841     0.4451  16.815   <2e-16 ***
#   Tired        -0.2679     0.1370  -1.955   0.0533 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.398 on 100 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.03683,	Adjusted R-squared:  0.0272 
# F-statistic: 3.824 on 1 and 100 DF,  p-value: 0.05332
