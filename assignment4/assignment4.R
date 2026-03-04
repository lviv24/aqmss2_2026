library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(readstata13)

# 1.1 a)
df = read.dta13("C:/Users/lesle/R/aqmss2_2026/assignment4/corruption.dta")

# b) Drop observations with missing values on key variables 
df = df %>% filter(!is.na(ti_cpi) & !is.na(undp_gdp))
nrow(df)

# c) summary statistics : 
summary(df$ti_cpi)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.200   2.500   3.300   4.051   4.900   9.700

sd(df$ti_cpi)
# [1] 2.105143

summary(df$undp_gdp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 520    1974    5280    8950   10862   61190 

sd(df$undp_gdp)
# [1] 9986.849

# The standard deviation for GDP is much larger than the median and it is also 
# larger than the mean, this indicates  a skew to the right. 

# 1.2 Exploratory visualization 
# a) create a scatterplot of ti_cpi (y-axis) against undp_gdp (x-axis) using geompoint
# add smooth line with geom smooth
ggplot(df, aes(x = undp_gdp, y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "GDP per capita (PPP)", y = "Corruption Perceptions Index")
ggsave("assignment4/scatter1_2.png" )
# b) describe pattern
#!!!!!!!!!!!!!!!!!!

# c) second scatter plot with log(undp_gdp) on x- axis
ggplot(df, aes(x = log(undp_gdp), y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "log(GDP per capita)", y = "Corruption Perceptions Index")
# Yes, the log transformation of GDP makes the spread of the data larger, 
# creating a more linear relationship that is easier to see in the scatter-plot. 

#1.3 Bivariate regression 

# a) Estimate a bivariate regression of corruption on GDP per capita 
m1 = lm(ti_cpi ~ undp_gdp, data = df)

# b) Print results using summary or broom tidy, interpret coefficient on undp_gdp.
# What is the predicted change in corruption index for a $10k increase in GDP 
# per capita? 

tidy(m1)

# A tibble: 2 × 5
# term        estimate  std.error statistic  p.value
# <chr>          <dbl>      <dbl>     <dbl>    <dbl>
#   1 (Intercept) 2.50     0.124           20.1 1.37e-46
# 2 undp_gdp    0.000173 0.00000929      18.6 1.12e-42

# The coefficient for undp_gdp indicated the predicted change for corupption index 
# for a one dollar increase in GDP per capita, if a $10,0000 increase occurs, 
# multiply the coefficient by 10,000.

coef(m1)["undp_gdp"] * 10000
# undp_gdp 
# 1.729782 

# c) predicted corruption at 25th and 75th percentiles of GDP.

q25 = quantile(df$undp_gdp, 0.25)
q75 = quantile(df$undp_gdp, 0.75)
c(q25, q75)
#25%      75% 
#  1974.25 10862.50 

predictions(m1, newdata = datagrid(undp_gdp = c(q25, q75)))

#undp_gdp Estimate Std. Error    z Pr(>|z|)     S
#1974     2.84     0.1130 25.2   <0.001 462.0
#10862     4.38     0.0942 46.5   <0.001   Inf
#2.5 % 97.5 %
#  2.62   3.07
#4.20   4.57

# The difference in the predicted corruption for a country at the 75th percentile
# and the 25th percentile of GDP represents the interquartile range. This value 
# is 4.38 - 2.84 = 1.54. The interquartile effect is 1.54. So the predicted 
# score of a country at the 75th percentile of GDP is 1.54 points higher than a 
# country at the 25th percentile.  
# The confidence intervals represent the precision of the predictions, at the 
# 25th percentile of GDP the 95% CI is [2.62, 3.07] and at the 75th percentile of
# GDP the 95% CI is [4.20, 4.57]

# 1.4) Non- linear specifications 
# a) Log model 
m2 = lm(ti_cpi ~ log(undp_gdp), data = df)
tidy(m2)
# A tibble: 2 × 5
#erm         estimate std.error statistic  p.value
#<chr>           <dbl>     <dbl>     <dbl>    <dbl>
#  1 (Intercept)     -8.11    0.769      -10.6 2.76e-20
#2 log(undp_gd…     1.43    0.0896      16.0 1.74e-35

# b) Interpret coefficient on log(undp_gdp). Create a prediction plot. 
coef(m2)["log(undp_gdp)"] * log(2)

# log(undp_gdp) 
# 0.9915562
gdp_seq <- seq(min(df$undp_gdp, na.rm=TRUE),
               max(df$undp_gdp, na.rm=TRUE),
               length.out = 100)

newdata <- data.frame(undp_gdp = gdp_seq)

newdata$pred <- predict(m2, newdata = newdata)

plot(df$undp_gdp, df$ti_cpi,
     xlab = "GDP per capita",
     ylab = "Predicted Corruption (CPI)",
     main = "Prediction Plot: GDP and Corruption")

lines(newdata$undp_gdp, newdata$pred, lwd = 2)

# c) Estimate a model with quadratic GDP term
m3 = lm(ti_cpi ~ undp_gdp + I(undp_gdp^2), data = df)
tidy(m3)

# A tibble: 3 × 5
#term         estimate std.error statistic  p.value
#<chr>           <dbl>     <dbl>     <dbl>    <dbl>
#  1 (Intercept)   2.14e+0  1.42e- 1     15.1  4.71e-33
#2 undp_gdp      2.63e-4  2.15e- 5     12.2  5.83e-25
#3 I(undp_gdp^… -2.49e-9  5.42e-10     -4.60 8.35e- 6

# d) Compare the R^2 of all three models. Which specification fits the data the
# best? Explain why a non-linear specification might be appropriate. 

r2 = c(
  "Level-Level" = summary(m1)$r.squared,
  "Level-Log" = summary(m2)$r.squared,
  "Quadratic" = summary(m3)$r.squared)
r2

# Level-Level   Level-Log   Quadratic 
# 0.6734049   0.6025131   0.7101202 

# The quadratic model fits the data the best because it has the highest value of 
# R^2 at 0.71. This indicates that it explains the largest share of variation in
# corruption. A non-linear model is best to explain the relationship between GDP 
# and corruption because the effects of GDP on corruption changes a lot across 
# income levels. This varying relationship is best represented by a quadratic 
# model. 

# 1.5 Marginal effects 
# a) For the log model (m2), compute the average marginal effect of GDP using
# avg slopes. 
avg_slopes(m2, variables = "undp_gdp")
# Estimate Std. Error  z Pr(>|z|)     S    2.5 %
#0.000524   3.28e-05 16   <0.001 188.0 0.000459
#97.5 %
#0.000588

# b) Explain why AME differs from raw coefficient on log(undp_gdp)
#!!!!!!!!!!!!!!!!!

# c) Marginal effects of quadratic model at specific GDP values. 

slopes(m3, variables = "undp_gdp",
       newdata = datagrid(undp_gdp = c(2000, 10000, 30000)))

#undp_gdp Estimate Std. Error     z Pr(>|z|)     S
#2000 0.000254   1.96e-05 12.94   <0.001 124.8
#10000 0.000214   1.25e-05 17.15   <0.001 216.5
#30000 0.000114   1.56e-05  7.32   <0.001  41.9
#2.5 %   97.5 %
#  2.15e-04 0.000292
#1.89e-04 0.000238
#8.34e-05 0.000144

# The marginal effect of GDP gets smaller as countries becomes richer. The impact
# of GDP is strongest in poor countries. 

#1.6 Prediction plots 
p1 = plot_predictions(m2, condition = "undp_gdp")
p1

ggsave("pred_plot_m2.png", p1, width = 6, height = 4)

# Prediction plot for quadratic model 
p2 = plot_predictions(m3, condition = "undp_gdp")
p2
ggsave("pred_plot_m3.png", p2, width = 6, height = 4)

# c) Comparing the two plots, they both show sharp decline in corruption with 
# an initial increase in GDP- eventually leveling off at higher economic levels. 
# The log model has a smoother curve. 

#1.7 Residual diagnostics
# a) Residuals vs fitted for level-level model
m1_aug = augment(m1)
ggplot(m1_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Level (m1)")

# The plot suggests heteroskedasticity because the spread of residuals 
# increases with fitted values. There is a curved pattern meaning the linear 
# specification missed the non-linear relationship. 

# b) Residuals vs. fitted for the log model 

m2_aug = augment(m2)

ggplot(m2_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Log (m2)")

# Yes the log transformation improves the residual pattern. The data is more 
# spread out, reducing the curvature. 

# c) Cook's distance for influential observations 

n = nrow(df)
threshold = 4 / n
cooks_d = cooks.distance(m2)
influential = which(cooks_d > threshold)
df$cname[influential]

#[1] "Australia"         "Bhutan"           
#[3] "Canada"            "Denmark"          
#[5] "Equatorial Guinea" "Ethiopia"         
#[7] "Finland"           "Iceland"          
#[9] "Malawi"            "Netherlands"      
#[11] "New Zealand"       "Singapore"        
#[13] "Sweden"            "United Kingdom" 

plot(m2, which = 4)

# d) The influential observations should not be removed, as a robustness check
# I would re-estimate the model excluding influential observations and comparing
# coefficients. If the results end up being similar it means the estimates are 
# robust. 

# 1.8 Publication-quality table 
# a) regression table comparing all 3 models 

modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "markdown")

# b) For a final presentation I would choose the level-log model because it has
# the most significant coefficients with the highest R^2 and best residuals. 
# This allows for a clear interpretation of the relationship between wealth and 
# corruption. 

#######################################################################################

#PART 2!!!!!!!!!!!!!!!!!!!
library(foreign)

#2.1 Data Exploration 

# a) Data set & summary statistics
df1 = read.dta("C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/assignment4/infantmortality.dta")
summary(df1)
print(summary(df1))
nrow(df1)

#country             region              income         infant      
#Length:101         Length:101         Min.   :  50   Min.   :  9.60  
#Class :character   Class :character   1st Qu.: 130   1st Qu.: 26.20  
#Mode  :character   Mode  :character   Median : 334   Median : 60.60  
#Mean   :1022   Mean   : 89.05  
#3rd Qu.:1191   3rd Qu.:129.40  
#Max.   :5596   Max.   :650.00  
#oil           
#Length:101        
#Class :character  
#Mode  :character  
# There are 101 countries .

# b) Create a histogram of infant and a histogram of income. Are either of them 
# right skewed? 

#infant mortality histogram 
hist(df1$infant,
     main = "Histogram of Infant Mortality",
     xlab = "Infant Mortality",
     col = "lightblue")

#income  histogram 
hist(df1$income,
     main = "Histogram of Income",
     xlab = "Income",
     col = "lightgreen")

# Yes, both histograms are right-skewed (long tails to the right).

# c) Create a scatter-plot of infant (y-axis) against income(x-axis), coloring
# points by region, describe relationship in a comment. 
df1$region <- as.factor(df1$region)

par(mar = c(5,4,4,8), xpd = TRUE)

plot(df1$income, df1$infant,
     col = as.numeric(df1$region),
     pch = 19,
     xlab = "Income",
     ylab = "Infant Mortality")

legend("topright",
       inset = c(-0.30, 0),
       legend = levels(as.factor(df1$region)),
       col = 1:length(levels(as.factor(df1$region))),
       pch = 19,
       bty = "n")

# There is a negative relationship observed. As income increases infant 
# mortality decreases. The wealthiest regions are Europe, then the Americas, 
# followed by Asia. 

# d) Create the same scatter-plot but using log(income) on the x-axis 
# log(infant) on the y-axis. Does the log-log relationship look more linear? 

df1$region <- as.factor(df1$region)

par(mar = c(5,4,4,2))

plot(log(df1$income), log(df1$infant),
     col = as.numeric(df1$region),
     pch = 19,
     xlab = "Log(Income)",
     ylab = "Log(Infant Mortality)")

legend("topright",
       legend = levels(df1$region),
       inset = c(-0.25, 0),
       col = 1:length(levels(df1$region)),
       pch = 19,
       bty = "n")
# Yes the log-log relationship looks more linear, the curve reduced and the
# negative relationship is more notable with a straight downward line. 

# 2.2 Comparing specifications 
# a) Estimate a level-level model: 
m1 =lm(infant ~ income, data = df1)
print(m1)

# Coefficients:
# (Intercept)       income  
# 110.42109     -0.02091  

# Estimate a log-log model: 
m2 = lm(log(infant) ~ log(income), data = df1)
print(m2)

# Coefficients:
# (Intercept)  log(income)  
# 7.1458      -0.5118 

# c) Interpret coefficient of each model. 
# For m1 the coefficient is -0.02091, this means that for every dollar 
# increase, infant mortality decreases by .02091 deaths per 1,000 births. 
# The predicted change in infant mortality for a $1,000 increase in income would 
# be -0.02091*1000 = -20.91. So a $1,000 increase in income is associated with 
# a decrease of 20.9 infant deaths per 1,000 live births. 


# The coefficient on log(income) is an elasticity of -0.5118 meaning that a 1% 
# increase in income is associated with a 0.5118% decrease in infant mortality. 
# A 10% increase in income would be associated with a 5.118% decrease in infant 
# deaths/mortality. 

# d) Create a residuals vs. fitted values plot for both models. Which 
# specification has a better residual pattern? Discuss in a comment.

# Residual plot for m1 (level-level)
plot(fitted(m1), resid(m1),
     pch = 19,
     col = "blue",
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted (Level-Level)")
abline(h = 0, lwd = 2)

# Residual plot for m2 (log-log)
plot(fitted(m2), resid(m2),
     pch = 19,
     col = "red",
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted (Log-Log)")

abline(h = 0, lwd = 2)

# M2 (log-log model) has a better residual because the data is more randomly 
# spread around zero with more constant variance- creating a better fit. 

#2.3 Multiple regression with controls 

# a) Estimate a log-log model with controls for region and oil-exporting status
m3 = lm(log(infant) ~ log(income) + region + oil, data = df1)

# b)
print(m3)
#Coefficients:
#  (Intercept)     log(income)  regionAmericas  
# 6.5521         -0.3398         -0.5498  
# regionAsia    regionEurope          oilyes  
# -0.7129         -1.0338          0.6402  

# Controlling for region and oil produces a smaller coefficient value of -0.3398
# while the previous log-log model had a coefficient value of -0.5118. This is 
# a slight change in the effect induced by the controls.

# c) Interpret the coefficient on the Africa region indicator. What does this 
# tell you about the infant mortality in Africa, controlling for income? 

# Africa is the reference variable so the intercept value (6.5521) indicates 
# that at the baseline income and non-oil status there is a child mortality rate
# of 6.552, the mortality rate is much higher than all other regions.

# d) Average marginal effects
avg_slopes(m3)
# The average marginal effect of income is -0.00159 with a p value of <0.001, 
# this is significant. This means that after taking the average across all 
# regions finding that a 1-unit increase in income decreases the infant mortality 
# rate by 0.00159 deaths per 1,000 births. 

# 2.4 Interaction: oil status & income. 

# a) Estimate a model with an interaction between oil status and log income. 
m4 = lm(log(infant)~log(income)*oil+region, data=df1)

# b) Use avg_slopes to compute the marginal effect of income separately for
# oil-exporting and non-oil countries 
avg_slopes(m4, variables ="income", by ="oil")

# c) The relationship between income and infant mortality differs for oil 
# countries. In non oil exporting countries an increase in income reduces the 
# mortality rate by .00209 per 1,000 births (this is significant), for 
# oil-exporting countries an increase in income (1 unit) increases the mortality
# rate by 0.0011 deaths per 1,000 live births (also significant). 

# d) Plot how marginal effect of income varies by oil status 
ps <- plot_slopes(m4, variables = "income", condition = "oil")

ggsave("income_marginal_by_oil.png", plot = ps, width = 7, height = 5)

# 2.5 Predicted values for specific scenarios 
# a) Using model m3 (no interaction), compute predicted infant mortality rates for 
# • A non-oil African country with income = $1,000
# • A non-oil European country with income = $20,000
# • An oil-exporting country in the Americas with income = $10,000

predictions(m3,
            newdata = datagrid(
              income = c(1000, 20000, 10000),
              region = c("Africa", "Europe", "Americas"),
              oil = c("no", "no", "yes")))


# Make newdata with scenarios
newdata <- data.frame(
  income = c(1000, 20000, 10000),
  region = c("Africa", "Europe", "Americas"),
  oil = c("no", "no", "yes")
)

# Get predictions
pred_log <- predictions(model = m3, newdata = newdata)

# Convert back to original scale
pred_log$infant_pred <- exp(pred_log$estimate)

# Add scenario labels
pred_log$Scenario <- c(
  "Non-oil Africa, $1,000",
  "Non-oil Europe, $20,000",
  "Oil Americas, $10,000"
)
# Inspect column names
names(pred_log)
# Display results (use backticks for columns with spaces)
pred_log[, c("Scenario", "infant_pred", "conf.low", "conf.high")]

# 2.5 % 97.5 %
# 3.89   4.52
# 1.77   2.53
# 2.97   4.05

# b) For non-oil Africa the infant mortality confidence intervals range from 
# 3.89 -4.52. For non-oil Europe the infant mortality rates range from 1.77-2.53
# and for oil exporting Americas the infant mortality ranges from 2.97 - 4.05. 
# There is a large gap between European and African mortality rates with a 
# difference of 2.05 deaths per 1,000 births. The results are plausible because 
# the intervals for Africa and Europe do not overlap, reinforcing the 
# statistical meaningful difference in values. 

# 2.6 Publication-quality visualization 
# a) Create a prediction plot showing predicted infant mortality across income 
# levels by region. 
pred_plot<-plot_predictions(m3, 
                            condition = c("income", "region")
                            )+
  labs(
    x = "Income (USD)",
    y = "Predicted Infant Mortality",
    title = "Predicted Infant Mortality Across Income Levels by Region"
  )+ 
  theme_minimal(base_size = 14)+ 
  scale_color_brewer(palette = "Set2")+
  theme(legend.title= element_blank())

pred_plot
ggsave("predicted_infant_by_income_region.png", pred_plot, width = 8, height = 5)

# b) The plot displays a clear negative relationship between income and infant 
# mortality rates across all regions. As income increases, infant mortality 
# decrease. However, there are clear geographic distinctions with Africa having 
# the highest predicted mortality rate at all income levels and Europe having 
# the lowest predicted infant mortality rate. Limitations to this study include 
# omitted variables such as education, healthcare infrastructure, and access to 
# vital resources such as clean water. 

# 2.7 Diagnostics & Robust inference 

# a) Create a residuals vs fitted values plot for m3, is there heteroskedasticity? 
# Augment model for residuals
m3_aug <- augment(m3)

# Residuals vs Fitted plot
ggplot(m3_aug, aes(x = .fitted, y = .resid)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = "Residuals vs Fitted Values: Model m3"
  ) +
  theme_minimal(base_size = 14)
# The plot does not suggest heteroskedasticity as most of the residuals stay 
# close to the central horizontal line with few outliers. This suggests that the 
# variance of errors is constant.

# b) Regression table comparing all 4 models with robust standard errors. 
modelsummary(
  list("Level" = m1, "Log-Log" = m2,
       "Controls" = m3, "Interaction" = m4),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

# c) Compare robust and default standard errors for m3. Run modelsummary with 
# and without vcov = robust. Do conclusions change? 
# None of the standard errors change dramatically except for regions that export 
# oil- there was a decrease in the robust standard error calculation, however 
# it is not significant. Most SE's increase slightly but the significance levels 
# remain unchanged. If heteroskedasticity is present it does not change the model 
# conclusions substantively. It is important to use robust standard errors because
# it produces more reliable inferences when residual variance may not be constant
# across observations, it avoids heteroskedasticity. 

