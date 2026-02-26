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
#1.7 Residual diagnostics
