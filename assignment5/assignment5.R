
library(dplyr)
getwd()

# Load necessary package
library(dplyr)
library(broom)
library(ggplot2)
install.packages("marginaleffects")
library(marginaleffects)
install.packages("modelsummary")
library(modelsummary)
install.packages("fixest")
library(fixest)
library(haven)

#----------------------------------------------------------------------------------------------
# 1.1) Load the data-set. How many unique states and years are in the data? 
#      Is the panel balanced? 
#-------------------------------------------------------------------------------

df <- read.csv("C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/assignment5/presidential_approval.csv")

length(unique(df$State))
#[1] 50

length(unique(df$Year))
#[1] 32

table(table(df$State))
# 6  8 10 11 12 13 14 15 16 17 19 20 21 22 23 24 25 26 27 
# 5  3  5  2  5  2  3  4  6  5  1  1  1  2  1  1  1  1  1 

# It is not a balanced panel, the states appear different number of times. 

# b) Summary statistics for key variables 
summary(df$PresApprov)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 10.00   37.00   46.00   47.84   58.00   93.00       1 

summary(df$UnemPct)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.300   4.300   5.100   5.302   6.200  13.200 

df_sub = df %>%
  filter(State %in% c("California", "Texas", "NewYork"))

ggplot(df_sub, aes(x = Year, y = PresApprov, color = State)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Year", y = "Presidential approval (%)", color = "State")
  
# Yes the three states move in similar ways. This coordinated movement suggests
# common national factors are the main driver of approval. 

# c) Scatterplot of presidential approval and unemployment across all state-year
# observations. 
ggplot(df, aes(x = UnemPct, y = PresApprov)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Unemployment rate (%)", y = "Presidential approval (%)")

# Looking at the scatter-plot a clear relationship between presidential approval 
# and unemployment rate is not obvious or apparent. Higher unemployment rates 
# are not associated with a higher or lower presidential approval rate. 
# There is a large concentration of data at lower unemployment rates. 

#-------------------------------------------------------------------------------
# 1.2 Pooled OLS 
#-------------------------------------------------------------------------------

# a) Pooled OLS regressing approval on unemployment 
m_pooled = lm(PresApprov ~ UnemPct, data = df)
summary(m_pooled)

# Residuals:
# Min      1Q  Median      3Q     Max 
# -37.836 -10.650  -1.153  10.005  45.149 
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  48.5986     1.8730  25.947   <2e-16 ***
#  UnemPct      -0.1438     0.3387  -0.425    0.671    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 14.44 on 721 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.0002499,	Adjusted R-squared:  -0.001137 
# F-statistic: 0.1802 on 1 and 721 DF,  p-value: 0.6713

# The coefficient on unemployed (-0.1438) says that an increase in unemployment 
# is associated with a decrease in presidential approval rating. 

# b) Adding south as a control 
m_pooled2 = lm(PresApprov ~ UnemPct + South, data = df)
summary(m_pooled2)

# Residuals:
# Min     1Q Median     3Q    Max 
# -36.88 -10.75  -1.22  10.05  46.09 
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   48.719      1.865  26.125  < 2e-16 ***
#  UnemPct       -0.347      0.345  -1.006  0.31474    
# South          3.373      1.214   2.779  0.00559 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 14.38 on 720 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.01086,	Adjusted R-squared:  0.008114 
# F-statistic: 3.953 on 2 and 720 DF,  p-value: 0.01961

## Adding south as a control changes the unemployed coefficient from (-0.1438) 
## (-0.347). This is not a large change, suggesting that the north and south 
## distinction is not a strong factor in unemployment and approval association. 

# c) Reflect on limitations of pooled OLS for this type of data. What kinds of 
#    unobserved, time-invariant differences across states might bias the estimate
#    of unemployment effect? (Give two or three concrete examples)

# Pooled OLS is not ideal for panel data because it ignores unobserved, time
# invariant differences across states that may be correlated with unemployment.

# 1. States with a larger predisposition for natural disasters have structural 
# risks that affect economic stability and unemployment levels that can influence 
# how residents evaluate the president's performance. 

# 2. States differ in long-standing political preferences, depending on their 
# political leaning, states may systematically report different presidential 
# evaluation reports. 

# 3. States specialize in different in different industries, these structural
# differences affect baseline unemployment levels and may shape political 
# attitudes. 

#-------------------------------------------------------------------------------
# 1.3) Entity fixed effects 
#-------------------------------------------------------------------------------

# a) State fixed effects model 
m_fe = feols(PresApprov ~ UnemPct | State, data = df)
modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "markdown")

# How does the coefficient on unemployed change compared to pooled OLS? 

# The coefficient on unemployed changes relative to the pooled OLS, the value 
# increases to -0.451 from -0.144. The FE model is measuring approval changes 
# dependent on unemployment within states, while the OLS is comparing states with 
# different unemployment levels to each other. 

# b) State fixed effects are absorbing all time invariant differences across
#    states. This is why the south variable drops out of the model- it cannot be
#    estimated when state fixed effects are included because it doesn't vary 
#    within a state over time, therefore its effect is indistinguishable from
#    the state specific intercept (fixed-effect). Any time invariant variables
#    are collinear with the set of state dummies and cannot be estimated
#    separately. 

# c) What does the coefficient on UnemPct now identify? In a comment, explain 
# the intuition: the state FE estimator compares approval ratings within the 
# same state across different years, rather than across different states. 
# How does this differ from the pooled OLS interpretation?

# The coefficient on unemployed in the state fixed effect model identifies a 
# within state effect, measuring how approval changes in a given state when its
# unemployment rises or falls- compared to its own average. This differs from 
# pooled OLS because the OLS compares states with different unemployment levels
# to each other while the FE estimator controls for all stable-level confounders 
# but cannot account for time-varying omitted variables. 


#-------------------------------------------------------------------------------
# 1.4) Two-way fixed effects 
#-------------------------------------------------------------------------------

# a-b) Adding year fixed effects to control for common time shocks 
m_twfe = feols(PresApprov ~ UnemPct | State + Year, data = df)

modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe, "Two-Way FE" = m_twfe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "markdown")

# c) Fixed effects interpretation 

# Fixed year effects absorb common time shocks that affect approval in all states
# simultaneously in a given year. Presidential approval changes that occur in all 
# states at once because of the shared macro environment. By adding year dummies, 
# the source of confounding is removed and the effect of a state's unemployment 
# relative to the national average in each year is identified. If the unemployment 
# coefficient changes after adding year fixed effects, it would suggest that common 
# time trends were partly responsible for the relationship estimated with state 
# fixed effects alone. 

#===============================================================================
# PART 2 -----------------------------------------------------------------------
#===============================================================================

df1 <- read_dta("C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/assignment5/teaching_evals.dta")


# 2.1 Data exploration  ---------------------------------------------------

# a) How many unique instructors and courses are in the data
length(unique(df1$InstrID))
# [1] 48

length(unique(df1$CourseID))
# [1] 254

# What is the average # of observations (course-year pairs) per instructor? 
# Is it a short or long panel. 
table(df1$InstrID)
mean(table(df1$InstrID))
#[1] 17.52083

length(unique(df1$InstrID))
length(unique(df1$Year))
#> length(unique(df1$InstrID))
#[1] 48
#> length(unique(df1$Year))
#[1] 9
# The average number of observations per instructor is 17.5. There are many 
# instructors and few years of observations per instructor, the dataset is a 
# short panel. 

# b) Create a scatter plot of eval (y-axis) against apct (x-axis). Add a
# regression. Describe the cross-sectional relationship between grading generosity and 
# evaluations.

ggplot(df1, aes(x = Apct, y = Eval)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Percent receiving A or A- (%)",
       y = "Course evaluation",
       title = "Relationship between grading generosity and evaluations")

# As course evaluations increase, the percentage of high grades also increase.
# The positive relationship is not surprising if the "grading leniency" problem 
# is considered. 


# 2.2) Pooled OLS baseline ------------------------------------------------

# a) Estimate a model with instructor fixed effects, and a 2-way model adding 
# year fixed effects: 

m1 = lm(Eval ~ Apct + Enrollment + Required, data = df1)
print(m1)
# Coefficients:
#(Intercept)         Apct   Enrollment     Required  
#4.3382783    0.3590967   -0.0002407   -0.1217797

# A one percentage point increase in the share of A grades is associated with a 
# 0.3590967 increase in evaluation scores of teachers. 

# b) The OLS estimate of Apct may be biased due to omitted variable bias. 
# There may be some unobserved characteristics of instructors that could 
# simultaneously drive both grading generosity and evaluation scores. Some
# characteristics that could cause this could be teaching quality, a charismatic 
# or friendly demeanor and course difficulty or rigor. The bias of the first two
# examples would be positive and the last one would induce negative bias. 


# 2.3 Fixed effects model  ------------------------------------------------

# a) Estimate a model with instructor fixed effects, and a 2-way model adding 
# year fixed effects:

library(fixest)
m_instr = feols(Eval ~ Apct + Enrollment + Required | InstrID, data = df1)
m_twfe = feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = df1
)


# b) Compare all three models (m1, m_instr, m_twfe) in a single table with 
# standard errors clustered by instructor

modelsummary(
  list("Pooled OLS" = m1, "Instructor FE" = m_instr, "Two-Way FE" = m_twfe),
  vcov = ~InstrID,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

# c) Interpretation of Apct coefficient in instructor FE model. 

# The instructor fixed effect is controlling for unobserved instructor 
# characteristics that do not change over time even if they are unobserved by 
# comparing each instructor to themselves instead of comparing them to each other. 
# The FE coefficient on Apct is smaller than in the pooled OLS. This tells us that 
# omitted variable bias is present in the pooled OLS estimate, upward bias implies 
# that unobserved instructor characteristics that increase evaluation scores are 
# positively correlated with giving more A grades. More lenient graders are 
# systematically better evaluators in terms of their unobserved characteristics.


# 2.4 Random effects and the Hausman test ---------------------------------
# a) Estimate random effects model using plm. 
install.packages("plm")
library(plm)

pdata = pdata.frame(df1, index = c("InstrID", "CourseID"))
m_re = plm(Eval ~ Apct + Enrollment + Required,
           data = pdata, model = "random")

# b) Run the Hausman test to assess whether fixed or random effects is more 
# appropriate. The Hausman test checks whether the random effects assumption 
# (no correlation between unobservables and regressors) holds: 
m_fe_plm = plm(Eval ~ Apct + Enrollment + Required,
               data = pdata, model = "within")
phtest(m_fe_plm, m_re)

# Hausman Test
#data:  Eval ~ Apct + Enrollment + Required
#chisq = 4.9169, df = 3, p-value = 0.178
#alternative hypothesis: one model is inconsistent

# Null hypothesis: random effects estimator is consistent and efficient, 
# individual effects are not correlated with the regressors, RE is preferred. 
#The p-value is 0.178, since this is greater than 0.05 we fail to reject the
# null hypothesis. The test doesn't find evidence that the random effects 
# estimator is inconsistent.
# However given the likelihood of omitted variable bias discussed in the previous 
# section, the fixed effect model is still preferable because it controls for 
# time-invariant characteristics even though the Hausman test doesn't statistically 
# reject RE. 


