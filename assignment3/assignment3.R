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

anes <- read.csv("C:/Users/lesle/R/aqmss2_2026/assignment3/anes_timeseries_2020.csv")


# Define key variables

df = anes %>%
  transmute(
    voted = ifelse(V202109x < 0, NA, V202109x),
    age = ifelse(V201507x < 0, NA, V201507x),
    female = case_when(V201600 == 2 ~ 1, V201600 == 1 ~ 0, TRUE ~ NA_real_),
    education = case_when(
      V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~ 14,
      V201511x == 4 ~ 16, V201511x == 5 ~ 20, TRUE ~ NA_real_),
    income = ifelse(V201617x < 0, NA, V201617x),
    party_id = ifelse(V201231x < 0, NA, V201231x)
  )

# Keep only rows with no missing values in the key variables
# b) Drop rows with missing values only for key variables
anes_clean <- df %>%
  filter(!is.na(voted) & 
           !is.na(age) & 
           !is.na(female) &
           !is.na(education) & 
           !is.na(income) & 
           !is.na(party_id))

# Number of remaining observations
nrow(anes_clean)
# Number of remaining observations
nrow(anes_clean)


# c) Compute turnout rate and summary statistics
# Turnout rate
mean(anes_clean$voted)
#0.9583333

# Overall turnout rate & Summary statistics
mean(anes_clean$voted)
#0.9583333
summary(anes_clean)
# voted             age            female     
#Min.   :0.0000   Min.   :26.00   Min.   :0.000  
#1st Qu.:1.0000   1st Qu.:42.50   1st Qu.:0.000  
#Median :1.0000   Median :50.00   Median :1.000  
#Mean   :0.9583   Mean   :50.96   Mean   :0.625  
#3rd Qu.:1.0000   3rd Qu.:57.50   3rd Qu.:1.000  
#Max.   :1.0000   Max.   :72.00   Max.   :1.000  
#education       income.V1          party_id   
#Min.   :10   Min.   :-1.5922379   Min.   :1.00  
#1st Qu.:12   1st Qu.:-0.5554318   1st Qu.:1.00  
#Median :14   Median :-0.2592015   Median :3.00  
#Mean   :15   Mean   :-0.0493717   Mean   :3.75  
#3rd Qu.:16   3rd Qu.: 0.6665182   3rd Qu.:6.00  
#Max.   :20   Max.   : 1.3700652   Max.   :7.00


# 1.2 Exploratory Visualization 
# a) Create a bar chart showing turnout rate by education level (compute mean 
# voted for each value of education, then use geom_col())

# Compute turnout by education using original dataset
turnout_by_edu <- anes %>%
  # Recode missing/invalid values on the fly
  mutate(
    voted = ifelse(V202109x < 0, NA, V202109x),
    education = case_when(
      V201511x == 1 ~ 10,
      V201511x == 2 ~ 12,
      V201511x == 3 ~ 14,
      V201511x == 4 ~ 16,
      V201511x == 5 ~ 20,
      TRUE ~ NA_real_
    )
  ) %>%
  # Group and compute mean turnout, ignoring NAs
  group_by(education) %>%
  summarise(turnout = mean(voted, na.rm = TRUE))

# Make education a factor for proper x-axis
turnout_by_edu$education <- factor(turnout_by_edu$education,
                                   levels = sort(unique(turnout_by_edu$education)))

# Plot
ggplot(turnout_by_edu, aes(x = education, y = turnout)) +
  geom_col() +
  labs(x = "Years of Education", y = "Turnout Rate") +
  theme_minimal()


# b) Describe the pattern, does turnout increase with education? 
# Yes the plot does indicate a correlation with years of education and turnout 
# levels. People with more years of education are more likely to report voting.


#1.3 Linear probability model 
#a-b)  Estimate lpm = lm(voted ~ age + education + income + female, data = df).
lpm = lm(voted ~ age + education + income + female, data = anes_clean)
tidy(lpm)
print(lpm)
#Coefficients:
#(Intercept)          age    education       income  
#0.239934     0.004127     0.019264     0.008230  
#female  
#0.034399  

#c) Interpret coefficient on education in a comment- what does it mean in terms
# probability? 
# The coefficient on education means that each additional year of education is 
# associated with a 0.019 increase in the likelihood of voting.

#d) Check predicted probabilities 
preds_lpm = predict(lpm)
sum(preds_lpm < 0)
# Amount below zero : [1] 0

sum(preds_lpm > 1)
# Amount above 1: [1] 802

range(preds_lpm) 
#minimum and maximum values 
#[1] 0.5150876 1.1708206

#1.4 Logistic regression 
#a-b) logit model estimate 
logit = glm(voted ~ age + education + income + female,
            family = binomial, data = anes_clean)
tidy(logit)
print(logit)
## A tibble: 5 × 5
#term        estimate std.error statistic  p.value
#<chr>          <dbl>     <dbl>     <dbl>    <dbl>
#  1 (Intercept)  -4.05     0.266      -15.2  1.99e-52
#2 age           0.0367   0.00226     16.2  2.69e-59
#3 education     0.222    0.0172      12.9  2.87e-38
#4 income        0.0713   0.00620     11.5  1.23e-30
#5 female        0.296    0.0764       3.87 1.08e- 4

#c) Compute & Interpret Odds ratio 
exp(coef(logit))
#(Intercept)         age   education      income       female
#0.01746474  1.03735990  1.24898963  1.07389559    1.34418610
 
# The odds ratio for education means that the multiplicative change in odds of 
# voting for each additional year of education, if the odds ratio is over 1, 
# it means that more education is associated with a higher odds of voting. 

#d) verify all predicted probabilities are between 0 & 1 (bounded)
preds_logit = predict(logit, type = "response")
range(preds_logit)
#[1] 0.2511085 0.9945010 (all between 0 and 1)

#1.5) Comparing LPM & Logit 
#a) Compute average marginal effects for logit model
avg_slopes(logit)

# Term Contrast Estimate Std. Error     z
#age          dY/dX  0.00382   0.000226 16.90
#education    dY/dX  0.02314   0.001760 13.15
#female       1 - 0  0.03101   0.008041  3.86
#income       dY/dX  0.00742   0.000633 11.72
#Pr(>|z|)     S   2.5 %  97.5 %
#  <0.001 210.3 0.00338 0.00426
#<0.001 128.8 0.01969 0.02659
#<0.001  13.1 0.01525 0.04677
#<0.001 103.0 0.00618 0.00866

#Type: response

#b) Compare the AME to the LPM coefficients
# The AME's from the logit model are similar to the LPM coefficients. 

#c) Create a table using modelsummary() showing LPM & Logit side by side. Use 
# robust standard errors for the LPM 
modelsummary(list("LPM" = lpm, "Logit" = logit),
             vcov = list("robust", NULL), output = "markdown")
#+-------------+-----------+-----------+
#|             | LPM       | Logit     |
#+=============+===========+===========+
#| (Intercept) | 0.240     | -4.048    |
#+-------------+-----------+-----------+
#|             | (0.029)   | (0.266)   |
#+-------------+-----------+-----------+
#| age         | 0.004     | 0.037     |
#+-------------+-----------+-----------+
#|             | (0.000)   | (0.002)   |
#+-------------+-----------+-----------+
#| education   | 0.019     | 0.222     |
#+-------------+-----------+-----------+
#|             | (0.001)   | (0.017)   |
#+-------------+-----------+-----------+
#| income      | 0.008     | 0.071     |
#+-------------+-----------+-----------+
#|             | (0.001)   | (0.006)   |
#+-------------+-----------+-----------+
#| female      | 0.034     | 0.296     |
#+-------------+-----------+-----------+
#|             | (0.008)   | (0.076)   |
#+-------------+-----------+-----------+
#| Num.Obs.    | 6733      | 6733      |
#+-------------+-----------+-----------+
#| R2          | 0.110     |           |
#+-------------+-----------+-----------+
#| R2 Adj.     | 0.110     |           |
#+-------------+-----------+-----------+
#| AIC         | 4038.4    | 4646.7    |
#+-------------+-----------+-----------+
#| BIC         | 4079.3    | 4680.8    |
#+-------------+-----------+-----------+
#| Log.Lik.    | -2013.218 | -2318.343 |
#+-------------+-----------+-----------+
#| RMSE        | 0.33      | 0.32      |
#+-------------+-----------+-----------+
#| Std.Errors  | HC3       |           |
#+-------------+-----------+-----------+

#1.6 Predicted probabilities 
#a) Use plot predictions to plot the predicted probability of voting across edu
# levels. 
p1 = plot_predictions(logit, condition = "education")
p1

ggsave("pred_prob_education.png", p1, width = 6, height = 4)

#b) Create second plot showing predicted probabilities across age for men and 
# women separately.
p2 = plot_predictions(logit, condition = c("age", "female"))
p2
ggsave("pred_prob_age_gender.png", p2, width = 6, height = 4)

#c) Describe the patterns, how does the effect of age differ from the effect of
# education? 
# The reported voting turnout has the same effect across genders and age. 

#1.7) Presenting results. 
#a) Create a coefficient plot comparing the LPM and logit models using modelplot. 
p3 = modelplot(list("LPM" = lpm, "Logit" = logit),
               vcov = list("robust", NULL))
p3

#b) Save the plot 
ggsave("coefplot_lpm_logit.png", p3, width = 6, height = 4)

#c) Do LPM & Logit lead to different conclusions, when might differences matter?
# LPM and logit have very similar results. Age, education, income along with 
# gender have positive associations with turnout. Differences between LPM and
# logit matter more when the predicted probabilities are close the boundaries
# of 0 or 1. 

###############################################################################
#PART 2! (STAR - High school graduation)

star <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/star/star.csv")

#Key Variables, data prep 
# Create factors
star <- subset(star, !is.na(hsgrad))

star$classtype <- factor(star$classtype,
                         levels = c(1,2,3),
                         labels = c("Small","Regular","Regular+Aid"))


# Now create small
star$small <- ifelse(star$classtype == "Small", 1, 0)

# Check variation
table(star$small)
star$race <- factor(star$race,
                    levels = c(1,2,3,4,5,6),
                    labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))
table(star$small)
table(star$race)

#d) Graduation rate
mean(star$hsgrad)
#[1] 0.8332786 overall graduation rate is 83%

# Graduation rate by class
aggregate(hsgrad ~ classtype, data = star, mean)
#classtype      hsgrad
#1  Small      0.8359202
#2  Regular    0.8251619
#3 Regular+Aid 0.8392857

# The overall graduation rate is 83%. The class type with the highest graduation
# rate is regular + aid, followed by small class type (83% graduation rate) 
#and with regular class type with the lowest graduation rate of 82%.


#2.2 LPM & Logit 
#a) estimate an LPM predicting hsgrad from small
lpm1 = lm(hsgrad ~ small, data = star)
print(lpm1)
#Coefficients:
#(Intercept)        small  
#0.832168     0.003752  

star <- subset(star, !is.na(hsgrad))
star$small <- ifelse(star$classtype == "Small", 1, 0)
#b) estimate a logit model with same predictor
logit1 = glm(hsgrad ~ small, family = binomial, data = star)
print(logit1)
#Coefficients:
#(Intercept)        small  
#1.60107      0.02711   

#c) Interpret the LPM coefficient on small, what is the estimated difference in 
# graduation probability between small and non-small classes? 

# The coefficient in the LPM testing the interaction between small class size 
# and graduation rate indicates that non-small class sizes have a graduation 
# rate of 83.2%. The difference in graduation probability between small class 
# size students and non-small class size students is 0.375%.

#d) AME from logit, how does it compare to the LPM coefficient? 
avg_slopes(logit1, variables = "small")

#Estimate Std. Error     z Pr(>|z|)   S   2.5 %
#0.00375     0.0147 0.255    0.799 0.3 -0.0251
#97.5 %
#0.0326

# The AME from logit is 0.00375 and the LPM coefficient for "small" is 0.003752.
# Suggesting there is no statistically meaningful effect in neither of the models.


#2.3) ADDING CONTROLS 
#a) Estimate both LPM & logit with controls
lpm2 = lm(hsgrad ~ small + race + yearssmall, data = star)
logit2 = glm(hsgrad ~ small + race + yearssmall,
             family = binomial, data = star)
print(lpm2)
print(logit2)

#b) Compare coefficient on small between bivariate and controlled models, what 
# is the change, what does it say about randomization? 
# The coefficient on small goes from 0.00375 in the bivariate lpm to -0.07556 in 
# the controlled lpm. The small coefficient goes from 0.003752 to -0.5620 in the 
# controlled logit. Once other factors are controlled for being in a small class
# is associated with a lower graduation possibility.  
# Randomization is fairly balanced across race and yearssmall, if it were 
# perfect, controlling for covariates wouldn't change the coefficient by much. 
# There are some imbalances in covariates since small changed a bit, but the 
# effect is still pretty small.
 
#c) Interpret coefficient on yearssmall from logit model, use avg slopes to
# convert to marginal effect. 

# The coefficient of yearssmall from logit is 0.2096, this is the log odds effect, 
# meaning that each additional year in a small class increases the log-odds of 
# graduating by 20%

avg_slopes(logit2, variables = "yearssmall")
#Estimate Std. Error    z Pr(>|z|)    S  2.5 %
#0.0283     0.0077 3.67   <0.001 12.0 0.0132
#97.5 %   0.0434

# The AME of yearssmall from logit indicates that each additional year 
# a student spends in a small class increases their likelihood of graduation by 
# 2.8 percentage points. It is statistically significant because the p value is 
# under 0.001. 

#2.4 PREDICTED PROBABILITIES 

#a) Using the controlled logit model compute predicted graduation probabilities
# for white student in small classes for 3 years & black student in regular 
# class with 0 years in small classes. 

newdata <- data.frame(
  small = c(1, 0),          # small class vs non-small
  race = c(1, 2),           # White = 1, Black = 2
  yearssmall = c(3, 0)      # 3 years in small vs 0
)

preds <- predictions(logit2, newdata = newdata, type = "response")
print(preds)

#Estimate Std. Error    z Pr(>|z|)   S 2.5 % 97.5 %
#0.864     0.0115 75.1   <0.001 Inf 0.842  0.887
#0.748     0.0160 46.8   <0.001 Inf 0.716  0.779

#The coefficient for white student in a small class for 3 years indicates an
# 86.4 % predicted probability fo graduating. The coefficient for black student 
# in a regular class with 0 years in small classes indicates a 74.8% predicted 
# probability of graduating.

#b) Plot prediction graduation probabilites across yearssmall for small vs
# non small classes. 
plot_predictions(logit2, condition = c("yearssmall", "small"))


#2.5 interactions 
#a) does small class effect on graduation differ by race? estimate. 
logit3 = glm(hsgrad ~ small * race + yearssmall,
             family = binomial, data = star)

#b) compute ME of small separately for each race. 
avg_slopes(logit3, variables = "small", by = "race")

#race Estimate Std. Error       z Pr(>|z|)   S
#1 -0.07807     0.0296 -2.6399  0.00829 6.9
#2 -0.08503     0.0448 -1.8970  0.05782 4.1
#3 -0.06919     0.0927 -0.7461  0.45558 1.1
#5  0.00363     0.1431  0.0254  0.97975 0.0
#6  0.02250     0.1313  0.1715  0.86387 0.2

#c) the small class effect is larger for some groups than others. More
# specifically, the effect is strongest being in the reduction in probability of
# graduation for white and black students and no significant effect for other
# races. 

#2.6 Presenting results & discussion 
#a) Create a table with model summary comparing all 4 models use robust SE's. 
library(sandwich)
models <- list(
  "LPM bivariate" = lpm1, 
  "LPM controlled" = lpm2, 
  "Logit Bivariate" = logit1, 
  "Logi controlled" = logit2
)

vcov_list <- list(
  "HC3", # Robust (lpm)
  "HC3",
  NULL, # standard SE (logit)
  NULL
)

modelsummary(models, vcov=vcov_list, output ="markdown")
#b) create coefficient plot using model plot 


p <- modelplot(
  list(
    "LPM bivariate" = lpm1,
    "LPM controlled" = lpm2,
    "Logit bivariate" = logit1,
    "Logit controlled" = logit2
  ),
  vcov = robust_list,   
)

p

#c) What does STAR data suggest about effect of small class sizes on high school 
# graduation? How does LPM and Logit compare? Why is experimental evidence more 
# credible than an observational study? 

# The STAR data suggests that small class sizes have a very small effect on high
# school graduation. After using controls the changes were minimal with no 
# statistically strong evidence in neither LPM or logit models. Since the 
# predicted probabilities are not near zero or one, the differences between 
# models are small. Since the STAR data-set is from an experiment it has 
# randomization, this avoids biases, ideally making it more reliable than an 
# observational study. 