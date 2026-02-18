getwd()
library(dplyr)

#2.1 Data Preparation 
star <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/master/datasets/star/star.csv")

head(star)

#b) Create factor variable for classtype
star$classtype <- factor(star$classtype,
                         levels = c(1,2,3),
                         labels = c("Small", "Regular", "Regular+Aid"))
#c) Create factor variable for race 
star$race <- factor(star$race,
                    levels = c(1,2,3,4,5,6),
                    labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))
#d) Create binary variable small 
star$small <- ifelse(star$classtype == "Small", 1, 0)
#e) Report # of observations & # of non-missing observations for reading & math 
num_obs <- nrow(star)
non_missing_reading <- sum(!is.na(star$g4reading))
non_missing_math <- sum(!is.na(star$g4math))

cat("Number of observations:", num_obs, "\n")
cat("Non-missing g4reading:", non_missing_reading, "\n")
cat("Non-missing g4math:", non_missing_math, "\n")
#2395 for non-missing math and 2353 for non-missing reading

#2.2) Comparing groups
# a) Calculate mean 4th grade reading score by class type
mean_reading_by_class <- star %>%
  group_by(classtype) %>%
  summarise(mean_g4reading = mean (g4reading, na.rm = TRUE))
#The small class type has the highest mean reading score. 

# b) Run a bivariate regression of g4reading on small 
model_reading_bivar <- lm(g4reading ~ small, data = star)
library(broom)
tidy(model_reading_bivar)
# The coefficient on small shows the predicted increase in reading scores for 
# students in small classes compared to others. 

# c) Verify that regression coefficient equals difference in means 
diff_means <- mean_reading_by_class$mean_g4reading[mean_reading_by_class$classtype == "Small"]-
  mean_reading_by_class$mean_g4reading[mean_reading_by_class$classtype == "Regular+Aid"]
diff_means
# The coefficient in bivariate model is 3.10 and the difference 
# in means is 2.67.

# d) Repeat bivariate regression for g4math
mean_math_by_class <- star %>% 
  group_by(classtype) %>%
  summarise(mean_g4math = mean(g4math,na.rm = TRUE))
mean_math_by_class
# The regular class has the highest mean math score but only by one unit, 
# followed by the small class type. 

model_math_bivar <- lm(g4math ~ small, data = star)
tidy(model_math_bivar)
# The coefficient shows the effects of being in a small class size on math scores. 

# 2.3 Adding controls 
# a) Run a multiple regression on g4reading on small, race, and yearssmall
model_reading_multiple <- lm(g4reading ~ small + race + yearssmall, data = star)
tidy(model_reading_multiple)

# b) Compare coefficient on small with a bivariate model. Does it change much?
# Yes there is significant change observed in the bivariate model versus the 
# multiple regression model, going from 3.10 to -4.00. This model includes 
# yearssmall which captures cumulative exposure to small classes and is closely 
# related to treatment.  

# c) Interpret the coefficient on yearssmall, What does it capture? 
# The coefficient on yearssmall is 2.17 meaning that each additional year in a 
# small class is associated with 2.17 additional reading point, there 
# may be an exposure effect of small class treatment on reading performance. 

# 2.4 Interactions 
# a-b) Does the effect of being in a small class differ by race?  Fit the following model:
# lm(g4reading ~ small * race + yearssmall, data = df).
model_interaction <- lm(g4reading ~ small * race + yearssmall, data = star)
tidy(model_interaction)

# c) What is the estimated effect of small class for White students? For Black
# students? - use the coefficients to calculate. 
# The estimated effect of small classes for white students is -5.32 (reference 
# group) and the coefficient for Black students is 1.65 (-5.32 + 6.97). 

# d) Discuss whether the interaction is substantively meaningful. 
# The p-value is not statistically significant (p=0.271), so there is no strong
# evidence that the effect of small sizes differs meaningfully by race. The 
# interaction does not appear to be substantively meaningful. 

install.packages("modelsummary")
library(modelsummary)

# 2.5 Presenting results
# a) Create a table with modelsummary() comparing all your reading score models
# (bivariate, multiple, interaction), using robust standard errors. 

models <- list(
"Bivariate" = model_reading_bivar,
"Multiple" = model_reading_multiple,
"Interaction" = model_interaction 
)

#Saved reading interaction model 
modelsummary(
  models,
  vcov ="HC1", 
  output = "reading_models.html"
)
#HC1 computes robust standard errors that are valid even if variance is unequal 
# across students. 

library(ggplot2)
# b) Create a coefficient plot with modelplot() for the three models 
modelplot(
  models,
  vcov = "HC1"
)
coef_plot <- modelplot(
  models, 
  vcov ="HC1"
)

# c) save both outputs 
ggsave("reading_coefficients.png", plot = coef_plot)
ggsave() 

# The interaction model was saved in part a of this section.

# 2.6) Brief Discussion 
# a) What does the STAR data suggest about the effect of small class sizes on 
# student achievement? 
# b) Why is this evidence more credible than typical observational study of 
# class sizes? 
# c) Are there any limitations or caveats based on what you observed in the
# data? 

# a) The STAR data suggests that small class sizes increase performance levels 
# among students as the data shows that on average performance scores increase 
# in small class sizes. 

# b) This evidence is more credible than a typical observational 
# study of class size because the students are randomly assigned to class types 
# in the experiment. This reduces the risk of selection bias and confounding 
# factors influence on results. 

# c) A limitation is that some interaction terms such as race were not 
# statistically significant, there were also null results for certain races 
# such as Hispanics which may indicate a lack of variety in racial diversity of 
# the sample chosen for the STAR experiment. This would reduce the 
# generalizability of the findings across racial groups

