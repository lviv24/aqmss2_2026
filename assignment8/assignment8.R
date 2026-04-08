install.packages("spdep")
install.packages("spatialreg")   
library(sf)
library(spData)
library(spdep)
library(spatialreg)
library(ggplot2)
data(world)


# 1.1 Setup and OLS baseline ----------------------------------------------
# a) Load world dataset, remove rows with missing gdp or lifeexp, remove antarctica
# How many observations remain? Log-transform gdppercap and store as new column. 

world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)
nrow(world)
# [1] 160

# In a comment, report the number of remaining observations and explain why we 
# logtransform GDP per capita. Recall the discussion of skewed distributions and 
# log transformations from earlier in the course

# There are 160 observations after dropping missing observations and dropping 
# Antarctica. GDP per capita is log-transformed because the raw variable is
# right-skewed, where a handful of wealthy countries have values far above the 
# bulk of the distribution. The log tansformation compresses the upper tail and
# makes the relationship between GDP and life expectancy more linear. 

# b) Fit an OLS regression of life expectancy (lifeExp) on log GDP per capita (log gdp):
ols_fit = lm(lifeExp ~ log_gdp, data = world)
summary(ols_fit)
# Call:lm(formula = lifeExp ~ log_gdp, data = world)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-20.479  -2.347   1.024   3.268   8.115 

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  20.2664     3.0027   6.749 2.67e-10 ***
#  log_gdp       5.5403     0.3254  17.024  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 4.891 on 158 degrees of freedom
#Multiple R-squared:  0.6472,	Adjusted R-squared:  0.6449 
#F-statistic: 289.8 on 1 and 158 DF,  p-value: < 2.2e-16

# The coefficient on log_gdp is psostive and statistically significant (p<0.001)
# It indicates that one - unit increase in log GDP per capita (doubling GDP) is 
# associated with higher life expectancy by approcimately that many years on average. 
# The model explains a substantial share of cross-country variation in life 
# expectancy as shown in the R^2. 

# c) Save OLS residuals and map them:

world$ols_resid = residuals(ols_fit)
ggplot(world) +
  geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, name = "OLS residual") +
  theme_void() +
  labs(title = "OLS residuals: life expectancy ~ log GDP per capita")
# In a comment, describe the geographic pattern. Do you see clusters of positive or negative residuals? Which regions appear to have higher life expectancy than the model
# predicts, and which appear lower?

# The residual map reveals clear geographic clustering. Sub-Saharan Africa shows 
# a concentration of negative residuals- countries with lower life expectancy than 
# the model predicts given their income level, likely due to disease burden. West 
# Europe and East Asia display positive residuals indicating that these regions 
# achieve higher life expectancy than income alone predicts. It is a visual signal
# of spatial autocorrelation. 


# 1.2 Spatial weights matrix ----------------------------------------------

# a) Build queen contiguity neighborhoods and row-standardized weights:
nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)
summary(nb)

#> summary(nb)
#Neighbour list object:
#  Number of regions: 160 
#Number of nonzero links: 564 
#Percentage nonzero weights: 2.203125 
#Average number of links: 3.525 
#16 regions with no links:
#  1, 20, 46, 47, 79, 90, 97, 136, 137, 138, 139, 145, 148, 156,
#162, 176
#21 disjoint connected subgraphs
#Link number distribution:
  
#  0  1  2  3  4  5  6  7  8  9 12 13 
#16 17 27 25 24 22 11  9  5  2  1  1 
#17 least connected regions:
#  4 8 17 18 25 27 78 81 86 111 132 133 134 143 144 150 163 with 1 link
#1 most connected region:
#  140 with 13 links

# In a comment, report how many countries have zero neighbors. Explain why some
# countries have no neighbors in a contiguity-based weights matrix.

# There are 16 countries that have 0 neighbors. Some countries have 0 neighbors 
# because they are islands such as Australia, New Zealand, etc. Queen contiguity 
# requires at least one shared point, islands surrounded by the ocean have none, 
# so they are isolated nodes in the weights graph. 

# b) Test Moran’s I on the OLS residuals:
moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)
# Moran I statistic standard deviate = 6.7376, p-value = 8.054e-12
# alternative hypothesis: greater
# sample estimates:
#  Moran I statistic       Expectation          Variance 
#        0.437486921      -0.006993007       0.004352103 

# Report the Moran’s I statistic and p-value. Is there statistically significant
# spatial autocorrelation in the residuals? What does this imply for OLS – specifically,
# what assumption of OLS is being violated?

# Moran's I statistic is 0.43786921 and the p-value is 8.054e-12. This indicates 
# a statistically significant positive spatial autocorrelation in the OLS residuals
# since the p-value is well below 0.05. Moran's statistic signifies that countries 
# close to each other tend to have similar resiudlas, either both overestimated or 
# underestimated, which violates the OLS assumption of independent errors. Ignoring
# this pattern yields inefficient estimates and invalid standard errors. 


# 1.3 Lagrange Multiplier tests  ------------------------------------------

# Should we use the Spatial Error Model (SEM) or the Spatial Lag Model (SLM)? 
# The Lagrange Multiplier(LM) tests help guide this decision. Run all four tests 
# at once:

lm_tests <- lm.RStests(
  ols_fit,
  listw = listw,
  test = c("RSerr", "RSlag", "adjRSerr", "adjRSlag"),
  zero.policy = TRUE
)
summary(lm_tests)
# Rao's score (a.k.a Lagrange multiplier) diagnostics for spatial dependence
#data:  
#  model: lm(formula = lifeExp ~ log_gdp, data = world)
#test weights: listw
#statistic parameter   p.value    
#RSerr    52.170055         1 5.089e-13 ***
#  RSlag     0.061576         1    0.8040    
#adjRSerr 54.305760         1 1.716e-13 ***
#  adjRSlag  2.197282         1    0.1383    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# a) Report the test statistics and p-values for LMerr and LMlag. Are both significant? In a
#comment, recall from class what each of these tests is checking: LMerr tests for spatial
#dependence in the error term (λ ̸= 0), while LMlag tests for a spatially lagged dependent
#variable (ρ ̸= 0).
# (RSerr is LMerr and RSlag is LMlag), LMerr or RSerr is statistically significant
# , but LMLag or RSlag is not significant. LMerr should be the model used to test 
# the spatial dependence in the error term. 

# b) Report the robust versions RLMerr and RLMlag. The robust tests control for the presence
#of the other type of dependence. Which is more significant? Based on the LM decision
#rule from class (if both LM tests are significant, compare the robust versions), which
#model would you choose: SEM or SLM? Write your reasoning in a comment.

# The robust versions are adjRSERR and adjRSLAG, adjSERR is more significant, the 
# evidence favors the SEM more. 


# 1.4 Spatial Error Model (SEM) -------------------------------------------
# Based on the diagnostics above, fit the Spatial Error Model using errorsarlm() from spdep:

sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)
summary(sem_fit)

# Call: errorsarlm(formula = lifeExp ~ log_gdp, data = world, listw = listw, 
#           zero.policy = TRUE)

#Residuals:
#  Min         1Q     Median         3Q        Max 
#-12.693538  -2.451111   0.052264   2.032955   8.113042 

#Type: error 
#Regions with no neighbours included:
#  1 20 46 47 79 90 97 136 137 138 139 145 148 156 162 176 
#Coefficients: (asymptotic standard errors) 
#Estimate Std. Error z value  Pr(>|z|)
#(Intercept) 36.76680    3.40084  10.811 < 2.2e-16
#log_gdp      3.95785    0.35495  11.150 < 2.2e-16

#Lambda: 0.76254, LR test value: 73.286, p-value: < 2.22e-16
#Asymptotic standard error: 0.044893
#z-value: 16.986, p-value: < 2.22e-16
#Wald statistic: 288.51, p-value: < 2.22e-16

#Log likelihood: -443.351 for error model
#ML residual variance (sigma squared): 12.107, (sigma: 3.4795)
#Number of observations: 160 
#Number of parameters estimated: 4 
#AIC: 894.7, (AIC for lm: 965.99)

# a) Report the estimated coefficient on log gdp from the SEM and compare it to 
# the OLS estimate. Has the coefficient changed? Report the λˆ (lambda) parameter
# and its p-value. Is it statistically significant?

# The estimated coefficient for log gdp from the SEM is 3.95785, and for OLS it 
# is 5.5403, the lambda is 0.76254 and the p-value is 2.22e-16. It is 
# statistically significant, indicating a strong positive spatial autocorrelation 
# in the error term. Confirming that the SEM's appropriate and that unobserved 
# factors affecting life expectancy are spatially correlated across countries. 

# b) explain what λ represents in the SEM. Recall from class:
# the SEM says u = λWu + ε, meaning the error at each unit is partly a function of
# neighbors’ errors. If λ > 0 and significant, what does this tell us about the structure of
# the unmeasured factors driving life expectancy?
#In the SEM, λ governs the spatial autoregressive processes in the disturbances: 
#u = λWu + ε.  A positive and significant λ means that unmeasured factors driving 
#life expectancy are spatially correlated, omitted variables are geographically 
#clustered. The SEM filters the spatial correlation out of the residuals without 
#proposing that life expectancy itself directly diffuses across borders. 

# c) Check whether the SEM has removed the spatial autocorrelation from the residuals.
# Save the SEM residuals and run Moran’s test again:
# In a comment, compare this result to the Moran’s I on the OLS residuals from question
# 1.2b. Has the spatial autocorrelation been removed or substantially reduced?
world$sem_resid = residuals(sem_fit)
moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)

# Moran I test under randomisation
#data:  world$sem_resid  
#weights: listw  
#n reduced by no-neighbour observations  
#Moran I statistic standard deviate = -1.197, p-value = 0.8843
#alternative hypothesis: greater
#sample estimates:
#  Moran I statistic       Expectation          Variance 
#       -0.085981749      -0.006993007       0.004354611 

# In comparison to the Moran test from 2b (0.437486921), the SEM substantially 
# reduces the spatial autocorrelation in the residuals. The test statistic is 
# now much closer to zero and the p-value is no longer significant, indicating 
# that the spatial error correction has absorbed most of the geographic clustering 
# that OLS left behind in its residuals. 


# 1.5 Distance-based weights: an alternative neighborhood -----------------

# a) Compute centroids of every country and build a distance-based neighborhood
coords = st_centroid(st_geometry(world))
nb_dist = dnearneigh(coords, d1 = 0, d2 = 300)
summary(nb_dist)
# There are 114 countries with no neighbors. This number is much higher than before 
# because queen contiguity requires at least one shared point, islands have none, 
# so they are isolated nodes in the weights graph from 1.2a. Now since countries 
# only qualify as neighbors if their centroids are within 300km there are much  
# fewer countries that fit this criteria. Hence the large increase in countries 
# with no neighbors. 

# b) Create row-standardized weights from the distance-based neighborhood and fit a SEM
# using the same formula (lifeExp ~ log gdp):
  listw_dist = nb2listw(nb_dist, style = "W", zero.policy = TRUE)
sem_dist = errorsarlm(lifeExp ~ log_gdp, data = world,
                      listw = listw_dist, zero.policy = TRUE)
summary(sem_dist)

# The lambda is 0.42486 and its p-value is .000015708. In 1.4a the Lambda is 
# 0.76254 and the p-value is 2.22e-16. In comparison to contiguity based SEM, the 
# lambda is smaller indicating weaker spatial dependence under the distance based
# weights. Results are sensitive to how a neighborhood is defined because the log
# gdp coefficient is also somewhat different . 

# c) Run Moran’s I on the residuals of this distance-based SEM (using listw dist). In a
# comment, does this model also succeed in removing spatial autocorrelation from the
# residuals? Compare to your answer in 1.4c.
world$sem_dist_resid = residuals(sem_dist)
moran.test(world$sem_dist_resid, listw = listw_dist, zero.policy = TRUE)

# The moran's I statistic is close to zero and not statistically significant, 
# indicating that the distance based SEM also removes spatial autocorrelation from 
# residuals. This is similar to contiguity based SEM results. The moran 
# statistic is even closer to zero than the moran statistic from 1.4c. 


# Part 2: Take home (Spatial lag model and model comparison  --------------

# 2.1 Spatial Lag Model (SLM)
# a)
slm_fit = lagsarlm(lifeExp ~ log_gdp, data = world,
                   listw = listw, zero.policy = TRUE)
summary(slm_fit)
# Rho: -0.0042561, LR test value: 0.060951, p-value: 0.805
# The estimated rho (ρ) is -0.0043 and has a p value of 0.806, this indicates 
# that ρ is not statistically significant, the coefficient on log_gdp is 5.54820.

# b) Interpret rho (ρ) 
# If rho (ρ) is larger than zero this indicates that the relationship between a 
# The estimated (ρ) captures spatial dependence in the dependent variable meaning 
# that life expectancy in one country is influenced by life expectancy in other 
# countries. However, since the rho (ρ) is close to zero and not statistically 
# significant it indicates that there's no evidence of spatial diffusion in life 
# expectancy across countries. 

# c) In a comment, explain why the coefficient on log gdp in the SLM output is not the
# marginal effect of GDP on life expectancy. 
# In the Spatial lag model the coefficient on log_gdp is not the marginal effect
# because of spatial feedback effects. The model can be rewritten as:
# y = (I - ρW)^(-1)(Xβ + ε), where the matrix (I - ρW)^(-1) captures how changes 
# propagate through the spatial network.
# A change in GDP in one country affects its own life expectancy (direct effect), 
# but also spills over to neighboring countries which can feed back into the 
# original country. This creates a system of interdependence where various 
# effects makeup the network. 
# In this case rho is not significant (minimal spillovers) in general the raw 
# coefficient (beta) doesn't represent the full marginal effect and must instead 
# be decomposed into direct and indirect effects. 


# 2.2 Direct and Indirect Effects -----------------------------------------
# a) Compute the equilibrium direct and indirect effects using the impacts() function,
# passing the SLM fit and the spatial weights. Use R = 500 for simulation-based standard
# errors (and set a seed for reproducibility). In a comment, report the direct effect,
# the indirect effect, and the total effect of log gdp. How does the direct effect compare
# to the raw log gdp coefficient from the SLM output and to the OLS coefficient?

set.seed(123)

impacts_slm = impacts(slm_fit, listw = listw, R = 500)
summary(impacts_slm)

# The direct effect of log_gdp is 5.548223, the indirect effect is -0.02353896 
# and the total effect is 5.524684. The direct effect is nearly identical to the 
# raw log_gdp coefficient from the SLM output (5.55) and very similar to the OLS 
# coefficient. This occurs because the estimated rho is close to zero meaning 
# there are essentially no spatial spillover effects (when rho=0, SLM --> OLS)

# b) In a comment explain the substantive meaning of the indirect effect.
# Recall from class: the indirect effect captures the spillover from unit i’s x to all other
# units’ y, after the spatial feedback loop reaches equilibrium. If log GDP per capita in
# Country A increases by 1 unit, what does the indirect effect say about life expectancy
# in neighboring countries?

# The indirect effect captures how a change in GDP per capita in one country 
# affects life expectancy in other countries through spatial spillovers. The
# indirect effect is close to zero in this case, indicating that an increase in 
# GDP in one country has little to no impact on life expectancy in neighboring 
# countries. 
# This suggests that there is no meaningful cross-country diffusion of life 
# expectancy through the spatial network in this model. 

# c) The total effect is larger than the direct effect. In a comment, explain 
# whether this is an expected feature of the SLM. Under what conditions would the 
# indirect effect be larger or smaller? (Hint: think about what happens to the 
# spillover term as ρ approaches 0 versus as ρ grows larger.)

# The total effect is larger than the direct effect in the SLM because it includes
# both the direct impact within a country and the indirect spillover effects 
# across countries. This is expected when rho is positive as changes go through the 
# spatial network and amplify the overall effect. 
# However, when rho is close to zero as in this case, the indirect effect becomes 
# negligible so the total effect is approximately equal to the direct effect. As 
# rho increases, spillovers become stronger and the indirect effect grows larger 
# as rho approaches zero, spillovers disappear and the indirect effect shrinks. 


# 2.3 Model Comparison  ---------------------------------------------------
# a) Compare OLS, SEM, and SLM using AIC(). Lower AIC indicates better fit, penalized
# for model complexity. In a comment, report the three AIC values. Which model has
# the lowest AIC? Does this agree with your LM-test-based model choice in question
# 1.3b?
AIC(ols_fit, sem_fit, slm_fit)
#       df      AIC
#   ols_fit  3 965.9880
#   sem_fit  4 894.7021
#   slm_fit  4 967.9270
# The SEM has the lowest AIC, indicating the best model fit after penalizing for
# model complexity. This agrees with the LM test results which pointed toward 
# the Spatial Error Model as the appropriate specification. 

# b) (1) whether spatial autocorrelation was present in the OLS residuals and 
# how strong it was; 
# The OLS residuals displayed strong and statistically significant spatial 
# autocorrelation, ans indicated by a moran score of 0.44 with a near zero p-value. 
# This siggests that nearby countries tend to have similar model errors which 
# violates the OLS assumption of independent errors. 

#(2) which spatial model you selected based on the LM
# tests and why; 
# Based on the lagrange multiplier test the Spatial error model (SEM) was selected 
# because the Lmerr and robust lmerr rests were highly significant while the lmag 
# tests weren't. This indicates that spatial dependence operates through error
# term rather than through direct spillovers in the dependent variable. 

#(3) how the key coefficient estimate on log gdp differs across 
# OLS, SEM, and SLM; 
# The estimated coefficient on log_gdp differs across models, it is about 5.54 in
# OLS, decreases to 3.96 in SEM and remains similar to OLS (5.55) in SLM. The 
# reduction in SEM suggests that OLS was partly capturing spatially correlated 
# omitted variables. 

#(4) what the SLM implies about life expectancy spillovers 
# across borders; 
# There is no evidence of life expectancy spillover effects because the SLM 
# results show that the spatial lag parameter (rho) is not statistically 
# significant. 

#(5) one limitation of using queen contiguity weights for 
# country-level data (think about what the matrix misses).
# A limitation of queen contiguity is that it excludes island nations and treats 
# all shared borders equally, regardless of distance/intensity of interaction. 
# This creates potential missed important forms of spatial connection. 


# 2.4 Spatial Durbin Model  -----------------------------------------------

sdm_fit = lagsarlm(lifeExp ~ log_gdp, data = world,
                   listw = listw, Durbin = TRUE, zero.policy = TRUE)
summary(sdm_fit)
# Call:lagsarlm(formula = lifeExp ~ log_gdp, data = world, listw = listw, 
#Durbin = TRUE, zero.policy = TRUE)

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-17.32228  -2.12897   0.83353   2.63920   7.57252 

#Type: mixed 
#Regions with no neighbours included:
#  1 18 39 40 71 82 88 125 126 127 128 133 136 143 147 159 
#Coefficients: (asymptotic standard errors) 
#Estimate Std. Error z value  Pr(>|z|)
#(Intercept) 17.11312    3.07966  5.5568 2.747e-08
#log_gdp      5.98747    0.32355 18.5054 < 2.2e-16
#lag.log_gdp -3.82746    0.54995 -6.9597 3.410e-12

#Rho: 0.48091, LR test value: 28.767, p-value: 8.1634e-08
#Asymptotic standard error: 0.071225
#z-value: 6.752, p-value: 1.4585e-11
#Wald statistic: 45.589, p-value: 1.4585e-11

#Log likelihood: -464.9167 for mixed model
#ML residual variance (sigma squared): 18.283, (sigma: 4.2758)
#Number of observations: 160 
#Number of parameters estimated: 5 
#AIC: 939.83, (AIC for lm: 966.6)
#LM test for residual autocorrelation
#test value: 27.91, p-value: 1.2711e-07

# The coefficient on lag is -3.8725 with a p-value of 3.41e-12 (statistically significant).
# This implies that a country's life expectancy is influence by its own GDP per capita
# as well as surrounding countries GDP per capita. The negative value means that a higher 
# GDP in neighboring countries is associated with lower life expectancy in a certain country 
# holding its own GDP constant. 

# b) Compare the AIC of the SDM to those of the SEM and SLM from question 2.3a. In a
# comment, is the added complexity of the SDM (one extra parameter) justified by the
# improvement in fit? Use the AIC values to support your answer
# The AIC values are:
# OLS: 965.99
# SEM: 894.70
# SLM: 967.93
# SDM: 939.83

# While the SDM improves substantially over OLS and SLM, it still has a higher 
# AIC than the SEM. This indicates that the SEM provides the best balance of model 
# fit. The added complexity of the SDM is not justified, as it does not 
# outperform the simpler SEM specification.