

# Part 1: In class(Card-Kreuger Minimum Wage) -----------------------------

df<- read.csv("minwage.csv")

library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)


# Data Setup & Exploration  -----------------------------------------------

# a) Create a Create a NJ dummy variable that equals 1 if location is not "PA" 
# and 0 otherwise. Report the number of restaurants in NJ vs. PA using table(). 
# Then compute the average wageBefore and wageAfter separately for NJ and PA 
# restaurants using group by() and summarise(). In a comment, note whether wages
# in NJ increased relative to PA after the policy change.
 
df = df %>% mutate(NJ = ifelse(location != "PA", 1, 0))
table(df$NJ)

# 0   1 
# 67 291 

# There are 291 New Jersey restaurants (coded as 1), and 67 Pennsylvania 
# restaurants (coded as 0).This is because NJ spans 4 sub-regions, while PA is 
# treated as one whole region. 

df %>%
  group_by(NJ) %>%
  summarise(
    mean_wage_before = mean(wageBefore, na.rm = TRUE),
    mean_wage_after = mean(wageAfter, na.rm = TRUE))

# A tibble: 2 × 3
# NJ mean_wage_before mean_wage_after
#<dbl>            <dbl>           <dbl>
#  1     0             4.65            4.61
# 2     1             4.61            5.08

# Before the policy change the avaerage starting wage rates were almost the same
# - both are close to the federal minimum wage of $4.25. After the policy change, 
# NJ minimum wage increases to $5.05 while PA stayed close to the same. 

# b) Manual Difference in Difference estimate for full-time employment
# INSTRUCTIONS.. 
#For NJ: nj_change = mean(fullAfter) - mean(fullBefore)
#For PA: mean(fullAfter) - mean(fullBefore)
#DiD = (NJ after − NJ before) − (PA after − PA before)

means = df %>%
  group_by(NJ) %>%
  summarise(
    before = mean(fullBefore, na.rm = TRUE),
    after = mean(fullAfter, na.rm = TRUE),
    change = after - before)
means

# A tibble: 2 × 4
# NJ      before after change
# <dbl>  <dbl> <dbl>  <dbl>
# 1     0  10.7   8.17 -2.49 
# 2     1   7.97  8.41  0.435

nj_change = means$change[means$NJ == 1]
pa_change = means$change[means$NJ == 0]
did_est = nj_change - pa_change
cat("DiD estimate:", round(did_est, 3), "\n")
# DiD estimate: 2.927 
# The Difference in Difference estimate is the difference in within-group changes, 
# Positive value m=indicates that full-time employment grew more or fell less in 
# NJ than in PA after the minimum wage increase, which contradicts the standard 
# prediction that higher minimum wages reduce employment.

# c) To run regressions, reshape the data to long format (one row per restaurant-period)
# Explain why the long format is needed for the DiD regression.

df_long = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(fullBefore, fullAfter),
    names_to = "period",
    values_to = "full_emp") %>%
  mutate(
    post = ifelse(period == "fullAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))
nrow(df_long)

# [1] 716

nrow(df)
# [1] 358

# The DiD regression requires long format because the interaction x NJ 
# is the DiD estimator: it captures how within-NJ change in employment (post-pre)
# differs from the corresponding within-PA change. 


# 1.2 DiD regression  -----------------------------------------------------

# a) Estimate the DiD regression using fixest: 

library(fixest)
m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)
modelsummary(m_did)

# The coefficient on postxNJ is the DiD estimator and it matches the manual 
# calculation from part 1.1 b. The psot coefficient indicates the pre-post change
# in PA and the NJ coefficient represents the baseline NJ-PA gap, the interaction 
# captures the additional change in NJ. 

# b) Add chain fixed effects to absorb time-invariant differences across fast 
# food chains:

m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)

modelsummary(
  list("DiD" = m_did, "DiD + Chain FE" = m_did_fe),
  stars = TRUE, gof_map = c("nobs", "r.squared"),
  output = "markdown")

# Compare the two models in a single modelsummary() table. Does controlling for 
# chain type change the DiD estimate noticeably? Explain what the chain fixed
# effects are absorbing and why controlling for them may or may not matter here.

# The difference in difference estimate doesn't change when fixed effects are
# added. The chain FE's absorb baseline differences in staffing levels across 
# fast food chains. Since the chain type is somewhat balanced across states, 
# controlling for it has little impact on the DiD coefficient. 

# c) state the parallel trends assumption for this specific example. What
# would we need to observe about NJ and PA employment trends in the pre-period to
# be confident in the DiD estimate? Give one concrete example of something that 
# could violate this assumption (i.e., something that would affect NJ but not PA 
# employment independently of the minimum wage change).

# The parallel trend assumption requires that without the NJ minimum wage increase, 
# employment trends in NJ and PA fast-food restaurants would have been the same 
# from February-November of 1992. This is plausible because both states share a
# similar economic environment and the 2 surveys are close in time, this limits 
# opportunities for diverging trends. A violation would occur if NJ experienced 
# an independent economic shock during this period. 


# 1.3 Wages as a validation check -----------------------------------------

# a) Repeat the DiD analysis using wages as the outcome instead of employment. 
# Reshape the data for wages and estimate the model. 

df_long_wage = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(wageBefore, wageAfter),
    names_to = "period",
    values_to = "wage") %>%
  mutate(
    post = ifelse(period == "wageAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))
m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)

# The interaction coefficient post: NJ is positive and statistically significant: 
# wages rose substantially in NJ relative to PA after the policy change, the 
# magnitude is consistent with the $0.80 minimum wage increase ($5.05 -$4.25).
# This is the sign and magnitude one would expect if the law was actually binding. 

# b) In a comment, explain why the wage result is important for interpreting 
# the employment DiD. If wages had not risen in NJ after the law change, what 
# would that imply about the employment result? Why is it reassuring 
# (or not surprising) that wages did rise in NJ?

# The wage DiD serves as a "first stage" or manipulation check. If wages had not
# risen in NJ after the minimum wage increase, it would be unclear whether the
# the study is truly estimating the effect of minimum wage change at all - the
# law might not have been binding, or firms might have already been paying above
# the new minimum. The fact that wages did rise in NJ gives us confidence that 
# the treatment actually occurred as intended, so the employment DiD can be credibly 
# interpreted as a causal response to the minimum wage increase rather than a surprious 
# or null comparison. 


# PART 2: take home  ------------------------------------------------------
install.packages("did")
library(did)
data("mpdta", package = "did")
data("mpdta")

# a) How many counties are in the dataset? How many unique treatment cohorts are
# there? Explain meaning of staggered treatment adoption, why is it a problem to
# compare treated vs untreated counties? 

length(unique(mpdta$countyreal))
# [1] 500
length(unique(mpdta$first.treat))
# [1] 4
table(mpdta$first.treat)
#    0 2004 2006 2007 
# 1545  100  200  655

# There are 500 counties and there are 4 different treatment cohorts. In this 
# context staggered treatment adoption means that different counties adopt the 
# treatment at different points in time rather than all at once. In this dataset 
# some countries are treated earlier while others are treated later, or never 
# treated. This is problematic when comparing treated vs untreated counties 
# because units that have already been treated become controls for units that are
# treated later. Since the "control" units are already affected by treatment, 
# they are no longer valid comparisons. This can bias the estimated treatment 
# effect and lead to misleading conclusions. 

# b) Plot log teen employment over years, separately for each treatment cohort. 
library(dplyr)
library(ggplot2)
mpdta_avg = mpdta %>%
  mutate(cohort = factor(first.treat,
                         levels = c(0, 2004, 2006, 2007),
                         labels = c("Never treated", "Adopted 2004",
                                    "Adopted 2006", "Adopted 2007"))) %>%
  group_by(year, cohort) %>%
  summarise(mean_lemp = mean(lemp, na.rm = TRUE))

p1 <-ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Log teen employment", color = "Treatment cohort")

ggsave("teenemp_plot.png", plot=  p1)

# The cohorts follow similar trends however the cohort with no treatment has 
# the lowest employment rate. Generally there seems to be no correlation with 
# treatment year and employment levels across all cohorts. The cohort with 
# adopted treatment in 2004 was on a downward trajectory that didn't increase 
# until after two years of being treated. 


# 2.2 Naive TWFE vs. Callaway-Santanna estimator --------------------------

# a) estimate naive TWFE model treating all treated counties as a single group. 

library(fixest)
library(dplyr)
mpdta = mpdta %>%
  mutate(treated_post = as.integer(first.treat > 0 & year >= first.treat))
m_twfe = feols(lemp ~ treated_post | countyreal + year,
               data = mpdta, cluster = ~countyreal)
summary(m_twfe)

#OLS estimation, Dep. Var.: lemp
#Observations: 2,500
#Fixed-effects: countyreal: 500,  year: 5
#Standard-errors: Clustered (countyreal) 
#             Estimate Std. Error  t value Pr(>|t|)    
#treated_post -0.036549   0.013265 -2.75526 0.006079 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#RMSE: 0.124223     Adj. R2: 0.991505
#Within R2: 0.004169

# The coefficient on treated_post is -0.036549 and it is statistically significant. 
# This implies that on average being treated reduces teen employment by ~3.65% 
# relative to untreated counties after controlling for county and year fixed 
# effects. Counties that adopt the policy experience a small but statistically 
# significant decline in employment compared to untreated counties. 
# The two-way fixed effects model pools all treatment cohorts together and 
# assumes that the treatment effect is homogeneous across cohorts and constant 
# over time. It assumes that all counties experience the same treatment effect 
# regardless of when they adopt the policy and how long they have been treated. 
# This ignores potential differences in treatment timing and dynamic effects, 
# and can lead to biased estimates in a staggered adoption setting since already 
# treated counties can serve as controls for later-treated counties. 

# b) Now use the Callaway-Sant´anna (2021) estimator, which estimates group-time average
# treatment effects separately for each cohort and time period, using never-treated
# counties as the control group:

library(did)
cs_out = att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~ lpop,
  data = mpdta,
  control_group = "nevertreated")
# Aggregate to an overall ATT
aggte(cs_out, type = "simple")

# The overall ATT estimate is -0.0418, it is similar to the TWFE estimate but the 
# value is larger. The result is still statistically significant. 

# c) Examine the event-study version of the Callaway-Sant´anna results:
cs_dyn <- aggte(cs_out, type = "dynamic")
csplot <- ggdid(cs_dyn)
ggsave("event_cs_plot.png", plot = csplot)

# The plot shows pre-treatment estimates indistinguishable from zero, meaning 
# not statistically significant since their confidence intervals include zero. 
# This result suggests that there is no evidence of differential trends between 
# treated and control counties before treatment. Since pre-treatment coefficients 
# are close to zero and not statistically significant, this supports the parallel
# trends assumption which is necessary for causal interpretation in DiD. Post- 
# treatment estimates are negative and statistically significant meaning that 
# the treatment reduces employment after adoption. 


# 2.3 Pre-testing the parallel trends assumption  -------------------------
# NOTE: ATT = Avg treatment effect on the treated: avg effect of treatment only 
# for units that actually recieved treatment. 

# a) Re-run the CS estimator with bootstrapped standard errors to obtain valid 
# uniform confidence bands and a joint pre-test:
  cs_out_bt = att_gt(
    yname = "lemp",
    gname = "first.treat",
    idname = "countyreal",
    tname = "year",
    xformla = ~ lpop,
    data = mpdta,
    control_group = "nevertreated",
    bstrap = TRUE,
    cband = TRUE)
summary(cs_out_bt)

# P-value for pre-test of parallel trends assumption:  0.23267. 
# The null-hypothesis is that all pre-treatment effects are zero (no systematic 
# differences in trends between treated and control groups before treatment). 
# Since the p-value is large, we fail to reject it, providing support for the 
# parallel trends assumption, supporting the validity of the difference in difference 
# design. 

# b) Visualize all group-time ATT estimates— both pre- and post-treatment— with:
ggdid(cs_out_bt)
ggsave("mpdta_att_gt.pdf", width = 10, height = 6)

# Pre-treatment ATT estimates are close to zero and statistically indistinguishable
# from zero across all cohorts. 

# c) Pre-testing only checks whether treated and control groups had similar trends
# before treatment, it cannot detect violations that occur after treatment 
# begins. It provides supportive but not definitive evidence fot the validity of 
# research designs. Even if we fail to reject the null hypothesis in the pre-test 
# we cannot be sure that the parallel trend assumption holds in the 
# post-treatment period. 


# 2.4 Comparing control group specifications ------------------------------

# a) Re-estimate the CS model using not-yet-treated counties as the control group:
cs_out_nyt = att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~ lpop,
  data = mpdta,
  control_group = "notyettreated")
aggte(cs_out_nyt, type = "simple")

# ATT    Std. Error     [ 95%  Conf. Int.]  
# -0.0414        0.0118    -0.0645     -0.0182 *

# The overall ATT is -0.0414, the never-treated estimate from 2.2 b is -0.0418. 
# the difference is minimal, they are similar in sign and magnitude. 

# b) Produce and save an event-study plot for this specification:
cs_dyn_nyt = aggte(cs_out_nyt, type = "dynamic")
ggdid(cs_dyn_nyt)
ggsave("mpdta_event_study_nyt.pdf", width = 7, height = 4)

# The pre-treatment estimates remain close to zero and aren't statistically
# significant, similar to the previous estimates using never-treated controls, 
# which continue to support the parallel trends assumption. The post-treatment 
# estimates are negative and statistically significant, showing a decline in 
# employment after treatment. Overall using the not yet treated control group 
# doesn't change the conclusions. 

# c) Trade-off between control groups 

# Using never-treated units as the control group is more credible because 
# these units are never exposed to treatment, avoiding contamination from 
# treatment effects, however this approach may use smaller or less comparable
# control groups. Using not-yer treated units increases the sample size and 
# comparability but requires the assumption that the units are valid controls 
# before they receive treatment, meaning their outcomes are not influenced by 
# future treatment. 

# 2.3 Discussion: why does TWFE fail in staggered settings? 

# a) The TWFE effects estimator can produce misleading results in staggered DiD 
# settings because some units are already treated while others receive treatment
# later. This creates a "forbidden comparison" problem where already treated units 
# are implicitly used as controls for newly treated units. This can lead to 
# biased estimates because the control group would no longer be unaffected by
# treatment. TWFE can produce misleading estimates that skew the true treatment
# effect. TWFE assumes a homogeneous treatment effect and ignores the timing
# difference which is not possible when treatment is staggered. 

# b) The TWFE estimate from 2.2a was -0.0365 while the Callaway estimate using 
# never treated controls from 2.2b was -0.0418. They are similar in size and 
# magnitude but the CS estimate accounts for staggered treatment timing and 
# cohort heterogeneity. Based on the event study pre-trends from 2.2c that show 
# no significant differences before treatment, the CS estimate is more credible 
# because it avoids the forbidden comparison problem and correctly aggregates 
# treatment effects across cohorts and over time. 