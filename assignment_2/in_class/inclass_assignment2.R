library(dplyr)
library(broom)
library(ggplot2)
qog <- read.csv("https://www.qogdata.pol.gu.se/data/qog_std_cs_jan26.csv")

df <- qog %>%
  select(
    country = cname,
    epi = epi_epi,
    women_parl = wdi_wip,
    gov_eff = wbgi_gee,
    green_seats = cpds_lg
  )
df_complete <- na.omit(df)

# Number of remaining countries
nrow(df_complete)

summary(df_complete)

#1.2 exploratory visualization
#Scatter-plot with linear fit
ggplot(df, aes(x = women_parl, y = epi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Women in Parliament (%)", y = "EPI Score")
#There is a positive relationship, countries with more women in parliament 
#tend to have higher EPI scores

#1.3 Bivariate regression 
m1 = lm(epi ~ women_parl, data = df)
tidy(m1)
#The coefficient in women in parliament shows the predicted change in EPI
#score for each additional point of women in parliament. 

p25 <- quantile(df$women_parl, 0.25, na.rm = TRUE)
p75 <- quantile(df$women_parl, 0.75, na.rm = TRUE)

coef(m1)["women_parl"] * (p75 - p25)
#Predicted EPI difference between a country at 75th percentile and 25th 
#percentile of women's representation is ~ 5.6. 

#1.4) Multiple regression 
m2 <- lm(epi ~ women_parl + gov_eff, data = df)

tidy(m2)
tidy(m1) %>% filter(term == "women_parl")
tidy(m2) %>% filter(term == "women_parl")

#b)Comparing coefficients 
#In the multiple regression the coefficient is 0.098, while in the bivariate 
#regression the coefficient is 0.308, suggesting that government effectiveness
#plays a role in the relationship between women's representation and EPI. 
#The positive association was partly due to omitted variable bias since 
#government effectiveness is positively correlated with both women's 
#representation and environmental performance. 

#1.5 Demonstrating Omitted variable Bias (OVB)
#a) Extracting coefficients from OVB formula 
beta1_biva = tidy(m1) %>% filter(term == "women_parl") %>% pull(estimate)
beta1_mult = tidy(m2) %>% filter(term == "women_parl") %>% pull(estimate)
beta2_mult = tidy(m2) %>% filter(term == "gov_eff") %>% pull(estimate)
#b)Auxiliary regression 
aux = lm(gov_eff ~ women_parl, data = df)
delta = tidy(aux) %>% filter(term == "women_parl") %>% pull(estimate)

#c) Verifying OVB formula 
round(beta1_mult + beta2_mult * delta, 4)
#0.3307
round(beta1_biva, 4)
#0.3078
#Values match so this confirms he OVB formula! 

#d) The coefficient on women in parliament changed when we added government 
#effectiveness because there is a positive bias in the correlation between 
#government effectiveness and women in parliament as well as EPI. This bias 
#inflated the bivariate regression estimate. 

install.packages("modelsummary")  
library(modelsummary)
install.packages("sandwich")
library(sandwich)

#1.6) Robust standard errors 
#a) printing multiple regression results with default standard errors (classical)
modelsummary(m2, output = "markdown")
#b) Robust standard errors
modelsummary(m2, vcov = "robust", output = "markdown")
#c) The standard errors do not differ, conclusions do not change. 

#1.7) Presenting results 
#a) Side by side table 
modelsummary(list("Bivariate" = m1, "Multiple" = m2),
             vcov = "robust", output = "markdown")
#b) coefficient plot 
modelplot(list("Bivariate" = m1, "Multiple" = m2),
          vcov = "robust")
#c)
# Save scatter plot
ggsave("scatterplot_women_epi.png", width = 6, height = 4)

# Save coefficient plot
ggsave("coefplot.png", width = 6, height = 4)
