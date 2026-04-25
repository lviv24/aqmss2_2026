library(tidyverse)
library(readstata13)
library(modelsummary)

# folders
dir.create("img", showWarnings = FALSE)
dir.create("tab", showWarnings = FALSE)

# load data
df <- read.dta13("corruption.dta") %>%
  filter(!is.na(ti_cpi), !is.na(undp_gdp))

names(df)
# regressions
m1 <- lm(ti_cpi ~ undp_gdp, data = df)

m2 <- lm(ti_cpi ~ log(undp_gdp), data = df)

# regression table
modelsummary(
  list("Model 1" = m1, "Model 2 (controls)" = m2),
  output = "tab/regression_table.tex",
  fmt = 3,
  stars = TRUE
)
# figure
p <- ggplot(df, aes(x = undp_gdp, y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "GDP per capita", y = "CPI")

ggsave("img/myplot.pdf", plot = p, width = 6, height = 4)

file.info("tab/regression_table.tex")
file.info("img/myplot.pdf")
