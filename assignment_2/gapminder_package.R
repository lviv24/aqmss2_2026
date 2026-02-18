install.packages("gapminder")
library(gapminder)
write.csv(gapminder, "data/gapminder.csv", row.names = FALSE)